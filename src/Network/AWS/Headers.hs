{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- Module      : Network.AWS.Headers
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Headers where

import           Crypto.Hash.MD5
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8  as BS
import           Data.CaseInsensitive   (CI)
import qualified Data.CaseInsensitive   as Case
import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text.Encoding     as Text
import           Data.Time
import           GHC.TypeLits

default (ByteString)

class IsHeader a where
    encodeHeader :: a -> ByteString -> (CI ByteString, ByteString)

instance IsHeader v => IsHeader (ByteString, v) where
    encodeHeader (k, v) = encodeHeader v . (`mappend` k)

instance IsHeader v => IsHeader (Text, v) where
    encodeHeader (k, v) = encodeHeader (Text.encodeUtf8 k, v)

instance IsHeader ByteString where
    encodeHeader s = (, s) . Case.mk

instance IsHeader Text where
    encodeHeader s = (, Text.encodeUtf8 s) . Case.mk

instance IsHeader String where
    encodeHeader s = (, BS.pack s) . Case.mk

instance IsHeader Integer where
    encodeHeader n = encodeHeader (show n)

data AnyHeader where
    AnyHeader :: IsHeader a => a -> AnyHeader

instance IsHeader AnyHeader where
    encodeHeader (AnyHeader h) = encodeHeader h

instance Show AnyHeader where
    show (AnyHeader h) = show $ encodeHeader h mempty

hdr :: IsHeader a => a -> AnyHeader
hdr = AnyHeader

data Header (k :: Symbol) v = Header v

instance Functor (Header k) where
    fmap f (Header x) = Header $ f x

instance (SingI k, IsHeader v) => IsHeader (Header k v) where
    encodeHeader h@(Header v) = encodeHeader v . mappend (withSing $ f h)
      where
        f :: Header k v -> Sing k -> ByteString
        f _ = BS.pack . fromSing

instance (SingI k, IsHeader v) => Show (Header k v) where
    show = show . (`encodeHeader` mempty)

type ContentLength     = Header "content-length" Integer
type ContentLanguage   = Header "content-language" ByteString
type Expect            = Header "expect" ByteString
type Expires           = Header "expires" ByteString
type Range             = Header "range" ByteString
type IfModifiedSince   = Header "if-modified-since" ByteString
type IfUnmodifiedSince = Header "if-unmodified-since" ByteString
type IfMatch           = Header "if-match" ByteString
type IfNoneMatch       = Header "if-none-match" ByteString

data Content (t :: Symbol) (s :: Symbol) = Content

instance (SingI t, SingI s) => IsHeader (Content t s) where
    encodeHeader c = encodeHeader (BS.pack "content-type", val)
      where
        val = BS.concat [contentType c, "/", contentSubType c]

contentType :: SingI t => Content t s -> ByteString
contentType = withSing . f
  where
    f :: Content t s -> Sing t -> ByteString
    f _ = BS.pack . fromSing

contentSubType :: SingI s => Content t s -> ByteString
contentSubType = withSing . f
  where
    f :: Content t s -> Sing s -> ByteString
    f _ = BS.pack . fromSing

type JSON           = Content "application" "json"
type XML            = Content "application" "xml"
type FormURLEncoded = Content "application" "x-www-form-urlencoded"

class CacheValue a where
    cacheValue :: ByteString -> a -> ByteString

newtype Cache (k :: Symbol) v = Cache v

instance (SingI k, CacheValue v) => IsHeader (Cache k v) where
    encodeHeader c@(Cache v) =
        encodeHeader (BS.pack "cache-control", cacheValue (withSing $ f c) v)
      where
        f :: Cache k v -> Sing k -> ByteString
        f _ = BS.pack . fromSing

type Public          = Cache "public" ()
type Private         = Cache "private" (Maybe Text)
type NoCache         = Cache "no-cache" (Maybe Text)
type NoStore         = Cache "no-store" ()
type NoTransform     = Cache "no-transform" ()
type MustRevalidate  = Cache "must-revalidate" ()
type ProxyRevalidate = Cache "proxy-revalidate" ()
type MaxAge          = Cache "max-age" Integer
type SMaxAge         = Cache "s-maxage" Integer
type MaxStale        = Cache "max-stale" (Maybe Integer)
type MinFresh        = Cache "min-fresh" Integer
type OnlyIfCache     = Cache "only-if-cached" ()

data Encoding (t :: Symbol) = Encoding

instance SingI t => IsHeader (Encoding t) where
    encodeHeader e = encodeHeader (BS.pack "encoding", withSing $ f e)
      where
        f :: Encoding t -> Sing t -> ByteString
        f _ = BS.pack . fromSing

type GZipEncoding    = Encoding "gzip"
type DeflateEncoding = Encoding "deflate"

newtype Param (k :: Symbol) v = Param v

class ParamValue a where
    paramValue :: a -> ByteString

instance ParamValue ByteString where
    paramValue = id

instance (SingI k, ParamValue v) => ParamValue (Param k v) where
    paramValue p@(Param v) = BS.concat [withSing $ f p, "=", paramValue v]
      where
        f :: Param k v -> Sing k -> ByteString
        f _ = BS.pack . fromSing

data AnyParam where
    AnyParam :: ParamValue a => a -> AnyParam

instance ParamValue AnyParam where
    paramValue (AnyParam p) = paramValue p

prm :: ParamValue a => a -> AnyParam
prm = AnyParam

newtype Disposition (t :: Symbol) = Disposition [AnyParam]

instance SingI t => IsHeader (Disposition t) where
    encodeHeader d@(Disposition ps) =
        encodeHeader (BS.pack "content-disposition", val)
      where
        val = BS.intercalate ";" $ withSing (f d) : map paramValue ps

        f :: Disposition t -> Sing t -> ByteString
        f _ = BS.pack . fromSing

-- | Displayed automatically [RFC2183]
type Inline = Disposition "inline"

-- | Attachment user controlled display [RFC2183]
type Attachment = Disposition "attachment"

-- | Process as form response [RFC2388]
type FormData = Disposition "form-data"

-- | Tunneled content to be processed silently [RFC3204]
type Signal = Disposition "signal"

-- | Custom ring tone to alert the user [RFC3261]
type Alert = Disposition "alert"

-- | Displayed as an icon to the user [RFC3261]
type Icon = Disposition "icon"

-- | Should be displayed to the user [RFC3261]
type Render = Disposition "render"

-- | Contains a list of URIs that indicates the recipients
-- of the request [RFC5364]
type RecipientListHistory = Disposition "recipient-list-history"

-- | Describes a communications session.
-- For example, an RFC2327 SDP body [RFC3261]
type Session = Disposition "session"

-- | Authenticated Identity Body [RFC3893]
type AIB = Disposition "aib"

-- | Describes an early communications session.
-- For example, and [RFC2327] SDP body [RFC3959]
type EarlySession = Disposition "early-session"

-- | Includes a list of URIs to which URI-list services
-- are to be applied. [RFC5363]
type RecipientList = Disposition "recipient-list"

-- | Payload of the message carrying this Content-Disposition header
-- field value is an Instant Message Disposition Notification as requested
-- in the corresponding Instant Message. [RFC5438]
type Notification = Disposition "notification"

-- | Needs to be handled according to a reference to the body that is located
-- in the same SIP message as the body. [RFC5621]
type ByReference = Disposition "by-reference"

-- | Contains information associated with an Info Package
type InfoPackage = Disposition "info-package"

-- | Name to be used when creating file [RFC2183]
type FileName = Param "filename" ByteString

-- | When content was created [RFC2183]
type CreationDate = Param "creation-date" UTCTime

-- | When content was last modified [RFC2183]
type ModificationDate = Param "modification-date" UTCTime

-- | When content was last read [RFC2183]
type ReadDate = Param "read-date" UTCTime

-- | Approximate size of content in octets [RFC2183]
type Size = Param "size" Integer

-- | Original field name in form [RFC2388]
type Name = Param "name" ByteString

-- | Whether or not processing is required [RFC3204]
type Handling = Param "handling" HandlingType

-- | Type or use of audio content [RFC2421]
type Voice = Param "voice" VoiceType

data HandlingType
    = Required
    | Optional
      deriving (Eq, Show)

instance ParamValue HandlingType where
    paramValue Required = "required"
    paramValue Optional = "optional"

data VoiceType
    = VoiceMessage
    | VoiceMessageNotification
    | OriginatorSpokenName
    | RecipientSpokenName
    | SpokenSubject
      deriving (Eq, Show)

instance ParamValue VoiceType where
    paramValue vt = case vt of
        VoiceMessage             -> "Voice-Message"
        VoiceMessageNotification -> "Voice-Message-Notification"
        OriginatorSpokenName     -> "Originator-Spoken-Name"
        RecipientSpokenName      -> "Recipient-Spoken-Name"
        SpokenSubject            -> "Spoken-Subject"

newtype MD5 = MD5 ByteString deriving (Eq, Show)

md5 :: ByteString -> MD5
md5 = MD5 . Base64.encode . hash

instance IsHeader MD5 where
    encodeHeader (MD5 bs) = encodeHeader (BS.pack "content-md5", bs)
