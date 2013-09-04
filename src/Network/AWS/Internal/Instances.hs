{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Network.AWS.Internal.Instances
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal.Instances where

import           Control.Monad
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Char8           as BS
import           Data.Time
import           GHC.Generics
import           Network.HTTP.QueryString.Pickle
import           System.Locale                   (defaultTimeLocale)
import           Text.XML.Expat.Pickle.Generic

instance IsQuery a => IsQuery [a] where
    queryPickler = qpOrdinalList queryPickler

newtype Items a = Items { items :: [a] }
    deriving (Eq, Show, Generic)

instance IsQuery a => IsQuery (Items a) where
    queryPickler = qpWrap (Items, items)
        (qpElem "item" $ qpOrdinalList queryPickler)

instance IsXML a => IsXML (Items a) where
    xmlPickler = xpWrap (Items, items) $ xpElemList (name "item") pu
      where
        name = maybe mkAnNName mkNName . join $ nnNamespace `fmap` root pu
        pu   = xmlPickler

newtype Members a = Members { members :: [a] }
    deriving (Eq, Show, Generic)

instance IsQuery a => IsQuery (Members a) where
    queryPickler = qpWrap (Members, members)
        (qpElem "member" $ qpOrdinalList queryPickler)

instance IsXML a => IsXML (Members a) where
    xmlPickler = xpWrap (Members, members) $ xpElemList (name "member") pu
      where
        name = maybe mkAnNName mkNName . join $ nnNamespace `fmap` root pu
        pu   = xmlPickler

instance IsQuery () where
    queryPickler = qpLift ()

instance IsQuery Bool where
    queryPickler = QueryPU p u
      where
        p True  = Value "true"
        p False = Value "false"

        u (Value "true")  = Right True
        u (Value "false") = Right False
        u err             = Left $ "unable to parse Bool from: " ++ show err

instance IsXML Bool where
    xmlPickler = xpContent $ XMLPU p u Nothing
      where
        p True  = "true"
        p False = "false"

        u "true"  = Right True
        u "false" = Right False
        u err     = Left $ "unable to parse Bool from: " ++ show err

instance IsQuery UTCTime where
    queryPickler = QueryPU p u
      where
        p d = Value . BS.pack $ take 23 (formatTime defaultTimeLocale "%FT%T%Q" d) ++ "Z"

        u (Value t) = case parseTime defaultTimeLocale "%FT%T%QZ" (BS.unpack t) of
            Just d -> Right d
            _      -> Left "could not parse ISO-8601 date"
        u _         = Left "could not parse ISO-8601 date"

instance IsXML UTCTime where
    xmlPickler = xpContent $ XMLPU
        { pickleTree   = \d ->
             BS.pack (take 23 (formatTime defaultTimeLocale "%FT%T%Q" d) ++ "Z")
        , unpickleTree = \t ->
              case parseTime defaultTimeLocale "%FT%T%QZ" (BS.unpack t) of
                  Just d -> Right d
                  _      -> Left "could not parse ISO-8601 date"
        , root         = Nothing
        }

instance IsQuery Double where
    queryPickler = qpPrim
