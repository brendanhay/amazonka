{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Network.AWS.Internal.Request
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal.Request where

import Control.Applicative
import Data.Aeson
import Data.Foldable                      (Foldable, foldl')
import Data.Maybe
import Data.Text                          (Text)
import Network.AWS.Generics.Query
import Network.AWS.Generics.XML
import Network.AWS.Headers
import Network.AWS.Internal.Serialisation
import Network.AWS.Internal.Types
import Network.HTTP.Conduit               hiding (rawBody)
import Network.HTTP.Types

Split these into a module heirarchy, just use verbs as fn names

Is toPath necessary - would just supplying the path as a param be better/worse?

Can the need for empty (non-generic) class instaces be removed?

Should i bake in lens support for all requests, responses, and shit like this module?

Define lenses by hand or? - use lens-family-core unchecked and roll them by hand
for these request setters, use TH for the actual requests


getRestXML :: (ToPath a, ToQuery a, ToHeaders a, AWSRequest a)
           => Service
           -> a
           -> RawRequest
getRestXML s x = setMethod GET
    . setPath x
    . setQuery x
    . setHeaders x
    $ mk s

deleteRestXML :: (ToPath a, ToQuery a, ToHeaders a, AWSRequest a)
           => Service
           -> a
           -> RawRequest
deleteRestXML s x = setMethod DELETE (getRestXML s x)

postRestXML :: (ToPath a, ToQuery a, ToHeaders a, ToXML a, AWSRequest a)
            => Service
            -> a
            -> RawRequest
postRestXML s x = setMethod POST
    . setBody (getRestXML s x)
    . RequestBodyLBS
    $ encodeXML x

postQuery :: (ToQuery a, AWSRequest a)
          => Service
          -> Text
          -> a
          -> RawRequest
postQuery s a x = setMethod POST
   . addQuery "Action" a
   . setQuery x
   $ mk s

postJSON :: (ToJSON a, AWSRequest a)
         => Service
         -> Text
         -> a
         -> RawRequest
postJSON s a = setMethod POST
    . addQuery "Action" a
    . setBody (mk s)
    . RequestBodyLBS
    . encode

s3 :: (ToPath a, ToQuery a, ToHeaders a, AWSRequest a)
   => StdMethod
   -> Service
   -> a
   -> RawRequest
s3 m s x = setMethod m
    . setPath x
    . setQuery x
    . setHeaders x
    $ mk s

s3Body :: (ToPath a, ToQuery a, ToHeaders a, AWSRequest a)
       => StdMethod
       -> Service
       -> RequestBody
       -> a
       -> RawRequest
s3Body m s b x = setBody (s3 m s x) b

s3XML :: (ToPath a, ToQuery a, ToHeaders a, AWSRequest a, ToXML b)
      => StdMethod
      -> Service
      -> b
      -> a
      -> RawRequest
s3XML m s b x = setBody (s3 m s x)
    . RequestBodyLBS
    $ encodeXML b

addHeader :: Header -> RawRequest -> RawRequest
addHeader h rq = rq { rawHeaders = h : rawHeaders rq }

addQuery :: Text -> Text -> RawRequest -> RawRequest
addQuery k v rq = rq { rawQuery = (k, Just v) : rawQuery rq }

setMethod :: StdMethod -> RawRequest -> RawRequest
setMethod m rq = rq { rawMethod = m }

setPath :: ToPath a => a -> RawRequest -> RawRequest
setPath x rq = rq { rawPath = toPath x }

setHeaders :: ToHeaders a => a -> RawRequest -> RawRequest
setHeaders = setMany addHeader
    . mapMaybe (\(k, mv) -> (k,) <$> mv)
    . toHeaders

setQuery :: ToQuery a => a -> RawRequest -> RawRequest
setQuery = setMany (uncurry addQuery) . encodeQuery

setMany :: Foldable t => (b -> a -> a) -> t b -> a -> a
setMany f xs rq = foldl' (\x y -> f y x) rq xs

setBody :: RawRequest -> RequestBody -> RawRequest
setBody rq b = rq { rawBody = b }

mk :: Service -> RawRequest
mk s@Service{..} = RawRequest s GET "/" [] hs (RequestBodyBS "")
  where
    hs = maybe [] (:[]) $ hAMZTarget <$> svcTarget
