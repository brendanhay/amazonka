{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Module      : Network.AWS.Request.RestJSON
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Request.RestJSON
    ( get
    , delete
    , post
    , put
    , stream
    ) where

import           Control.Applicative
import           Control.Lens                 hiding (Action)
import           Data.Aeson
import           Data.Monoid
import           Network.AWS.Data
import           Network.AWS.Request.Internal
import           Network.AWS.Types
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method

get :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request a
get = defaultRequest
{-# INLINE get #-}

delete :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request a
delete x = get x & rqMethod .~ DELETE
{-# INLINE delete #-}

post :: (AWSService (Sv a), ToQuery a, ToPath a, ToHeaders a, ToJSON a)
     => a
     -> Request a
post x = put x & rqMethod .~ POST
{-# INLINE post #-}

put :: forall a. (AWSService (Sv a), ToQuery a, ToPath a, ToHeaders a, ToJSON a)
    => a
    -> Request a
put x = get x
    & rqMethod   .~ PUT
    & rqHeaders <>~ toHeader hContentType ct
    & rqBody     .~ toBody (toJSON x)
  where
    ct = ("application/x-amz-json-" <>) <$> _svcJSONVersion svc

    svc :: Service (Sv a)
    svc = service
{-# INLINE put #-}

stream :: (AWSService (Sv a), ToPath a, ToQuery a, ToHeaders a, ToBody a)
       => StdMethod
       -> a
       -> Request a
stream m x = content $ get x & rqMethod .~ m & rqBody .~ toBody x
{-# INLINE stream #-}

content :: Request a -> Request a
content rq = rq & rqHeaders %~ hdr hAMZContentSHA256 (bodyHash (rq ^. rqBody))
