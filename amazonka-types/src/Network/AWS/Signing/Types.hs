{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Signing.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Signing.Types where

import           Control.Applicative
import           Control.Lens
import           Data.Default
import           Data.Time
import           Network.AWS.Types
import qualified Network.HTTP.Client as Client
import           System.Locale

data family Meta v :: *

data Signed a v = Signed
    { _sgMeta    :: Meta v
    , _sgRequest :: Client.Request
    }

sgMeta :: Functor f => LensLike' f (Signed a v) (Meta v)
sgMeta f x = (\y -> x { _sgMeta = y }) <$> f (_sgMeta x)

sgRequest :: Functor f => LensLike' f (Signed a v) Client.Request
sgRequest f x = (\y -> x { _sgRequest = y }) <$> f (_sgRequest x)

class AWSSigner v where
    signed :: v ~ Sg (Sv a)
           => Service (Sv a)
           -> AuthState
           -> Region
           -> Request a
           -> TimeLocale
           -> UTCTime
           -> Signed a v

class AWSPresigner v where
    presigned :: v ~ Sg (Sv a)
              => Service (Sv a)
              -> AuthState
              -> Region
              -> Request a
              -> TimeLocale
              -> UTCTime
              -> Int
              -> Signed a v

sign :: (v ~ Sg (Sv a), AWSRequest a, AWSSigner v)
     => AuthState
     -> Region
     -> a
     -> UTCTime
     -> Signed a v
sign as r rq t = signed service as r (request rq) defaultTimeLocale t

clientRequest :: Client.Request
clientRequest = def
    { Client.secure      = True
    , Client.port        = 443
    , Client.checkStatus = \_ _ _ -> Nothing
    }
