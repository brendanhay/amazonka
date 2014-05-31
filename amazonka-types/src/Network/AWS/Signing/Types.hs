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

import           Data.Default
import           Data.Time
import           Network.AWS.Types
import qualified Network.HTTP.Client as HTTP
import           Network.HTTP.Client hiding (Request)
import           System.Locale

type Signable a = (AWSRequest a, AWSService (Sv a), SigningAlgorithm (Sg (Sv a)))

data family Meta a :: *

data Signed a = Signed
    { sgHost    :: Host
    , sgRequest :: HTTP.Request
    , sgMeta    :: Meta a
    }

class SigningAlgorithm a where
    finalise :: Service b a
             -> Request b
             -> Auth
             -> Region
             -> TimeLocale
             -> UTCTime
             -> Signed a

sign :: Signable a
     => a
     -> Auth
     -> Region
     -> UTCTime
     -> Signed (Sg (Sv a))
sign rq a r = finalise service (request rq) a r defaultTimeLocale

signed :: HTTP.Request
signed = def
    { secure      = True
    , port        = 443
    , checkStatus = \_ _ _ -> Nothing
    }
