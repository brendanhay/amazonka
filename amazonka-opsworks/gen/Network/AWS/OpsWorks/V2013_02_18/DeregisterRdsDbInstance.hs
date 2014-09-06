{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.DeregisterRdsDbInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deregisters an Amazon RDS instance.
module Network.AWS.OpsWorks.V2013_02_18.DeregisterRdsDbInstance
    (
    -- * Request
      DeregisterRdsDbInstance
    -- ** Request constructor
    , mkDeregisterRdsDbInstance
    -- ** Request lenses
    , drdiRdsDbInstanceArn

    -- * Response
    , DeregisterRdsDbInstanceResponse
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

newtype DeregisterRdsDbInstance = DeregisterRdsDbInstance
    { _drdiRdsDbInstanceArn :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeregisterRdsDbInstance' request.
mkDeregisterRdsDbInstance :: Text -- ^ 'drdiRdsDbInstanceArn'
                          -> DeregisterRdsDbInstance
mkDeregisterRdsDbInstance p1 = DeregisterRdsDbInstance
    { _drdiRdsDbInstanceArn = p1
    }
{-# INLINE mkDeregisterRdsDbInstance #-}

-- | The Amazon RDS instance's ARN.
drdiRdsDbInstanceArn :: Lens' DeregisterRdsDbInstance Text
drdiRdsDbInstanceArn =
    lens _drdiRdsDbInstanceArn (\s a -> s { _drdiRdsDbInstanceArn = a })
{-# INLINE drdiRdsDbInstanceArn #-}

instance ToPath DeregisterRdsDbInstance

instance ToQuery DeregisterRdsDbInstance

instance ToHeaders DeregisterRdsDbInstance

instance ToJSON DeregisterRdsDbInstance

data DeregisterRdsDbInstanceResponse = DeregisterRdsDbInstanceResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeregisterRdsDbInstance where
    type Sv DeregisterRdsDbInstance = OpsWorks
    type Rs DeregisterRdsDbInstance = DeregisterRdsDbInstanceResponse

    request = get
    response _ = nullaryResponse DeregisterRdsDbInstanceResponse
