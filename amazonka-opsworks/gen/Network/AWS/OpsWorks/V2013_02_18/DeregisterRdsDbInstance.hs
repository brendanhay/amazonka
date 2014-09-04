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
    , mkDeregisterRdsDbInstanceRequest
    -- ** Request lenses
    , drdirRdsDbInstanceArn

    -- * Response
    , DeregisterRdsDbInstanceResponse
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeregisterRdsDbInstance' request.
mkDeregisterRdsDbInstanceRequest :: Text -- ^ 'drdirRdsDbInstanceArn'
                                 -> DeregisterRdsDbInstance
mkDeregisterRdsDbInstanceRequest p1 = DeregisterRdsDbInstance
    { _drdirRdsDbInstanceArn = p1
    }
{-# INLINE mkDeregisterRdsDbInstanceRequest #-}

newtype DeregisterRdsDbInstance = DeregisterRdsDbInstance
    { _drdirRdsDbInstanceArn :: Text
      -- ^ The Amazon RDS instance's ARN.
    } deriving (Show, Generic)

-- | The Amazon RDS instance's ARN.
drdirRdsDbInstanceArn :: Lens' DeregisterRdsDbInstance (Text)
drdirRdsDbInstanceArn = lens _drdirRdsDbInstanceArn (\s a -> s { _drdirRdsDbInstanceArn = a })
{-# INLINE drdirRdsDbInstanceArn #-}

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
