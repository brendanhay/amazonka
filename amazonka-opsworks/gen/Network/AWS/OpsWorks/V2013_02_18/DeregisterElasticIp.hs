{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.DeregisterElasticIp
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deregisters a specified Elastic IP address. The address can then be
-- registered by another stack. For more information, see Resource Management.
-- Required Permissions: To use this action, an IAM user must have a Manage
-- permissions level for the stack, or an attached policy that explicitly
-- grants permissions. For more information on user permissions, see Managing
-- User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.DeregisterElasticIp
    (
    -- * Request
      DeregisterElasticIp
    -- ** Request constructor
    , mkDeregisterElasticIp
    -- ** Request lenses
    , deiElasticIp

    -- * Response
    , DeregisterElasticIpResponse
    ) where

import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

newtype DeregisterElasticIp = DeregisterElasticIp
    { _deiElasticIp :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeregisterElasticIp' request.
mkDeregisterElasticIp :: Text -- ^ 'deiElasticIp'
                      -> DeregisterElasticIp
mkDeregisterElasticIp p1 = DeregisterElasticIp
    { _deiElasticIp = p1
    }

-- | The Elastic IP address.
deiElasticIp :: Lens' DeregisterElasticIp Text
deiElasticIp = lens _deiElasticIp (\s a -> s { _deiElasticIp = a })

instance ToPath DeregisterElasticIp

instance ToQuery DeregisterElasticIp

instance ToHeaders DeregisterElasticIp

instance ToJSON DeregisterElasticIp

data DeregisterElasticIpResponse = DeregisterElasticIpResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeregisterElasticIp where
    type Sv DeregisterElasticIp = OpsWorks
    type Rs DeregisterElasticIp = DeregisterElasticIpResponse

    request = get
    response _ = nullaryResponse DeregisterElasticIpResponse
