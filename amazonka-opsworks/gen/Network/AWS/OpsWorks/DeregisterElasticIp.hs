{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.DeregisterElasticIp
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
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DeregisterElasticIp.html>
module Network.AWS.OpsWorks.DeregisterElasticIp
    (
    -- * Request
      DeregisterElasticIp
    -- ** Request constructor
    , deregisterElasticIp
    -- ** Request lenses
    , dei1ElasticIp

    -- * Response
    , DeregisterElasticIpResponse
    -- ** Response constructor
    , deregisterElasticIpResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

newtype DeregisterElasticIp = DeregisterElasticIp
    { _dei1ElasticIp :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeregisterElasticIp' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dei1ElasticIp' @::@ 'Text'
--
deregisterElasticIp :: Text -- ^ 'dei1ElasticIp'
                    -> DeregisterElasticIp
deregisterElasticIp p1 = DeregisterElasticIp
    { _dei1ElasticIp = p1
    }

-- | The Elastic IP address.
dei1ElasticIp :: Lens' DeregisterElasticIp Text
dei1ElasticIp = lens _dei1ElasticIp (\s a -> s { _dei1ElasticIp = a })

data DeregisterElasticIpResponse = DeregisterElasticIpResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeregisterElasticIpResponse' constructor.
deregisterElasticIpResponse :: DeregisterElasticIpResponse
deregisterElasticIpResponse = DeregisterElasticIpResponse

instance ToPath DeregisterElasticIp where
    toPath = const "/"

instance ToQuery DeregisterElasticIp where
    toQuery = const mempty

instance ToHeaders DeregisterElasticIp

instance ToJSON DeregisterElasticIp where
    toJSON DeregisterElasticIp{..} = object
        [ "ElasticIp" .= _dei1ElasticIp
        ]

instance AWSRequest DeregisterElasticIp where
    type Sv DeregisterElasticIp = OpsWorks
    type Rs DeregisterElasticIp = DeregisterElasticIpResponse

    request  = post "DeregisterElasticIp"
    response = nullResponse DeregisterElasticIpResponse
