{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.CreateDeploymentConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a deployment configuration.
--
--
module Network.AWS.CodeDeploy.CreateDeploymentConfig
    (
    -- * Creating a Request
      createDeploymentConfig
    , CreateDeploymentConfig
    -- * Request Lenses
    , cdcComputePlatform
    , cdcMinimumHealthyHosts
    , cdcTrafficRoutingConfig
    , cdcDeploymentConfigName

    -- * Destructuring the Response
    , createDeploymentConfigResponse
    , CreateDeploymentConfigResponse
    -- * Response Lenses
    , cdcrsDeploymentConfigId
    , cdcrsResponseStatus
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a CreateDeploymentConfig operation.
--
--
--
-- /See:/ 'createDeploymentConfig' smart constructor.
data CreateDeploymentConfig = CreateDeploymentConfig'
  { _cdcComputePlatform      :: !(Maybe ComputePlatform)
  , _cdcMinimumHealthyHosts  :: !(Maybe MinimumHealthyHosts)
  , _cdcTrafficRoutingConfig :: !(Maybe TrafficRoutingConfig)
  , _cdcDeploymentConfigName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDeploymentConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdcComputePlatform' - The destination platform type for the deployment (@Lambda@ or @Server@ >).
--
-- * 'cdcMinimumHealthyHosts' - The minimum number of healthy instances that should be available at any time during the deployment. There are two parameters expected in the input: type and value. The type parameter takes either of the following values:     * HOST_COUNT: The value parameter represents the minimum number of healthy instances as an absolute value.     * FLEET_PERCENT: The value parameter represents the minimum number of healthy instances as a percentage of the total number of instances in the deployment. If you specify FLEET_PERCENT, at the start of the deployment, AWS CodeDeploy converts the percentage to the equivalent number of instance and rounds up fractional instances. The value parameter takes an integer. For example, to set a minimum of 95% healthy instance, specify a type of FLEET_PERCENT and a value of 95.
--
-- * 'cdcTrafficRoutingConfig' - The configuration that specifies how the deployment traffic will be routed.
--
-- * 'cdcDeploymentConfigName' - The name of the deployment configuration to create.
createDeploymentConfig
    :: Text -- ^ 'cdcDeploymentConfigName'
    -> CreateDeploymentConfig
createDeploymentConfig pDeploymentConfigName_ =
  CreateDeploymentConfig'
    { _cdcComputePlatform = Nothing
    , _cdcMinimumHealthyHosts = Nothing
    , _cdcTrafficRoutingConfig = Nothing
    , _cdcDeploymentConfigName = pDeploymentConfigName_
    }


-- | The destination platform type for the deployment (@Lambda@ or @Server@ >).
cdcComputePlatform :: Lens' CreateDeploymentConfig (Maybe ComputePlatform)
cdcComputePlatform = lens _cdcComputePlatform (\ s a -> s{_cdcComputePlatform = a})

-- | The minimum number of healthy instances that should be available at any time during the deployment. There are two parameters expected in the input: type and value. The type parameter takes either of the following values:     * HOST_COUNT: The value parameter represents the minimum number of healthy instances as an absolute value.     * FLEET_PERCENT: The value parameter represents the minimum number of healthy instances as a percentage of the total number of instances in the deployment. If you specify FLEET_PERCENT, at the start of the deployment, AWS CodeDeploy converts the percentage to the equivalent number of instance and rounds up fractional instances. The value parameter takes an integer. For example, to set a minimum of 95% healthy instance, specify a type of FLEET_PERCENT and a value of 95.
cdcMinimumHealthyHosts :: Lens' CreateDeploymentConfig (Maybe MinimumHealthyHosts)
cdcMinimumHealthyHosts = lens _cdcMinimumHealthyHosts (\ s a -> s{_cdcMinimumHealthyHosts = a})

-- | The configuration that specifies how the deployment traffic will be routed.
cdcTrafficRoutingConfig :: Lens' CreateDeploymentConfig (Maybe TrafficRoutingConfig)
cdcTrafficRoutingConfig = lens _cdcTrafficRoutingConfig (\ s a -> s{_cdcTrafficRoutingConfig = a})

-- | The name of the deployment configuration to create.
cdcDeploymentConfigName :: Lens' CreateDeploymentConfig Text
cdcDeploymentConfigName = lens _cdcDeploymentConfigName (\ s a -> s{_cdcDeploymentConfigName = a})

instance AWSRequest CreateDeploymentConfig where
        type Rs CreateDeploymentConfig =
             CreateDeploymentConfigResponse
        request = postJSON codeDeploy
        response
          = receiveJSON
              (\ s h x ->
                 CreateDeploymentConfigResponse' <$>
                   (x .?> "deploymentConfigId") <*> (pure (fromEnum s)))

instance Hashable CreateDeploymentConfig where

instance NFData CreateDeploymentConfig where

instance ToHeaders CreateDeploymentConfig where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.CreateDeploymentConfig" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateDeploymentConfig where
        toJSON CreateDeploymentConfig'{..}
          = object
              (catMaybes
                 [("computePlatform" .=) <$> _cdcComputePlatform,
                  ("minimumHealthyHosts" .=) <$>
                    _cdcMinimumHealthyHosts,
                  ("trafficRoutingConfig" .=) <$>
                    _cdcTrafficRoutingConfig,
                  Just
                    ("deploymentConfigName" .=
                       _cdcDeploymentConfigName)])

instance ToPath CreateDeploymentConfig where
        toPath = const "/"

instance ToQuery CreateDeploymentConfig where
        toQuery = const mempty

-- | Represents the output of a CreateDeploymentConfig operation.
--
--
--
-- /See:/ 'createDeploymentConfigResponse' smart constructor.
data CreateDeploymentConfigResponse = CreateDeploymentConfigResponse'
  { _cdcrsDeploymentConfigId :: !(Maybe Text)
  , _cdcrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDeploymentConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdcrsDeploymentConfigId' - A unique deployment configuration ID.
--
-- * 'cdcrsResponseStatus' - -- | The response status code.
createDeploymentConfigResponse
    :: Int -- ^ 'cdcrsResponseStatus'
    -> CreateDeploymentConfigResponse
createDeploymentConfigResponse pResponseStatus_ =
  CreateDeploymentConfigResponse'
    { _cdcrsDeploymentConfigId = Nothing
    , _cdcrsResponseStatus = pResponseStatus_
    }


-- | A unique deployment configuration ID.
cdcrsDeploymentConfigId :: Lens' CreateDeploymentConfigResponse (Maybe Text)
cdcrsDeploymentConfigId = lens _cdcrsDeploymentConfigId (\ s a -> s{_cdcrsDeploymentConfigId = a})

-- | -- | The response status code.
cdcrsResponseStatus :: Lens' CreateDeploymentConfigResponse Int
cdcrsResponseStatus = lens _cdcrsResponseStatus (\ s a -> s{_cdcrsResponseStatus = a})

instance NFData CreateDeploymentConfigResponse where
