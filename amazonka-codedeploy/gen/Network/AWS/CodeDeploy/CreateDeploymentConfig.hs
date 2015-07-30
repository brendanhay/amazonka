{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.CreateDeploymentConfig
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a new deployment configuration.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_CreateDeploymentConfig.html>
module Network.AWS.CodeDeploy.CreateDeploymentConfig
    (
    -- * Request
      CreateDeploymentConfig
    -- ** Request constructor
    , createDeploymentConfig
    -- ** Request lenses
    , cdcMinimumHealthyHosts
    , cdcDeploymentConfigName

    -- * Response
    , CreateDeploymentConfigResponse
    -- ** Response constructor
    , createDeploymentConfigResponse
    -- ** Response lenses
    , cdcrsDeploymentConfigId
    , cdcrsStatus
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a create deployment configuration operation.
--
-- /See:/ 'createDeploymentConfig' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdcMinimumHealthyHosts'
--
-- * 'cdcDeploymentConfigName'
data CreateDeploymentConfig = CreateDeploymentConfig'
    { _cdcMinimumHealthyHosts  :: !(Maybe MinimumHealthyHosts)
    , _cdcDeploymentConfigName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDeploymentConfig' smart constructor.
createDeploymentConfig :: Text -> CreateDeploymentConfig
createDeploymentConfig pDeploymentConfigName_ =
    CreateDeploymentConfig'
    { _cdcMinimumHealthyHosts = Nothing
    , _cdcDeploymentConfigName = pDeploymentConfigName_
    }

-- | The minimum number of healthy instances that should be available at any
-- time during the deployment. There are two parameters expected in the
-- input: type and value.
--
-- The type parameter takes either of the following values:
--
-- -   HOST_COUNT: The value parameter represents the minimum number of
--     healthy instances, as an absolute value.
-- -   FLEET_PERCENT: The value parameter represents the minimum number of
--     healthy instances, as a percentage of the total number of instances
--     in the deployment. If you specify FLEET_PERCENT, then at the start
--     of the deployment AWS CodeDeploy converts the percentage to the
--     equivalent number of instances and rounds fractional instances up.
--
-- The value parameter takes an integer.
--
-- For example, to set a minimum of 95% healthy instances, specify a type
-- of FLEET_PERCENT and a value of 95.
cdcMinimumHealthyHosts :: Lens' CreateDeploymentConfig (Maybe MinimumHealthyHosts)
cdcMinimumHealthyHosts = lens _cdcMinimumHealthyHosts (\ s a -> s{_cdcMinimumHealthyHosts = a});

-- | The name of the deployment configuration to create.
cdcDeploymentConfigName :: Lens' CreateDeploymentConfig Text
cdcDeploymentConfigName = lens _cdcDeploymentConfigName (\ s a -> s{_cdcDeploymentConfigName = a});

instance AWSRequest CreateDeploymentConfig where
        type Sv CreateDeploymentConfig = CodeDeploy
        type Rs CreateDeploymentConfig =
             CreateDeploymentConfigResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateDeploymentConfigResponse' <$>
                   (x .?> "deploymentConfigId") <*> (pure (fromEnum s)))

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
              ["minimumHealthyHosts" .= _cdcMinimumHealthyHosts,
               "deploymentConfigName" .= _cdcDeploymentConfigName]

instance ToPath CreateDeploymentConfig where
        toPath = const "/"

instance ToQuery CreateDeploymentConfig where
        toQuery = const mempty

-- | Represents the output of a create deployment configuration operation.
--
-- /See:/ 'createDeploymentConfigResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdcrsDeploymentConfigId'
--
-- * 'cdcrsStatus'
data CreateDeploymentConfigResponse = CreateDeploymentConfigResponse'
    { _cdcrsDeploymentConfigId :: !(Maybe Text)
    , _cdcrsStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDeploymentConfigResponse' smart constructor.
createDeploymentConfigResponse :: Int -> CreateDeploymentConfigResponse
createDeploymentConfigResponse pStatus_ =
    CreateDeploymentConfigResponse'
    { _cdcrsDeploymentConfigId = Nothing
    , _cdcrsStatus = pStatus_
    }

-- | A unique deployment configuration ID.
cdcrsDeploymentConfigId :: Lens' CreateDeploymentConfigResponse (Maybe Text)
cdcrsDeploymentConfigId = lens _cdcrsDeploymentConfigId (\ s a -> s{_cdcrsDeploymentConfigId = a});

-- | FIXME: Undocumented member.
cdcrsStatus :: Lens' CreateDeploymentConfigResponse Int
cdcrsStatus = lens _cdcrsStatus (\ s a -> s{_cdcrsStatus = a});
