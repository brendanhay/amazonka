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
-- Module      : Network.AWS.OpsWorks.UpdateLayer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a specified layer.
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.UpdateLayer
    (
    -- * Creating a Request
      updateLayer
    , UpdateLayer
    -- * Request Lenses
    , ulCustomInstanceProfileARN
    , ulCustomSecurityGroupIds
    , ulInstallUpdatesOnBoot
    , ulCloudWatchLogsConfiguration
    , ulLifecycleEventConfiguration
    , ulShortname
    , ulCustomRecipes
    , ulCustomJSON
    , ulVolumeConfigurations
    , ulEnableAutoHealing
    , ulPackages
    , ulAttributes
    , ulName
    , ulAutoAssignPublicIPs
    , ulUseEBSOptimizedInstances
    , ulAutoAssignElasticIPs
    , ulLayerId

    -- * Destructuring the Response
    , updateLayerResponse
    , UpdateLayerResponse
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateLayer' smart constructor.
data UpdateLayer = UpdateLayer'
  { _ulCustomInstanceProfileARN :: !(Maybe Text)
  , _ulCustomSecurityGroupIds :: !(Maybe [Text])
  , _ulInstallUpdatesOnBoot :: !(Maybe Bool)
  , _ulCloudWatchLogsConfiguration :: !(Maybe CloudWatchLogsConfiguration)
  , _ulLifecycleEventConfiguration :: !(Maybe LifecycleEventConfiguration)
  , _ulShortname :: !(Maybe Text)
  , _ulCustomRecipes :: !(Maybe Recipes)
  , _ulCustomJSON :: !(Maybe Text)
  , _ulVolumeConfigurations :: !(Maybe [VolumeConfiguration])
  , _ulEnableAutoHealing :: !(Maybe Bool)
  , _ulPackages :: !(Maybe [Text])
  , _ulAttributes :: !(Maybe (Map LayerAttributesKeys (Maybe Text)))
  , _ulName :: !(Maybe Text)
  , _ulAutoAssignPublicIPs :: !(Maybe Bool)
  , _ulUseEBSOptimizedInstances :: !(Maybe Bool)
  , _ulAutoAssignElasticIPs :: !(Maybe Bool)
  , _ulLayerId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateLayer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ulCustomInstanceProfileARN' - The ARN of an IAM profile to be used for all of the layer's EC2 instances. For more information about IAM ARNs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- * 'ulCustomSecurityGroupIds' - An array containing the layer's custom security group IDs.
--
-- * 'ulInstallUpdatesOnBoot' - Whether to install operating system and package updates when the instance boots. The default value is @true@ . To control when updates are installed, set this value to @false@ . You must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
--
-- * 'ulCloudWatchLogsConfiguration' - Specifies CloudWatch Logs configuration options for the layer. For more information, see 'CloudWatchLogsLogStream' .
--
-- * 'ulLifecycleEventConfiguration' -
--
-- * 'ulShortname' - For custom layers only, use this parameter to specify the layer's short name, which is used internally by AWS OpsWorks Stacks and by Chef. The short name is also used as the name for the directory where your app files are installed. It can have a maximum of 200 characters and must be in the following format: /\A[a-z0-9\-\_\.]+\Z/. The built-in layers' short names are defined by AWS OpsWorks Stacks. For more information, see the <http://docs.aws.amazon.com/opsworks/latest/userguide/layers.html Layer Reference>
--
-- * 'ulCustomRecipes' - A @LayerCustomRecipes@ object that specifies the layer's custom recipes.
--
-- * 'ulCustomJSON' - A JSON-formatted string containing custom stack configuration and deployment attributes to be installed on the layer's instances. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-json-override.html Using Custom JSON> .
--
-- * 'ulVolumeConfigurations' - A @VolumeConfigurations@ object that describes the layer's Amazon EBS volumes.
--
-- * 'ulEnableAutoHealing' - Whether to disable auto healing for the layer.
--
-- * 'ulPackages' - An array of @Package@ objects that describe the layer's packages.
--
-- * 'ulAttributes' - One or more user-defined key/value pairs to be added to the stack attributes.
--
-- * 'ulName' - The layer name, which is used by the console.
--
-- * 'ulAutoAssignPublicIPs' - For stacks that are running in a VPC, whether to automatically assign a public IP address to the layer's instances. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
--
-- * 'ulUseEBSOptimizedInstances' - Whether to use Amazon EBS-optimized instances.
--
-- * 'ulAutoAssignElasticIPs' - Whether to automatically assign an <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address> to the layer's instances. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
--
-- * 'ulLayerId' - The layer ID.
updateLayer
    :: Text -- ^ 'ulLayerId'
    -> UpdateLayer
updateLayer pLayerId_ =
  UpdateLayer'
    { _ulCustomInstanceProfileARN = Nothing
    , _ulCustomSecurityGroupIds = Nothing
    , _ulInstallUpdatesOnBoot = Nothing
    , _ulCloudWatchLogsConfiguration = Nothing
    , _ulLifecycleEventConfiguration = Nothing
    , _ulShortname = Nothing
    , _ulCustomRecipes = Nothing
    , _ulCustomJSON = Nothing
    , _ulVolumeConfigurations = Nothing
    , _ulEnableAutoHealing = Nothing
    , _ulPackages = Nothing
    , _ulAttributes = Nothing
    , _ulName = Nothing
    , _ulAutoAssignPublicIPs = Nothing
    , _ulUseEBSOptimizedInstances = Nothing
    , _ulAutoAssignElasticIPs = Nothing
    , _ulLayerId = pLayerId_
    }


-- | The ARN of an IAM profile to be used for all of the layer's EC2 instances. For more information about IAM ARNs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
ulCustomInstanceProfileARN :: Lens' UpdateLayer (Maybe Text)
ulCustomInstanceProfileARN = lens _ulCustomInstanceProfileARN (\ s a -> s{_ulCustomInstanceProfileARN = a})

-- | An array containing the layer's custom security group IDs.
ulCustomSecurityGroupIds :: Lens' UpdateLayer [Text]
ulCustomSecurityGroupIds = lens _ulCustomSecurityGroupIds (\ s a -> s{_ulCustomSecurityGroupIds = a}) . _Default . _Coerce

-- | Whether to install operating system and package updates when the instance boots. The default value is @true@ . To control when updates are installed, set this value to @false@ . You must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
ulInstallUpdatesOnBoot :: Lens' UpdateLayer (Maybe Bool)
ulInstallUpdatesOnBoot = lens _ulInstallUpdatesOnBoot (\ s a -> s{_ulInstallUpdatesOnBoot = a})

-- | Specifies CloudWatch Logs configuration options for the layer. For more information, see 'CloudWatchLogsLogStream' .
ulCloudWatchLogsConfiguration :: Lens' UpdateLayer (Maybe CloudWatchLogsConfiguration)
ulCloudWatchLogsConfiguration = lens _ulCloudWatchLogsConfiguration (\ s a -> s{_ulCloudWatchLogsConfiguration = a})

-- |
ulLifecycleEventConfiguration :: Lens' UpdateLayer (Maybe LifecycleEventConfiguration)
ulLifecycleEventConfiguration = lens _ulLifecycleEventConfiguration (\ s a -> s{_ulLifecycleEventConfiguration = a})

-- | For custom layers only, use this parameter to specify the layer's short name, which is used internally by AWS OpsWorks Stacks and by Chef. The short name is also used as the name for the directory where your app files are installed. It can have a maximum of 200 characters and must be in the following format: /\A[a-z0-9\-\_\.]+\Z/. The built-in layers' short names are defined by AWS OpsWorks Stacks. For more information, see the <http://docs.aws.amazon.com/opsworks/latest/userguide/layers.html Layer Reference>
ulShortname :: Lens' UpdateLayer (Maybe Text)
ulShortname = lens _ulShortname (\ s a -> s{_ulShortname = a})

-- | A @LayerCustomRecipes@ object that specifies the layer's custom recipes.
ulCustomRecipes :: Lens' UpdateLayer (Maybe Recipes)
ulCustomRecipes = lens _ulCustomRecipes (\ s a -> s{_ulCustomRecipes = a})

-- | A JSON-formatted string containing custom stack configuration and deployment attributes to be installed on the layer's instances. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-json-override.html Using Custom JSON> .
ulCustomJSON :: Lens' UpdateLayer (Maybe Text)
ulCustomJSON = lens _ulCustomJSON (\ s a -> s{_ulCustomJSON = a})

-- | A @VolumeConfigurations@ object that describes the layer's Amazon EBS volumes.
ulVolumeConfigurations :: Lens' UpdateLayer [VolumeConfiguration]
ulVolumeConfigurations = lens _ulVolumeConfigurations (\ s a -> s{_ulVolumeConfigurations = a}) . _Default . _Coerce

-- | Whether to disable auto healing for the layer.
ulEnableAutoHealing :: Lens' UpdateLayer (Maybe Bool)
ulEnableAutoHealing = lens _ulEnableAutoHealing (\ s a -> s{_ulEnableAutoHealing = a})

-- | An array of @Package@ objects that describe the layer's packages.
ulPackages :: Lens' UpdateLayer [Text]
ulPackages = lens _ulPackages (\ s a -> s{_ulPackages = a}) . _Default . _Coerce

-- | One or more user-defined key/value pairs to be added to the stack attributes.
ulAttributes :: Lens' UpdateLayer (HashMap LayerAttributesKeys (Maybe Text))
ulAttributes = lens _ulAttributes (\ s a -> s{_ulAttributes = a}) . _Default . _Map

-- | The layer name, which is used by the console.
ulName :: Lens' UpdateLayer (Maybe Text)
ulName = lens _ulName (\ s a -> s{_ulName = a})

-- | For stacks that are running in a VPC, whether to automatically assign a public IP address to the layer's instances. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
ulAutoAssignPublicIPs :: Lens' UpdateLayer (Maybe Bool)
ulAutoAssignPublicIPs = lens _ulAutoAssignPublicIPs (\ s a -> s{_ulAutoAssignPublicIPs = a})

-- | Whether to use Amazon EBS-optimized instances.
ulUseEBSOptimizedInstances :: Lens' UpdateLayer (Maybe Bool)
ulUseEBSOptimizedInstances = lens _ulUseEBSOptimizedInstances (\ s a -> s{_ulUseEBSOptimizedInstances = a})

-- | Whether to automatically assign an <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address> to the layer's instances. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
ulAutoAssignElasticIPs :: Lens' UpdateLayer (Maybe Bool)
ulAutoAssignElasticIPs = lens _ulAutoAssignElasticIPs (\ s a -> s{_ulAutoAssignElasticIPs = a})

-- | The layer ID.
ulLayerId :: Lens' UpdateLayer Text
ulLayerId = lens _ulLayerId (\ s a -> s{_ulLayerId = a})

instance AWSRequest UpdateLayer where
        type Rs UpdateLayer = UpdateLayerResponse
        request = postJSON opsWorks
        response = receiveNull UpdateLayerResponse'

instance Hashable UpdateLayer where

instance NFData UpdateLayer where

instance ToHeaders UpdateLayer where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.UpdateLayer" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateLayer where
        toJSON UpdateLayer'{..}
          = object
              (catMaybes
                 [("CustomInstanceProfileArn" .=) <$>
                    _ulCustomInstanceProfileARN,
                  ("CustomSecurityGroupIds" .=) <$>
                    _ulCustomSecurityGroupIds,
                  ("InstallUpdatesOnBoot" .=) <$>
                    _ulInstallUpdatesOnBoot,
                  ("CloudWatchLogsConfiguration" .=) <$>
                    _ulCloudWatchLogsConfiguration,
                  ("LifecycleEventConfiguration" .=) <$>
                    _ulLifecycleEventConfiguration,
                  ("Shortname" .=) <$> _ulShortname,
                  ("CustomRecipes" .=) <$> _ulCustomRecipes,
                  ("CustomJson" .=) <$> _ulCustomJSON,
                  ("VolumeConfigurations" .=) <$>
                    _ulVolumeConfigurations,
                  ("EnableAutoHealing" .=) <$> _ulEnableAutoHealing,
                  ("Packages" .=) <$> _ulPackages,
                  ("Attributes" .=) <$> _ulAttributes,
                  ("Name" .=) <$> _ulName,
                  ("AutoAssignPublicIps" .=) <$>
                    _ulAutoAssignPublicIPs,
                  ("UseEbsOptimizedInstances" .=) <$>
                    _ulUseEBSOptimizedInstances,
                  ("AutoAssignElasticIps" .=) <$>
                    _ulAutoAssignElasticIPs,
                  Just ("LayerId" .= _ulLayerId)])

instance ToPath UpdateLayer where
        toPath = const "/"

instance ToQuery UpdateLayer where
        toQuery = const mempty

-- | /See:/ 'updateLayerResponse' smart constructor.
data UpdateLayerResponse =
  UpdateLayerResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateLayerResponse' with the minimum fields required to make a request.
--
updateLayerResponse
    :: UpdateLayerResponse
updateLayerResponse = UpdateLayerResponse'


instance NFData UpdateLayerResponse where
