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
-- Module      : Network.AWS.OpsWorks.CreateLayer
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a layer. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-create.html How to Create a Layer>.
--
-- You should use __CreateLayer__ for noncustom layer types such as PHP App Server only if the stack does not have an existing layer of that type. A stack can have at most one instance of each noncustom layer; if you attempt to create a second instance, __CreateLayer__ fails. A stack can have an arbitrary number of custom layers, so you can call __CreateLayer__ as many times as you like for that layer type.
--
-- __Required Permissions__: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.CreateLayer
    (
    -- * Creating a Request
      createLayer
    , CreateLayer
    -- * Request Lenses
    , clCustomInstanceProfileARN
    , clCustomSecurityGroupIds
    , clInstallUpdatesOnBoot
    , clLifecycleEventConfiguration
    , clCustomRecipes
    , clCustomJSON
    , clVolumeConfigurations
    , clEnableAutoHealing
    , clPackages
    , clAttributes
    , clAutoAssignPublicIPs
    , clUseEBSOptimizedInstances
    , clAutoAssignElasticIPs
    , clStackId
    , clType
    , clName
    , clShortname

    -- * Destructuring the Response
    , createLayerResponse
    , CreateLayerResponse
    -- * Response Lenses
    , clrsLayerId
    , clrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.OpsWorks.Types
import           Network.AWS.OpsWorks.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createLayer' smart constructor.
data CreateLayer = CreateLayer'
    { _clCustomInstanceProfileARN    :: !(Maybe Text)
    , _clCustomSecurityGroupIds      :: !(Maybe [Text])
    , _clInstallUpdatesOnBoot        :: !(Maybe Bool)
    , _clLifecycleEventConfiguration :: !(Maybe LifecycleEventConfiguration)
    , _clCustomRecipes               :: !(Maybe Recipes)
    , _clCustomJSON                  :: !(Maybe Text)
    , _clVolumeConfigurations        :: !(Maybe [VolumeConfiguration])
    , _clEnableAutoHealing           :: !(Maybe Bool)
    , _clPackages                    :: !(Maybe [Text])
    , _clAttributes                  :: !(Maybe (Map LayerAttributesKeys Text))
    , _clAutoAssignPublicIPs         :: !(Maybe Bool)
    , _clUseEBSOptimizedInstances    :: !(Maybe Bool)
    , _clAutoAssignElasticIPs        :: !(Maybe Bool)
    , _clStackId                     :: !Text
    , _clType                        :: !LayerType
    , _clName                        :: !Text
    , _clShortname                   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateLayer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clCustomInstanceProfileARN'
--
-- * 'clCustomSecurityGroupIds'
--
-- * 'clInstallUpdatesOnBoot'
--
-- * 'clLifecycleEventConfiguration'
--
-- * 'clCustomRecipes'
--
-- * 'clCustomJSON'
--
-- * 'clVolumeConfigurations'
--
-- * 'clEnableAutoHealing'
--
-- * 'clPackages'
--
-- * 'clAttributes'
--
-- * 'clAutoAssignPublicIPs'
--
-- * 'clUseEBSOptimizedInstances'
--
-- * 'clAutoAssignElasticIPs'
--
-- * 'clStackId'
--
-- * 'clType'
--
-- * 'clName'
--
-- * 'clShortname'
createLayer
    :: Text -- ^ 'clStackId'
    -> LayerType -- ^ 'clType'
    -> Text -- ^ 'clName'
    -> Text -- ^ 'clShortname'
    -> CreateLayer
createLayer pStackId_ pType_ pName_ pShortname_ =
    CreateLayer'
    { _clCustomInstanceProfileARN = Nothing
    , _clCustomSecurityGroupIds = Nothing
    , _clInstallUpdatesOnBoot = Nothing
    , _clLifecycleEventConfiguration = Nothing
    , _clCustomRecipes = Nothing
    , _clCustomJSON = Nothing
    , _clVolumeConfigurations = Nothing
    , _clEnableAutoHealing = Nothing
    , _clPackages = Nothing
    , _clAttributes = Nothing
    , _clAutoAssignPublicIPs = Nothing
    , _clUseEBSOptimizedInstances = Nothing
    , _clAutoAssignElasticIPs = Nothing
    , _clStackId = pStackId_
    , _clType = pType_
    , _clName = pName_
    , _clShortname = pShortname_
    }

-- | The ARN of an IAM profile to be used for the layer\'s EC2 instances. For more information about IAM ARNs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
clCustomInstanceProfileARN :: Lens' CreateLayer (Maybe Text)
clCustomInstanceProfileARN = lens _clCustomInstanceProfileARN (\ s a -> s{_clCustomInstanceProfileARN = a});

-- | An array containing the layer custom security group IDs.
clCustomSecurityGroupIds :: Lens' CreateLayer [Text]
clCustomSecurityGroupIds = lens _clCustomSecurityGroupIds (\ s a -> s{_clCustomSecurityGroupIds = a}) . _Default . _Coerce;

-- | Whether to install operating system and package updates when the instance boots. The default value is 'true'. To control when updates are installed, set this value to 'false'. You must then update your instances manually by using < CreateDeployment> to run the 'update_dependencies' stack command or by manually running 'yum' (Amazon Linux) or 'apt-get' (Ubuntu) on the instances.
--
-- To ensure that your instances have the latest security updates, we strongly recommend using the default value of 'true'.
clInstallUpdatesOnBoot :: Lens' CreateLayer (Maybe Bool)
clInstallUpdatesOnBoot = lens _clInstallUpdatesOnBoot (\ s a -> s{_clInstallUpdatesOnBoot = a});

-- | A 'LifeCycleEventConfiguration' object that you can use to configure the Shutdown event to specify an execution timeout and enable or disable Elastic Load Balancer connection draining.
clLifecycleEventConfiguration :: Lens' CreateLayer (Maybe LifecycleEventConfiguration)
clLifecycleEventConfiguration = lens _clLifecycleEventConfiguration (\ s a -> s{_clLifecycleEventConfiguration = a});

-- | A 'LayerCustomRecipes' object that specifies the layer custom recipes.
clCustomRecipes :: Lens' CreateLayer (Maybe Recipes)
clCustomRecipes = lens _clCustomRecipes (\ s a -> s{_clCustomRecipes = a});

-- | A JSON-formatted string containing custom stack configuration and deployment attributes to be installed on the layer\'s instances. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-json-override.html Using Custom JSON>. This feature is supported as of version 1.7.42 of the AWS CLI.
clCustomJSON :: Lens' CreateLayer (Maybe Text)
clCustomJSON = lens _clCustomJSON (\ s a -> s{_clCustomJSON = a});

-- | A 'VolumeConfigurations' object that describes the layer\'s Amazon EBS volumes.
clVolumeConfigurations :: Lens' CreateLayer [VolumeConfiguration]
clVolumeConfigurations = lens _clVolumeConfigurations (\ s a -> s{_clVolumeConfigurations = a}) . _Default . _Coerce;

-- | Whether to disable auto healing for the layer.
clEnableAutoHealing :: Lens' CreateLayer (Maybe Bool)
clEnableAutoHealing = lens _clEnableAutoHealing (\ s a -> s{_clEnableAutoHealing = a});

-- | An array of 'Package' objects that describes the layer packages.
clPackages :: Lens' CreateLayer [Text]
clPackages = lens _clPackages (\ s a -> s{_clPackages = a}) . _Default . _Coerce;

-- | One or more user-defined key-value pairs to be added to the stack attributes.
--
-- To create a cluster layer, set the 'EcsClusterArn' attribute to the cluster\'s ARN.
clAttributes :: Lens' CreateLayer (HashMap LayerAttributesKeys Text)
clAttributes = lens _clAttributes (\ s a -> s{_clAttributes = a}) . _Default . _Map;

-- | For stacks that are running in a VPC, whether to automatically assign a public IP address to the layer\'s instances. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
clAutoAssignPublicIPs :: Lens' CreateLayer (Maybe Bool)
clAutoAssignPublicIPs = lens _clAutoAssignPublicIPs (\ s a -> s{_clAutoAssignPublicIPs = a});

-- | Whether to use Amazon EBS-optimized instances.
clUseEBSOptimizedInstances :: Lens' CreateLayer (Maybe Bool)
clUseEBSOptimizedInstances = lens _clUseEBSOptimizedInstances (\ s a -> s{_clUseEBSOptimizedInstances = a});

-- | Whether to automatically assign an <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address> to the layer\'s instances. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
clAutoAssignElasticIPs :: Lens' CreateLayer (Maybe Bool)
clAutoAssignElasticIPs = lens _clAutoAssignElasticIPs (\ s a -> s{_clAutoAssignElasticIPs = a});

-- | The layer stack ID.
clStackId :: Lens' CreateLayer Text
clStackId = lens _clStackId (\ s a -> s{_clStackId = a});

-- | The layer type. A stack cannot have more than one built-in layer of the same type. It can have any number of custom layers. Built-in layers are not available in Chef 12 stacks.
clType :: Lens' CreateLayer LayerType
clType = lens _clType (\ s a -> s{_clType = a});

-- | The layer name, which is used by the console.
clName :: Lens' CreateLayer Text
clName = lens _clName (\ s a -> s{_clName = a});

-- | For custom layers only, use this parameter to specify the layer\'s short name, which is used internally by AWS OpsWorks and by Chef recipes. The short name is also used as the name for the directory where your app files are installed. It can have a maximum of 200 characters, which are limited to the alphanumeric characters, \'-\', \'_\', and \'.\'.
--
-- The built-in layers\' short names are defined by AWS OpsWorks. For more information, see the <http://docs.aws.amazon.com/opsworks/latest/userguide/layers.html Layer Reference>.
clShortname :: Lens' CreateLayer Text
clShortname = lens _clShortname (\ s a -> s{_clShortname = a});

instance AWSRequest CreateLayer where
        type Rs CreateLayer = CreateLayerResponse
        request = postJSON opsWorks
        response
          = receiveJSON
              (\ s h x ->
                 CreateLayerResponse' <$>
                   (x .?> "LayerId") <*> (pure (fromEnum s)))

instance Hashable CreateLayer

instance NFData CreateLayer

instance ToHeaders CreateLayer where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.CreateLayer" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateLayer where
        toJSON CreateLayer'{..}
          = object
              (catMaybes
                 [("CustomInstanceProfileArn" .=) <$>
                    _clCustomInstanceProfileARN,
                  ("CustomSecurityGroupIds" .=) <$>
                    _clCustomSecurityGroupIds,
                  ("InstallUpdatesOnBoot" .=) <$>
                    _clInstallUpdatesOnBoot,
                  ("LifecycleEventConfiguration" .=) <$>
                    _clLifecycleEventConfiguration,
                  ("CustomRecipes" .=) <$> _clCustomRecipes,
                  ("CustomJson" .=) <$> _clCustomJSON,
                  ("VolumeConfigurations" .=) <$>
                    _clVolumeConfigurations,
                  ("EnableAutoHealing" .=) <$> _clEnableAutoHealing,
                  ("Packages" .=) <$> _clPackages,
                  ("Attributes" .=) <$> _clAttributes,
                  ("AutoAssignPublicIps" .=) <$>
                    _clAutoAssignPublicIPs,
                  ("UseEbsOptimizedInstances" .=) <$>
                    _clUseEBSOptimizedInstances,
                  ("AutoAssignElasticIps" .=) <$>
                    _clAutoAssignElasticIPs,
                  Just ("StackId" .= _clStackId),
                  Just ("Type" .= _clType), Just ("Name" .= _clName),
                  Just ("Shortname" .= _clShortname)])

instance ToPath CreateLayer where
        toPath = const "/"

instance ToQuery CreateLayer where
        toQuery = const mempty

-- | Contains the response to a 'CreateLayer' request.
--
-- /See:/ 'createLayerResponse' smart constructor.
data CreateLayerResponse = CreateLayerResponse'
    { _clrsLayerId        :: !(Maybe Text)
    , _clrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateLayerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clrsLayerId'
--
-- * 'clrsResponseStatus'
createLayerResponse
    :: Int -- ^ 'clrsResponseStatus'
    -> CreateLayerResponse
createLayerResponse pResponseStatus_ =
    CreateLayerResponse'
    { _clrsLayerId = Nothing
    , _clrsResponseStatus = pResponseStatus_
    }

-- | The layer ID.
clrsLayerId :: Lens' CreateLayerResponse (Maybe Text)
clrsLayerId = lens _clrsLayerId (\ s a -> s{_clrsLayerId = a});

-- | The response status code.
clrsResponseStatus :: Lens' CreateLayerResponse Int
clrsResponseStatus = lens _clrsResponseStatus (\ s a -> s{_clrsResponseStatus = a});

instance NFData CreateLayerResponse
