{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.OpsWorks.CreateLayer
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a layer. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-create.html How to Create a Layer>.
--
-- You should use __CreateLayer__ for noncustom layer types such as PHP App
-- Server only if the stack does not have an existing layer of that type. A
-- stack can have at most one instance of each noncustom layer; if you
-- attempt to create a second instance, __CreateLayer__ fails. A stack can
-- have an arbitrary number of custom layers, so you can call
-- __CreateLayer__ as many times as you like for that layer type.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_CreateLayer.html>
module Network.AWS.OpsWorks.CreateLayer
    (
    -- * Request
      CreateLayer
    -- ** Request constructor
    , createLayer
    -- ** Request lenses
    , clCustomInstanceProfileARN
    , clInstallUpdatesOnBoot
    , clCustomSecurityGroupIds
    , clLifecycleEventConfiguration
    , clCustomRecipes
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

    -- * Response
    , CreateLayerResponse
    -- ** Response constructor
    , createLayerResponse
    -- ** Response lenses
    , clrLayerId
    , clrStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createLayer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clCustomInstanceProfileARN'
--
-- * 'clInstallUpdatesOnBoot'
--
-- * 'clCustomSecurityGroupIds'
--
-- * 'clLifecycleEventConfiguration'
--
-- * 'clCustomRecipes'
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
data CreateLayer = CreateLayer'
    { _clCustomInstanceProfileARN    :: !(Maybe Text)
    , _clInstallUpdatesOnBoot        :: !(Maybe Bool)
    , _clCustomSecurityGroupIds      :: !(Maybe [Text])
    , _clLifecycleEventConfiguration :: !(Maybe LifecycleEventConfiguration)
    , _clCustomRecipes               :: !(Maybe Recipes)
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
    } deriving (Eq,Read,Show)

-- | 'CreateLayer' smart constructor.
createLayer :: Text -> LayerType -> Text -> Text -> CreateLayer
createLayer pStackId pType pName pShortname =
    CreateLayer'
    { _clCustomInstanceProfileARN = Nothing
    , _clInstallUpdatesOnBoot = Nothing
    , _clCustomSecurityGroupIds = Nothing
    , _clLifecycleEventConfiguration = Nothing
    , _clCustomRecipes = Nothing
    , _clVolumeConfigurations = Nothing
    , _clEnableAutoHealing = Nothing
    , _clPackages = Nothing
    , _clAttributes = Nothing
    , _clAutoAssignPublicIPs = Nothing
    , _clUseEBSOptimizedInstances = Nothing
    , _clAutoAssignElasticIPs = Nothing
    , _clStackId = pStackId
    , _clType = pType
    , _clName = pName
    , _clShortname = pShortname
    }

-- | The ARN of an IAM profile that to be used for the layer\'s EC2
-- instances. For more information about IAM ARNs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
clCustomInstanceProfileARN :: Lens' CreateLayer (Maybe Text)
clCustomInstanceProfileARN = lens _clCustomInstanceProfileARN (\ s a -> s{_clCustomInstanceProfileARN = a});

-- | Whether to install operating system and package updates when the
-- instance boots. The default value is @true@. To control when updates are
-- installed, set this value to @false@. You must then update your
-- instances manually by using CreateDeployment to run the
-- @update_dependencies@ stack command or manually running @yum@ (Amazon
-- Linux) or @apt-get@ (Ubuntu) on the instances.
--
-- We strongly recommend using the default value of @true@, to ensure that
-- your instances have the latest security updates.
clInstallUpdatesOnBoot :: Lens' CreateLayer (Maybe Bool)
clInstallUpdatesOnBoot = lens _clInstallUpdatesOnBoot (\ s a -> s{_clInstallUpdatesOnBoot = a});

-- | An array containing the layer custom security group IDs.
clCustomSecurityGroupIds :: Lens' CreateLayer [Text]
clCustomSecurityGroupIds = lens _clCustomSecurityGroupIds (\ s a -> s{_clCustomSecurityGroupIds = a}) . _Default;

-- | A LifeCycleEventConfiguration object that you can use to configure the
-- Shutdown event to specify an execution timeout and enable or disable
-- Elastic Load Balancer connection draining.
clLifecycleEventConfiguration :: Lens' CreateLayer (Maybe LifecycleEventConfiguration)
clLifecycleEventConfiguration = lens _clLifecycleEventConfiguration (\ s a -> s{_clLifecycleEventConfiguration = a});

-- | A @LayerCustomRecipes@ object that specifies the layer custom recipes.
clCustomRecipes :: Lens' CreateLayer (Maybe Recipes)
clCustomRecipes = lens _clCustomRecipes (\ s a -> s{_clCustomRecipes = a});

-- | A @VolumeConfigurations@ object that describes the layer\'s Amazon EBS
-- volumes.
clVolumeConfigurations :: Lens' CreateLayer [VolumeConfiguration]
clVolumeConfigurations = lens _clVolumeConfigurations (\ s a -> s{_clVolumeConfigurations = a}) . _Default;

-- | Whether to disable auto healing for the layer.
clEnableAutoHealing :: Lens' CreateLayer (Maybe Bool)
clEnableAutoHealing = lens _clEnableAutoHealing (\ s a -> s{_clEnableAutoHealing = a});

-- | An array of @Package@ objects that describe the layer packages.
clPackages :: Lens' CreateLayer [Text]
clPackages = lens _clPackages (\ s a -> s{_clPackages = a}) . _Default;

-- | One or more user-defined key\/value pairs to be added to the stack
-- attributes.
clAttributes :: Lens' CreateLayer (HashMap LayerAttributesKeys Text)
clAttributes = lens _clAttributes (\ s a -> s{_clAttributes = a}) . _Default . _Map;

-- | For stacks that are running in a VPC, whether to automatically assign a
-- public IP address to the layer\'s instances. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
clAutoAssignPublicIPs :: Lens' CreateLayer (Maybe Bool)
clAutoAssignPublicIPs = lens _clAutoAssignPublicIPs (\ s a -> s{_clAutoAssignPublicIPs = a});

-- | Whether to use Amazon EBS-optimized instances.
clUseEBSOptimizedInstances :: Lens' CreateLayer (Maybe Bool)
clUseEBSOptimizedInstances = lens _clUseEBSOptimizedInstances (\ s a -> s{_clUseEBSOptimizedInstances = a});

-- | Whether to automatically assign an
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address>
-- to the layer\'s instances. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
clAutoAssignElasticIPs :: Lens' CreateLayer (Maybe Bool)
clAutoAssignElasticIPs = lens _clAutoAssignElasticIPs (\ s a -> s{_clAutoAssignElasticIPs = a});

-- | The layer stack ID.
clStackId :: Lens' CreateLayer Text
clStackId = lens _clStackId (\ s a -> s{_clStackId = a});

-- | The layer type. A stack cannot have more than one built-in layer of the
-- same type. It can have any number of custom layers.
clType :: Lens' CreateLayer LayerType
clType = lens _clType (\ s a -> s{_clType = a});

-- | The layer name, which is used by the console.
clName :: Lens' CreateLayer Text
clName = lens _clName (\ s a -> s{_clName = a});

-- | For custom layers only, use this parameter to specify the layer\'s short
-- name, which is used internally by AWS OpsWorks and by Chef recipes. The
-- short name is also used as the name for the directory where your app
-- files are installed. It can have a maximum of 200 characters, which are
-- limited to the alphanumeric characters, \'-\', \'_\', and \'.\'.
--
-- The built-in layers\' short names are defined by AWS OpsWorks. For more
-- information, see the
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/layers.html Layer Reference>
clShortname :: Lens' CreateLayer Text
clShortname = lens _clShortname (\ s a -> s{_clShortname = a});

instance AWSRequest CreateLayer where
        type Sv CreateLayer = OpsWorks
        type Rs CreateLayer = CreateLayerResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateLayerResponse' <$>
                   (x .?> "LayerId") <*> (pure (fromEnum s)))

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
              ["CustomInstanceProfileArn" .=
                 _clCustomInstanceProfileARN,
               "InstallUpdatesOnBoot" .= _clInstallUpdatesOnBoot,
               "CustomSecurityGroupIds" .=
                 _clCustomSecurityGroupIds,
               "LifecycleEventConfiguration" .=
                 _clLifecycleEventConfiguration,
               "CustomRecipes" .= _clCustomRecipes,
               "VolumeConfigurations" .= _clVolumeConfigurations,
               "EnableAutoHealing" .= _clEnableAutoHealing,
               "Packages" .= _clPackages,
               "Attributes" .= _clAttributes,
               "AutoAssignPublicIps" .= _clAutoAssignPublicIPs,
               "UseEbsOptimizedInstances" .=
                 _clUseEBSOptimizedInstances,
               "AutoAssignElasticIps" .= _clAutoAssignElasticIPs,
               "StackId" .= _clStackId, "Type" .= _clType,
               "Name" .= _clName, "Shortname" .= _clShortname]

instance ToPath CreateLayer where
        toPath = const "/"

instance ToQuery CreateLayer where
        toQuery = const mempty

-- | Contains the response to a @CreateLayer@ request.
--
-- /See:/ 'createLayerResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clrLayerId'
--
-- * 'clrStatus'
data CreateLayerResponse = CreateLayerResponse'
    { _clrLayerId :: !(Maybe Text)
    , _clrStatus  :: !Int
    } deriving (Eq,Read,Show)

-- | 'CreateLayerResponse' smart constructor.
createLayerResponse :: Int -> CreateLayerResponse
createLayerResponse pStatus =
    CreateLayerResponse'
    { _clrLayerId = Nothing
    , _clrStatus = pStatus
    }

-- | The layer ID.
clrLayerId :: Lens' CreateLayerResponse (Maybe Text)
clrLayerId = lens _clrLayerId (\ s a -> s{_clrLayerId = a});

-- | FIXME: Undocumented member.
clrStatus :: Lens' CreateLayerResponse Int
clrStatus = lens _clrStatus (\ s a -> s{_clrStatus = a});
