{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.CreateLayer
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a layer. For more information, see
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
    , clrqCustomInstanceProfileARN
    , clrqInstallUpdatesOnBoot
    , clrqCustomSecurityGroupIds
    , clrqLifecycleEventConfiguration
    , clrqCustomRecipes
    , clrqVolumeConfigurations
    , clrqEnableAutoHealing
    , clrqPackages
    , clrqAttributes
    , clrqAutoAssignPublicIPs
    , clrqUseEBSOptimizedInstances
    , clrqAutoAssignElasticIPs
    , clrqStackId
    , clrqType
    , clrqName
    , clrqShortname

    -- * Response
    , CreateLayerResponse
    -- ** Response constructor
    , createLayerResponse
    -- ** Response lenses
    , clrsLayerId
    , clrsStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createLayer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clrqCustomInstanceProfileARN'
--
-- * 'clrqInstallUpdatesOnBoot'
--
-- * 'clrqCustomSecurityGroupIds'
--
-- * 'clrqLifecycleEventConfiguration'
--
-- * 'clrqCustomRecipes'
--
-- * 'clrqVolumeConfigurations'
--
-- * 'clrqEnableAutoHealing'
--
-- * 'clrqPackages'
--
-- * 'clrqAttributes'
--
-- * 'clrqAutoAssignPublicIPs'
--
-- * 'clrqUseEBSOptimizedInstances'
--
-- * 'clrqAutoAssignElasticIPs'
--
-- * 'clrqStackId'
--
-- * 'clrqType'
--
-- * 'clrqName'
--
-- * 'clrqShortname'
data CreateLayer = CreateLayer'
    { _clrqCustomInstanceProfileARN    :: !(Maybe Text)
    , _clrqInstallUpdatesOnBoot        :: !(Maybe Bool)
    , _clrqCustomSecurityGroupIds      :: !(Maybe [Text])
    , _clrqLifecycleEventConfiguration :: !(Maybe LifecycleEventConfiguration)
    , _clrqCustomRecipes               :: !(Maybe Recipes)
    , _clrqVolumeConfigurations        :: !(Maybe [VolumeConfiguration])
    , _clrqEnableAutoHealing           :: !(Maybe Bool)
    , _clrqPackages                    :: !(Maybe [Text])
    , _clrqAttributes                  :: !(Maybe (Map LayerAttributesKeys Text))
    , _clrqAutoAssignPublicIPs         :: !(Maybe Bool)
    , _clrqUseEBSOptimizedInstances    :: !(Maybe Bool)
    , _clrqAutoAssignElasticIPs        :: !(Maybe Bool)
    , _clrqStackId                     :: !Text
    , _clrqType                        :: !LayerType
    , _clrqName                        :: !Text
    , _clrqShortname                   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateLayer' smart constructor.
createLayer :: Text -> LayerType -> Text -> Text -> CreateLayer
createLayer pStackId pType pName pShortname =
    CreateLayer'
    { _clrqCustomInstanceProfileARN = Nothing
    , _clrqInstallUpdatesOnBoot = Nothing
    , _clrqCustomSecurityGroupIds = Nothing
    , _clrqLifecycleEventConfiguration = Nothing
    , _clrqCustomRecipes = Nothing
    , _clrqVolumeConfigurations = Nothing
    , _clrqEnableAutoHealing = Nothing
    , _clrqPackages = Nothing
    , _clrqAttributes = Nothing
    , _clrqAutoAssignPublicIPs = Nothing
    , _clrqUseEBSOptimizedInstances = Nothing
    , _clrqAutoAssignElasticIPs = Nothing
    , _clrqStackId = pStackId
    , _clrqType = pType
    , _clrqName = pName
    , _clrqShortname = pShortname
    }

-- | The ARN of an IAM profile that to be used for the layer\'s EC2
-- instances. For more information about IAM ARNs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
clrqCustomInstanceProfileARN :: Lens' CreateLayer (Maybe Text)
clrqCustomInstanceProfileARN = lens _clrqCustomInstanceProfileARN (\ s a -> s{_clrqCustomInstanceProfileARN = a});

-- | Whether to install operating system and package updates when the
-- instance boots. The default value is @true@. To control when updates are
-- installed, set this value to @false@. You must then update your
-- instances manually by using CreateDeployment to run the
-- @update_dependencies@ stack command or manually running @yum@ (Amazon
-- Linux) or @apt-get@ (Ubuntu) on the instances.
--
-- We strongly recommend using the default value of @true@, to ensure that
-- your instances have the latest security updates.
clrqInstallUpdatesOnBoot :: Lens' CreateLayer (Maybe Bool)
clrqInstallUpdatesOnBoot = lens _clrqInstallUpdatesOnBoot (\ s a -> s{_clrqInstallUpdatesOnBoot = a});

-- | An array containing the layer custom security group IDs.
clrqCustomSecurityGroupIds :: Lens' CreateLayer [Text]
clrqCustomSecurityGroupIds = lens _clrqCustomSecurityGroupIds (\ s a -> s{_clrqCustomSecurityGroupIds = a}) . _Default;

-- | A LifeCycleEventConfiguration object that you can use to configure the
-- Shutdown event to specify an execution timeout and enable or disable
-- Elastic Load Balancer connection draining.
clrqLifecycleEventConfiguration :: Lens' CreateLayer (Maybe LifecycleEventConfiguration)
clrqLifecycleEventConfiguration = lens _clrqLifecycleEventConfiguration (\ s a -> s{_clrqLifecycleEventConfiguration = a});

-- | A @LayerCustomRecipes@ object that specifies the layer custom recipes.
clrqCustomRecipes :: Lens' CreateLayer (Maybe Recipes)
clrqCustomRecipes = lens _clrqCustomRecipes (\ s a -> s{_clrqCustomRecipes = a});

-- | A @VolumeConfigurations@ object that describes the layer\'s Amazon EBS
-- volumes.
clrqVolumeConfigurations :: Lens' CreateLayer [VolumeConfiguration]
clrqVolumeConfigurations = lens _clrqVolumeConfigurations (\ s a -> s{_clrqVolumeConfigurations = a}) . _Default;

-- | Whether to disable auto healing for the layer.
clrqEnableAutoHealing :: Lens' CreateLayer (Maybe Bool)
clrqEnableAutoHealing = lens _clrqEnableAutoHealing (\ s a -> s{_clrqEnableAutoHealing = a});

-- | An array of @Package@ objects that describe the layer packages.
clrqPackages :: Lens' CreateLayer [Text]
clrqPackages = lens _clrqPackages (\ s a -> s{_clrqPackages = a}) . _Default;

-- | One or more user-defined key\/value pairs to be added to the stack
-- attributes.
clrqAttributes :: Lens' CreateLayer (HashMap LayerAttributesKeys Text)
clrqAttributes = lens _clrqAttributes (\ s a -> s{_clrqAttributes = a}) . _Default . _Map;

-- | For stacks that are running in a VPC, whether to automatically assign a
-- public IP address to the layer\'s instances. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
clrqAutoAssignPublicIPs :: Lens' CreateLayer (Maybe Bool)
clrqAutoAssignPublicIPs = lens _clrqAutoAssignPublicIPs (\ s a -> s{_clrqAutoAssignPublicIPs = a});

-- | Whether to use Amazon EBS-optimized instances.
clrqUseEBSOptimizedInstances :: Lens' CreateLayer (Maybe Bool)
clrqUseEBSOptimizedInstances = lens _clrqUseEBSOptimizedInstances (\ s a -> s{_clrqUseEBSOptimizedInstances = a});

-- | Whether to automatically assign an
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address>
-- to the layer\'s instances. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
clrqAutoAssignElasticIPs :: Lens' CreateLayer (Maybe Bool)
clrqAutoAssignElasticIPs = lens _clrqAutoAssignElasticIPs (\ s a -> s{_clrqAutoAssignElasticIPs = a});

-- | The layer stack ID.
clrqStackId :: Lens' CreateLayer Text
clrqStackId = lens _clrqStackId (\ s a -> s{_clrqStackId = a});

-- | The layer type. A stack cannot have more than one built-in layer of the
-- same type. It can have any number of custom layers.
clrqType :: Lens' CreateLayer LayerType
clrqType = lens _clrqType (\ s a -> s{_clrqType = a});

-- | The layer name, which is used by the console.
clrqName :: Lens' CreateLayer Text
clrqName = lens _clrqName (\ s a -> s{_clrqName = a});

-- | For custom layers only, use this parameter to specify the layer\'s short
-- name, which is used internally by AWS OpsWorks and by Chef recipes. The
-- short name is also used as the name for the directory where your app
-- files are installed. It can have a maximum of 200 characters, which are
-- limited to the alphanumeric characters, \'-\', \'_\', and \'.\'.
--
-- The built-in layers\' short names are defined by AWS OpsWorks. For more
-- information, see the
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/layers.html Layer Reference>
clrqShortname :: Lens' CreateLayer Text
clrqShortname = lens _clrqShortname (\ s a -> s{_clrqShortname = a});

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
                 _clrqCustomInstanceProfileARN,
               "InstallUpdatesOnBoot" .= _clrqInstallUpdatesOnBoot,
               "CustomSecurityGroupIds" .=
                 _clrqCustomSecurityGroupIds,
               "LifecycleEventConfiguration" .=
                 _clrqLifecycleEventConfiguration,
               "CustomRecipes" .= _clrqCustomRecipes,
               "VolumeConfigurations" .= _clrqVolumeConfigurations,
               "EnableAutoHealing" .= _clrqEnableAutoHealing,
               "Packages" .= _clrqPackages,
               "Attributes" .= _clrqAttributes,
               "AutoAssignPublicIps" .= _clrqAutoAssignPublicIPs,
               "UseEbsOptimizedInstances" .=
                 _clrqUseEBSOptimizedInstances,
               "AutoAssignElasticIps" .= _clrqAutoAssignElasticIPs,
               "StackId" .= _clrqStackId, "Type" .= _clrqType,
               "Name" .= _clrqName, "Shortname" .= _clrqShortname]

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
-- * 'clrsLayerId'
--
-- * 'clrsStatus'
data CreateLayerResponse = CreateLayerResponse'
    { _clrsLayerId :: !(Maybe Text)
    , _clrsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateLayerResponse' smart constructor.
createLayerResponse :: Int -> CreateLayerResponse
createLayerResponse pStatus =
    CreateLayerResponse'
    { _clrsLayerId = Nothing
    , _clrsStatus = pStatus
    }

-- | The layer ID.
clrsLayerId :: Lens' CreateLayerResponse (Maybe Text)
clrsLayerId = lens _clrsLayerId (\ s a -> s{_clrsLayerId = a});

-- | FIXME: Undocumented member.
clrsStatus :: Lens' CreateLayerResponse Int
clrsStatus = lens _clrsStatus (\ s a -> s{_clrsStatus = a});
