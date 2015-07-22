{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.UpdateLayer
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Updates a specified layer.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_UpdateLayer.html>
module Network.AWS.OpsWorks.UpdateLayer
    (
    -- * Request
      UpdateLayer
    -- ** Request constructor
    , updateLayer
    -- ** Request lenses
    , ulrqCustomInstanceProfileARN
    , ulrqInstallUpdatesOnBoot
    , ulrqCustomSecurityGroupIds
    , ulrqLifecycleEventConfiguration
    , ulrqShortname
    , ulrqCustomRecipes
    , ulrqVolumeConfigurations
    , ulrqEnableAutoHealing
    , ulrqPackages
    , ulrqName
    , ulrqAttributes
    , ulrqAutoAssignPublicIPs
    , ulrqUseEBSOptimizedInstances
    , ulrqAutoAssignElasticIPs
    , ulrqLayerId

    -- * Response
    , UpdateLayerResponse
    -- ** Response constructor
    , updateLayerResponse
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateLayer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ulrqCustomInstanceProfileARN'
--
-- * 'ulrqInstallUpdatesOnBoot'
--
-- * 'ulrqCustomSecurityGroupIds'
--
-- * 'ulrqLifecycleEventConfiguration'
--
-- * 'ulrqShortname'
--
-- * 'ulrqCustomRecipes'
--
-- * 'ulrqVolumeConfigurations'
--
-- * 'ulrqEnableAutoHealing'
--
-- * 'ulrqPackages'
--
-- * 'ulrqName'
--
-- * 'ulrqAttributes'
--
-- * 'ulrqAutoAssignPublicIPs'
--
-- * 'ulrqUseEBSOptimizedInstances'
--
-- * 'ulrqAutoAssignElasticIPs'
--
-- * 'ulrqLayerId'
data UpdateLayer = UpdateLayer'
    { _ulrqCustomInstanceProfileARN    :: !(Maybe Text)
    , _ulrqInstallUpdatesOnBoot        :: !(Maybe Bool)
    , _ulrqCustomSecurityGroupIds      :: !(Maybe [Text])
    , _ulrqLifecycleEventConfiguration :: !(Maybe LifecycleEventConfiguration)
    , _ulrqShortname                   :: !(Maybe Text)
    , _ulrqCustomRecipes               :: !(Maybe Recipes)
    , _ulrqVolumeConfigurations        :: !(Maybe [VolumeConfiguration])
    , _ulrqEnableAutoHealing           :: !(Maybe Bool)
    , _ulrqPackages                    :: !(Maybe [Text])
    , _ulrqName                        :: !(Maybe Text)
    , _ulrqAttributes                  :: !(Maybe (Map LayerAttributesKeys Text))
    , _ulrqAutoAssignPublicIPs         :: !(Maybe Bool)
    , _ulrqUseEBSOptimizedInstances    :: !(Maybe Bool)
    , _ulrqAutoAssignElasticIPs        :: !(Maybe Bool)
    , _ulrqLayerId                     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateLayer' smart constructor.
updateLayer :: Text -> UpdateLayer
updateLayer pLayerId =
    UpdateLayer'
    { _ulrqCustomInstanceProfileARN = Nothing
    , _ulrqInstallUpdatesOnBoot = Nothing
    , _ulrqCustomSecurityGroupIds = Nothing
    , _ulrqLifecycleEventConfiguration = Nothing
    , _ulrqShortname = Nothing
    , _ulrqCustomRecipes = Nothing
    , _ulrqVolumeConfigurations = Nothing
    , _ulrqEnableAutoHealing = Nothing
    , _ulrqPackages = Nothing
    , _ulrqName = Nothing
    , _ulrqAttributes = Nothing
    , _ulrqAutoAssignPublicIPs = Nothing
    , _ulrqUseEBSOptimizedInstances = Nothing
    , _ulrqAutoAssignElasticIPs = Nothing
    , _ulrqLayerId = pLayerId
    }

-- | The ARN of an IAM profile to be used for all of the layer\'s EC2
-- instances. For more information about IAM ARNs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
ulrqCustomInstanceProfileARN :: Lens' UpdateLayer (Maybe Text)
ulrqCustomInstanceProfileARN = lens _ulrqCustomInstanceProfileARN (\ s a -> s{_ulrqCustomInstanceProfileARN = a});

-- | Whether to install operating system and package updates when the
-- instance boots. The default value is @true@. To control when updates are
-- installed, set this value to @false@. You must then update your
-- instances manually by using CreateDeployment to run the
-- @update_dependencies@ stack command or manually running @yum@ (Amazon
-- Linux) or @apt-get@ (Ubuntu) on the instances.
--
-- We strongly recommend using the default value of @true@, to ensure that
-- your instances have the latest security updates.
ulrqInstallUpdatesOnBoot :: Lens' UpdateLayer (Maybe Bool)
ulrqInstallUpdatesOnBoot = lens _ulrqInstallUpdatesOnBoot (\ s a -> s{_ulrqInstallUpdatesOnBoot = a});

-- | An array containing the layer\'s custom security group IDs.
ulrqCustomSecurityGroupIds :: Lens' UpdateLayer [Text]
ulrqCustomSecurityGroupIds = lens _ulrqCustomSecurityGroupIds (\ s a -> s{_ulrqCustomSecurityGroupIds = a}) . _Default;

-- |
ulrqLifecycleEventConfiguration :: Lens' UpdateLayer (Maybe LifecycleEventConfiguration)
ulrqLifecycleEventConfiguration = lens _ulrqLifecycleEventConfiguration (\ s a -> s{_ulrqLifecycleEventConfiguration = a});

-- | For custom layers only, use this parameter to specify the layer\'s short
-- name, which is used internally by AWS OpsWorksand by Chef. The short
-- name is also used as the name for the directory where your app files are
-- installed. It can have a maximum of 200 characters and must be in the
-- following format: \/\\A[a-z0-9\\-\\_\\.]+\\Z\/.
--
-- The built-in layers\' short names are defined by AWS OpsWorks. For more
-- information, see the
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/layers.html Layer Reference>
ulrqShortname :: Lens' UpdateLayer (Maybe Text)
ulrqShortname = lens _ulrqShortname (\ s a -> s{_ulrqShortname = a});

-- | A @LayerCustomRecipes@ object that specifies the layer\'s custom
-- recipes.
ulrqCustomRecipes :: Lens' UpdateLayer (Maybe Recipes)
ulrqCustomRecipes = lens _ulrqCustomRecipes (\ s a -> s{_ulrqCustomRecipes = a});

-- | A @VolumeConfigurations@ object that describes the layer\'s Amazon EBS
-- volumes.
ulrqVolumeConfigurations :: Lens' UpdateLayer [VolumeConfiguration]
ulrqVolumeConfigurations = lens _ulrqVolumeConfigurations (\ s a -> s{_ulrqVolumeConfigurations = a}) . _Default;

-- | Whether to disable auto healing for the layer.
ulrqEnableAutoHealing :: Lens' UpdateLayer (Maybe Bool)
ulrqEnableAutoHealing = lens _ulrqEnableAutoHealing (\ s a -> s{_ulrqEnableAutoHealing = a});

-- | An array of @Package@ objects that describe the layer\'s packages.
ulrqPackages :: Lens' UpdateLayer [Text]
ulrqPackages = lens _ulrqPackages (\ s a -> s{_ulrqPackages = a}) . _Default;

-- | The layer name, which is used by the console.
ulrqName :: Lens' UpdateLayer (Maybe Text)
ulrqName = lens _ulrqName (\ s a -> s{_ulrqName = a});

-- | One or more user-defined key\/value pairs to be added to the stack
-- attributes.
ulrqAttributes :: Lens' UpdateLayer (HashMap LayerAttributesKeys Text)
ulrqAttributes = lens _ulrqAttributes (\ s a -> s{_ulrqAttributes = a}) . _Default . _Map;

-- | For stacks that are running in a VPC, whether to automatically assign a
-- public IP address to the layer\'s instances. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
ulrqAutoAssignPublicIPs :: Lens' UpdateLayer (Maybe Bool)
ulrqAutoAssignPublicIPs = lens _ulrqAutoAssignPublicIPs (\ s a -> s{_ulrqAutoAssignPublicIPs = a});

-- | Whether to use Amazon EBS-optimized instances.
ulrqUseEBSOptimizedInstances :: Lens' UpdateLayer (Maybe Bool)
ulrqUseEBSOptimizedInstances = lens _ulrqUseEBSOptimizedInstances (\ s a -> s{_ulrqUseEBSOptimizedInstances = a});

-- | Whether to automatically assign an
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address>
-- to the layer\'s instances. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
ulrqAutoAssignElasticIPs :: Lens' UpdateLayer (Maybe Bool)
ulrqAutoAssignElasticIPs = lens _ulrqAutoAssignElasticIPs (\ s a -> s{_ulrqAutoAssignElasticIPs = a});

-- | The layer ID.
ulrqLayerId :: Lens' UpdateLayer Text
ulrqLayerId = lens _ulrqLayerId (\ s a -> s{_ulrqLayerId = a});

instance AWSRequest UpdateLayer where
        type Sv UpdateLayer = OpsWorks
        type Rs UpdateLayer = UpdateLayerResponse
        request = postJSON
        response = receiveNull UpdateLayerResponse'

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
              ["CustomInstanceProfileArn" .=
                 _ulrqCustomInstanceProfileARN,
               "InstallUpdatesOnBoot" .= _ulrqInstallUpdatesOnBoot,
               "CustomSecurityGroupIds" .=
                 _ulrqCustomSecurityGroupIds,
               "LifecycleEventConfiguration" .=
                 _ulrqLifecycleEventConfiguration,
               "Shortname" .= _ulrqShortname,
               "CustomRecipes" .= _ulrqCustomRecipes,
               "VolumeConfigurations" .= _ulrqVolumeConfigurations,
               "EnableAutoHealing" .= _ulrqEnableAutoHealing,
               "Packages" .= _ulrqPackages, "Name" .= _ulrqName,
               "Attributes" .= _ulrqAttributes,
               "AutoAssignPublicIps" .= _ulrqAutoAssignPublicIPs,
               "UseEbsOptimizedInstances" .=
                 _ulrqUseEBSOptimizedInstances,
               "AutoAssignElasticIps" .= _ulrqAutoAssignElasticIPs,
               "LayerId" .= _ulrqLayerId]

instance ToPath UpdateLayer where
        toPath = const "/"

instance ToQuery UpdateLayer where
        toQuery = const mempty

-- | /See:/ 'updateLayerResponse' smart constructor.
data UpdateLayerResponse =
    UpdateLayerResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateLayerResponse' smart constructor.
updateLayerResponse :: UpdateLayerResponse
updateLayerResponse = UpdateLayerResponse'
