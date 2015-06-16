{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Redshift.CreateHSMConfiguration
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates an HSM configuration that contains the information required by
-- an Amazon Redshift cluster to store and use database encryption keys in
-- a Hardware Security Module (HSM). After creating the HSM configuration,
-- you can specify it as a parameter when creating a cluster. The cluster
-- will then store its encryption keys in the HSM.
--
-- In addition to creating an HSM configuration, you must also create an
-- HSM client certificate. For more information, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-HSM.html Hardware Security Modules>
-- in the Amazon Redshift Cluster Management Guide.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_CreateHSMConfiguration.html>
module Network.AWS.Redshift.CreateHSMConfiguration
    (
    -- * Request
      CreateHSMConfiguration
    -- ** Request constructor
    , createHSMConfiguration
    -- ** Request lenses
    , chcTags
    , chcHSMConfigurationIdentifier
    , chcDescription
    , chcHSMIPAddress
    , chcHSMPartitionName
    , chcHSMPartitionPassword
    , chcHSMServerPublicCertificate

    -- * Response
    , CreateHSMConfigurationResponse
    -- ** Response constructor
    , createHSMConfigurationResponse
    -- ** Response lenses
    , chcrHSMConfiguration
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.Redshift.Types

-- | /See:/ 'createHSMConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chcTags'
--
-- * 'chcHSMConfigurationIdentifier'
--
-- * 'chcDescription'
--
-- * 'chcHSMIPAddress'
--
-- * 'chcHSMPartitionName'
--
-- * 'chcHSMPartitionPassword'
--
-- * 'chcHSMServerPublicCertificate'
data CreateHSMConfiguration = CreateHSMConfiguration'{_chcTags :: Maybe [Tag], _chcHSMConfigurationIdentifier :: Text, _chcDescription :: Text, _chcHSMIPAddress :: Text, _chcHSMPartitionName :: Text, _chcHSMPartitionPassword :: Text, _chcHSMServerPublicCertificate :: Text} deriving (Eq, Read, Show)

-- | 'CreateHSMConfiguration' smart constructor.
createHSMConfiguration :: Text -> Text -> Text -> Text -> Text -> Text -> CreateHSMConfiguration
createHSMConfiguration pHSMConfigurationIdentifier pDescription pHSMIPAddress pHSMPartitionName pHSMPartitionPassword pHSMServerPublicCertificate = CreateHSMConfiguration'{_chcTags = Nothing, _chcHSMConfigurationIdentifier = pHSMConfigurationIdentifier, _chcDescription = pDescription, _chcHSMIPAddress = pHSMIPAddress, _chcHSMPartitionName = pHSMPartitionName, _chcHSMPartitionPassword = pHSMPartitionPassword, _chcHSMServerPublicCertificate = pHSMServerPublicCertificate};

-- | A list of tag instances.
chcTags :: Lens' CreateHSMConfiguration [Tag]
chcTags = lens _chcTags (\ s a -> s{_chcTags = a}) . _Default;

-- | The identifier to be assigned to the new Amazon Redshift HSM
-- configuration.
chcHSMConfigurationIdentifier :: Lens' CreateHSMConfiguration Text
chcHSMConfigurationIdentifier = lens _chcHSMConfigurationIdentifier (\ s a -> s{_chcHSMConfigurationIdentifier = a});

-- | A text description of the HSM configuration to be created.
chcDescription :: Lens' CreateHSMConfiguration Text
chcDescription = lens _chcDescription (\ s a -> s{_chcDescription = a});

-- | The IP address that the Amazon Redshift cluster must use to access the
-- HSM.
chcHSMIPAddress :: Lens' CreateHSMConfiguration Text
chcHSMIPAddress = lens _chcHSMIPAddress (\ s a -> s{_chcHSMIPAddress = a});

-- | The name of the partition in the HSM where the Amazon Redshift clusters
-- will store their database encryption keys.
chcHSMPartitionName :: Lens' CreateHSMConfiguration Text
chcHSMPartitionName = lens _chcHSMPartitionName (\ s a -> s{_chcHSMPartitionName = a});

-- | The password required to access the HSM partition.
chcHSMPartitionPassword :: Lens' CreateHSMConfiguration Text
chcHSMPartitionPassword = lens _chcHSMPartitionPassword (\ s a -> s{_chcHSMPartitionPassword = a});

-- | The HSMs public certificate file. When using Cloud HSM, the file name is
-- server.pem.
chcHSMServerPublicCertificate :: Lens' CreateHSMConfiguration Text
chcHSMServerPublicCertificate = lens _chcHSMServerPublicCertificate (\ s a -> s{_chcHSMServerPublicCertificate = a});

instance AWSRequest CreateHSMConfiguration where
        type Sv CreateHSMConfiguration = Redshift
        type Rs CreateHSMConfiguration =
             CreateHSMConfigurationResponse
        request = post
        response
          = receiveXMLWrapper "CreateHsmConfigurationResult"
              (\ s h x ->
                 CreateHSMConfigurationResponse' <$>
                   (x .@? "HsmConfiguration"))

instance ToHeaders CreateHSMConfiguration where
        toHeaders = const mempty

instance ToPath CreateHSMConfiguration where
        toPath = const "/"

instance ToQuery CreateHSMConfiguration where
        toQuery CreateHSMConfiguration'{..}
          = mconcat
              ["Action" =:
                 ("CreateHSMConfiguration" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "Tags" =: toQuery (toQueryList "Tag" <$> _chcTags),
               "HsmConfigurationIdentifier" =:
                 _chcHSMConfigurationIdentifier,
               "Description" =: _chcDescription,
               "HsmIpAddress" =: _chcHSMIPAddress,
               "HsmPartitionName" =: _chcHSMPartitionName,
               "HsmPartitionPassword" =: _chcHSMPartitionPassword,
               "HsmServerPublicCertificate" =:
                 _chcHSMServerPublicCertificate]

-- | /See:/ 'createHSMConfigurationResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chcrHSMConfiguration'
newtype CreateHSMConfigurationResponse = CreateHSMConfigurationResponse'{_chcrHSMConfiguration :: Maybe HSMConfiguration} deriving (Eq, Read, Show)

-- | 'CreateHSMConfigurationResponse' smart constructor.
createHSMConfigurationResponse :: CreateHSMConfigurationResponse
createHSMConfigurationResponse = CreateHSMConfigurationResponse'{_chcrHSMConfiguration = Nothing};

-- | FIXME: Undocumented member.
chcrHSMConfiguration :: Lens' CreateHSMConfigurationResponse (Maybe HSMConfiguration)
chcrHSMConfiguration = lens _chcrHSMConfiguration (\ s a -> s{_chcrHSMConfiguration = a});
