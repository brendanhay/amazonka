{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateHSMConfiguration
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates an HSM configuration that contains the information required by
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
    , chcrqTags
    , chcrqHSMConfigurationIdentifier
    , chcrqDescription
    , chcrqHSMIPAddress
    , chcrqHSMPartitionName
    , chcrqHSMPartitionPassword
    , chcrqHSMServerPublicCertificate

    -- * Response
    , CreateHSMConfigurationResponse
    -- ** Response constructor
    , createHSMConfigurationResponse
    -- ** Response lenses
    , chcrsHSMConfiguration
    , chcrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'createHSMConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chcrqTags'
--
-- * 'chcrqHSMConfigurationIdentifier'
--
-- * 'chcrqDescription'
--
-- * 'chcrqHSMIPAddress'
--
-- * 'chcrqHSMPartitionName'
--
-- * 'chcrqHSMPartitionPassword'
--
-- * 'chcrqHSMServerPublicCertificate'
data CreateHSMConfiguration = CreateHSMConfiguration'
    { _chcrqTags                       :: !(Maybe [Tag])
    , _chcrqHSMConfigurationIdentifier :: !Text
    , _chcrqDescription                :: !Text
    , _chcrqHSMIPAddress               :: !Text
    , _chcrqHSMPartitionName           :: !Text
    , _chcrqHSMPartitionPassword       :: !Text
    , _chcrqHSMServerPublicCertificate :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateHSMConfiguration' smart constructor.
createHSMConfiguration :: Text -> Text -> Text -> Text -> Text -> Text -> CreateHSMConfiguration
createHSMConfiguration pHSMConfigurationIdentifier pDescription pHSMIPAddress pHSMPartitionName pHSMPartitionPassword pHSMServerPublicCertificate =
    CreateHSMConfiguration'
    { _chcrqTags = Nothing
    , _chcrqHSMConfigurationIdentifier = pHSMConfigurationIdentifier
    , _chcrqDescription = pDescription
    , _chcrqHSMIPAddress = pHSMIPAddress
    , _chcrqHSMPartitionName = pHSMPartitionName
    , _chcrqHSMPartitionPassword = pHSMPartitionPassword
    , _chcrqHSMServerPublicCertificate = pHSMServerPublicCertificate
    }

-- | A list of tag instances.
chcrqTags :: Lens' CreateHSMConfiguration [Tag]
chcrqTags = lens _chcrqTags (\ s a -> s{_chcrqTags = a}) . _Default;

-- | The identifier to be assigned to the new Amazon Redshift HSM
-- configuration.
chcrqHSMConfigurationIdentifier :: Lens' CreateHSMConfiguration Text
chcrqHSMConfigurationIdentifier = lens _chcrqHSMConfigurationIdentifier (\ s a -> s{_chcrqHSMConfigurationIdentifier = a});

-- | A text description of the HSM configuration to be created.
chcrqDescription :: Lens' CreateHSMConfiguration Text
chcrqDescription = lens _chcrqDescription (\ s a -> s{_chcrqDescription = a});

-- | The IP address that the Amazon Redshift cluster must use to access the
-- HSM.
chcrqHSMIPAddress :: Lens' CreateHSMConfiguration Text
chcrqHSMIPAddress = lens _chcrqHSMIPAddress (\ s a -> s{_chcrqHSMIPAddress = a});

-- | The name of the partition in the HSM where the Amazon Redshift clusters
-- will store their database encryption keys.
chcrqHSMPartitionName :: Lens' CreateHSMConfiguration Text
chcrqHSMPartitionName = lens _chcrqHSMPartitionName (\ s a -> s{_chcrqHSMPartitionName = a});

-- | The password required to access the HSM partition.
chcrqHSMPartitionPassword :: Lens' CreateHSMConfiguration Text
chcrqHSMPartitionPassword = lens _chcrqHSMPartitionPassword (\ s a -> s{_chcrqHSMPartitionPassword = a});

-- | The HSMs public certificate file. When using Cloud HSM, the file name is
-- server.pem.
chcrqHSMServerPublicCertificate :: Lens' CreateHSMConfiguration Text
chcrqHSMServerPublicCertificate = lens _chcrqHSMServerPublicCertificate (\ s a -> s{_chcrqHSMServerPublicCertificate = a});

instance AWSRequest CreateHSMConfiguration where
        type Sv CreateHSMConfiguration = Redshift
        type Rs CreateHSMConfiguration =
             CreateHSMConfigurationResponse
        request = post
        response
          = receiveXMLWrapper "CreateHsmConfigurationResult"
              (\ s h x ->
                 CreateHSMConfigurationResponse' <$>
                   (x .@? "HsmConfiguration") <*> (pure (fromEnum s)))

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
               "Tags" =: toQuery (toQueryList "Tag" <$> _chcrqTags),
               "HsmConfigurationIdentifier" =:
                 _chcrqHSMConfigurationIdentifier,
               "Description" =: _chcrqDescription,
               "HsmIpAddress" =: _chcrqHSMIPAddress,
               "HsmPartitionName" =: _chcrqHSMPartitionName,
               "HsmPartitionPassword" =: _chcrqHSMPartitionPassword,
               "HsmServerPublicCertificate" =:
                 _chcrqHSMServerPublicCertificate]

-- | /See:/ 'createHSMConfigurationResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chcrsHSMConfiguration'
--
-- * 'chcrsStatus'
data CreateHSMConfigurationResponse = CreateHSMConfigurationResponse'
    { _chcrsHSMConfiguration :: !(Maybe HSMConfiguration)
    , _chcrsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateHSMConfigurationResponse' smart constructor.
createHSMConfigurationResponse :: Int -> CreateHSMConfigurationResponse
createHSMConfigurationResponse pStatus =
    CreateHSMConfigurationResponse'
    { _chcrsHSMConfiguration = Nothing
    , _chcrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
chcrsHSMConfiguration :: Lens' CreateHSMConfigurationResponse (Maybe HSMConfiguration)
chcrsHSMConfiguration = lens _chcrsHSMConfiguration (\ s a -> s{_chcrsHSMConfiguration = a});

-- | FIXME: Undocumented member.
chcrsStatus :: Lens' CreateHSMConfigurationResponse Int
chcrsStatus = lens _chcrsStatus (\ s a -> s{_chcrsStatus = a});
