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
-- Module      : Network.AWS.Redshift.CreateHSMConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an HSM configuration that contains the information required by an Amazon Redshift cluster to store and use database encryption keys in a Hardware Security Module (HSM). After creating the HSM configuration, you can specify it as a parameter when creating a cluster. The cluster will then store its encryption keys in the HSM.
--
--
-- In addition to creating an HSM configuration, you must also create an HSM client certificate. For more information, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-HSM.html Hardware Security Modules> in the Amazon Redshift Cluster Management Guide.
--
module Network.AWS.Redshift.CreateHSMConfiguration
    (
    -- * Creating a Request
      createHSMConfiguration
    , CreateHSMConfiguration
    -- * Request Lenses
    , chcTags
    , chcHSMConfigurationIdentifier
    , chcDescription
    , chcHSMIPAddress
    , chcHSMPartitionName
    , chcHSMPartitionPassword
    , chcHSMServerPublicCertificate

    -- * Destructuring the Response
    , createHSMConfigurationResponse
    , CreateHSMConfigurationResponse
    -- * Response Lenses
    , chcrsHSMConfiguration
    , chcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'createHSMConfiguration' smart constructor.
data CreateHSMConfiguration = CreateHSMConfiguration'
  { _chcTags                       :: !(Maybe [Tag])
  , _chcHSMConfigurationIdentifier :: !Text
  , _chcDescription                :: !Text
  , _chcHSMIPAddress               :: !Text
  , _chcHSMPartitionName           :: !Text
  , _chcHSMPartitionPassword       :: !Text
  , _chcHSMServerPublicCertificate :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateHSMConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chcTags' - A list of tag instances.
--
-- * 'chcHSMConfigurationIdentifier' - The identifier to be assigned to the new Amazon Redshift HSM configuration.
--
-- * 'chcDescription' - A text description of the HSM configuration to be created.
--
-- * 'chcHSMIPAddress' - The IP address that the Amazon Redshift cluster must use to access the HSM.
--
-- * 'chcHSMPartitionName' - The name of the partition in the HSM where the Amazon Redshift clusters will store their database encryption keys.
--
-- * 'chcHSMPartitionPassword' - The password required to access the HSM partition.
--
-- * 'chcHSMServerPublicCertificate' - The HSMs public certificate file. When using Cloud HSM, the file name is server.pem.
createHSMConfiguration
    :: Text -- ^ 'chcHSMConfigurationIdentifier'
    -> Text -- ^ 'chcDescription'
    -> Text -- ^ 'chcHSMIPAddress'
    -> Text -- ^ 'chcHSMPartitionName'
    -> Text -- ^ 'chcHSMPartitionPassword'
    -> Text -- ^ 'chcHSMServerPublicCertificate'
    -> CreateHSMConfiguration
createHSMConfiguration pHSMConfigurationIdentifier_ pDescription_ pHSMIPAddress_ pHSMPartitionName_ pHSMPartitionPassword_ pHSMServerPublicCertificate_ =
  CreateHSMConfiguration'
    { _chcTags = Nothing
    , _chcHSMConfigurationIdentifier = pHSMConfigurationIdentifier_
    , _chcDescription = pDescription_
    , _chcHSMIPAddress = pHSMIPAddress_
    , _chcHSMPartitionName = pHSMPartitionName_
    , _chcHSMPartitionPassword = pHSMPartitionPassword_
    , _chcHSMServerPublicCertificate = pHSMServerPublicCertificate_
    }


-- | A list of tag instances.
chcTags :: Lens' CreateHSMConfiguration [Tag]
chcTags = lens _chcTags (\ s a -> s{_chcTags = a}) . _Default . _Coerce

-- | The identifier to be assigned to the new Amazon Redshift HSM configuration.
chcHSMConfigurationIdentifier :: Lens' CreateHSMConfiguration Text
chcHSMConfigurationIdentifier = lens _chcHSMConfigurationIdentifier (\ s a -> s{_chcHSMConfigurationIdentifier = a})

-- | A text description of the HSM configuration to be created.
chcDescription :: Lens' CreateHSMConfiguration Text
chcDescription = lens _chcDescription (\ s a -> s{_chcDescription = a})

-- | The IP address that the Amazon Redshift cluster must use to access the HSM.
chcHSMIPAddress :: Lens' CreateHSMConfiguration Text
chcHSMIPAddress = lens _chcHSMIPAddress (\ s a -> s{_chcHSMIPAddress = a})

-- | The name of the partition in the HSM where the Amazon Redshift clusters will store their database encryption keys.
chcHSMPartitionName :: Lens' CreateHSMConfiguration Text
chcHSMPartitionName = lens _chcHSMPartitionName (\ s a -> s{_chcHSMPartitionName = a})

-- | The password required to access the HSM partition.
chcHSMPartitionPassword :: Lens' CreateHSMConfiguration Text
chcHSMPartitionPassword = lens _chcHSMPartitionPassword (\ s a -> s{_chcHSMPartitionPassword = a})

-- | The HSMs public certificate file. When using Cloud HSM, the file name is server.pem.
chcHSMServerPublicCertificate :: Lens' CreateHSMConfiguration Text
chcHSMServerPublicCertificate = lens _chcHSMServerPublicCertificate (\ s a -> s{_chcHSMServerPublicCertificate = a})

instance AWSRequest CreateHSMConfiguration where
        type Rs CreateHSMConfiguration =
             CreateHSMConfigurationResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper "CreateHsmConfigurationResult"
              (\ s h x ->
                 CreateHSMConfigurationResponse' <$>
                   (x .@? "HsmConfiguration") <*> (pure (fromEnum s)))

instance Hashable CreateHSMConfiguration where

instance NFData CreateHSMConfiguration where

instance ToHeaders CreateHSMConfiguration where
        toHeaders = const mempty

instance ToPath CreateHSMConfiguration where
        toPath = const "/"

instance ToQuery CreateHSMConfiguration where
        toQuery CreateHSMConfiguration'{..}
          = mconcat
              ["Action" =:
                 ("CreateHsmConfiguration" :: ByteString),
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
data CreateHSMConfigurationResponse = CreateHSMConfigurationResponse'
  { _chcrsHSMConfiguration :: !(Maybe HSMConfiguration)
  , _chcrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateHSMConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chcrsHSMConfiguration' - Undocumented member.
--
-- * 'chcrsResponseStatus' - -- | The response status code.
createHSMConfigurationResponse
    :: Int -- ^ 'chcrsResponseStatus'
    -> CreateHSMConfigurationResponse
createHSMConfigurationResponse pResponseStatus_ =
  CreateHSMConfigurationResponse'
    {_chcrsHSMConfiguration = Nothing, _chcrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
chcrsHSMConfiguration :: Lens' CreateHSMConfigurationResponse (Maybe HSMConfiguration)
chcrsHSMConfiguration = lens _chcrsHSMConfiguration (\ s a -> s{_chcrsHSMConfiguration = a})

-- | -- | The response status code.
chcrsResponseStatus :: Lens' CreateHSMConfigurationResponse Int
chcrsResponseStatus = lens _chcrsResponseStatus (\ s a -> s{_chcrsResponseStatus = a})

instance NFData CreateHSMConfigurationResponse where
