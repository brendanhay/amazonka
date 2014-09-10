{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.CreateHsmConfiguration
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an HSM configuration that contains the information required by an
-- Amazon Redshift cluster to store and use database encryption keys in a
-- Hardware Security Module (HSM). After creating the HSM configuration, you
-- can specify it as a parameter when creating a cluster. The cluster will
-- then store its encryption keys in the HSM. In addition to creating an HSM
-- configuration, you must also create an HSM client certificate. For more
-- information, go to Hardware Security Modules in the Amazon Redshift
-- Management Guide.
module Network.AWS.Redshift.CreateHsmConfiguration
    (
    -- * Request
      CreateHsmConfiguration
    -- ** Request constructor
    , mkCreateHsmConfiguration
    -- ** Request lenses
    , chcHsmConfigurationIdentifier
    , chcDescription
    , chcHsmIpAddress
    , chcHsmPartitionName
    , chcHsmPartitionPassword
    , chcHsmServerPublicCertificate

    -- * Response
    , CreateHsmConfigurationResponse
    -- ** Response constructor
    , mkCreateHsmConfigurationResponse
    -- ** Response lenses
    , chcrHsmConfiguration
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import Network.AWS.Prelude

-- | 
data CreateHsmConfiguration = CreateHsmConfiguration
    { _chcHsmConfigurationIdentifier :: !Text
    , _chcDescription :: !Text
    , _chcHsmIpAddress :: !Text
    , _chcHsmPartitionName :: !Text
    , _chcHsmPartitionPassword :: !Text
    , _chcHsmServerPublicCertificate :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateHsmConfiguration' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @HsmConfigurationIdentifier ::@ @Text@
--
-- * @Description ::@ @Text@
--
-- * @HsmIpAddress ::@ @Text@
--
-- * @HsmPartitionName ::@ @Text@
--
-- * @HsmPartitionPassword ::@ @Text@
--
-- * @HsmServerPublicCertificate ::@ @Text@
--
mkCreateHsmConfiguration :: Text -- ^ 'chcHsmConfigurationIdentifier'
                         -> Text -- ^ 'chcDescription'
                         -> Text -- ^ 'chcHsmIpAddress'
                         -> Text -- ^ 'chcHsmPartitionName'
                         -> Text -- ^ 'chcHsmPartitionPassword'
                         -> Text -- ^ 'chcHsmServerPublicCertificate'
                         -> CreateHsmConfiguration
mkCreateHsmConfiguration p1 p2 p3 p4 p5 p6 = CreateHsmConfiguration
    { _chcHsmConfigurationIdentifier = p1
    , _chcDescription = p2
    , _chcHsmIpAddress = p3
    , _chcHsmPartitionName = p4
    , _chcHsmPartitionPassword = p5
    , _chcHsmServerPublicCertificate = p6
    }

-- | The identifier to be assigned to the new Amazon Redshift HSM configuration.
chcHsmConfigurationIdentifier :: Lens' CreateHsmConfiguration Text
chcHsmConfigurationIdentifier =
    lens _chcHsmConfigurationIdentifier
         (\s a -> s { _chcHsmConfigurationIdentifier = a })

-- | A text description of the HSM configuration to be created.
chcDescription :: Lens' CreateHsmConfiguration Text
chcDescription = lens _chcDescription (\s a -> s { _chcDescription = a })

-- | The IP address that the Amazon Redshift cluster must use to access the HSM.
chcHsmIpAddress :: Lens' CreateHsmConfiguration Text
chcHsmIpAddress = lens _chcHsmIpAddress (\s a -> s { _chcHsmIpAddress = a })

-- | The name of the partition in the HSM where the Amazon Redshift clusters
-- will store their database encryption keys.
chcHsmPartitionName :: Lens' CreateHsmConfiguration Text
chcHsmPartitionName =
    lens _chcHsmPartitionName (\s a -> s { _chcHsmPartitionName = a })

-- | The password required to access the HSM partition.
chcHsmPartitionPassword :: Lens' CreateHsmConfiguration Text
chcHsmPartitionPassword =
    lens _chcHsmPartitionPassword
         (\s a -> s { _chcHsmPartitionPassword = a })

-- | The HSMs public certificate file. When using Cloud HSM, the file name is
-- server.pem.
chcHsmServerPublicCertificate :: Lens' CreateHsmConfiguration Text
chcHsmServerPublicCertificate =
    lens _chcHsmServerPublicCertificate
         (\s a -> s { _chcHsmServerPublicCertificate = a })

instance ToQuery CreateHsmConfiguration where
    toQuery = genericQuery def

newtype CreateHsmConfigurationResponse = CreateHsmConfigurationResponse
    { _chcrHsmConfiguration :: Maybe HsmConfiguration
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateHsmConfigurationResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @HsmConfiguration ::@ @Maybe HsmConfiguration@
--
mkCreateHsmConfigurationResponse :: CreateHsmConfigurationResponse
mkCreateHsmConfigurationResponse = CreateHsmConfigurationResponse
    { _chcrHsmConfiguration = Nothing
    }

-- | Returns information about an HSM configuration, which is an object that
-- describes to Amazon Redshift clusters the information they require to
-- connect to an HSM where they can store database encryption keys.
chcrHsmConfiguration :: Lens' CreateHsmConfigurationResponse (Maybe HsmConfiguration)
chcrHsmConfiguration =
    lens _chcrHsmConfiguration (\s a -> s { _chcrHsmConfiguration = a })

instance FromXML CreateHsmConfigurationResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateHsmConfiguration where
    type Sv CreateHsmConfiguration = Redshift
    type Rs CreateHsmConfiguration = CreateHsmConfigurationResponse

    request = post "CreateHsmConfiguration"
    response _ = xmlResponse
