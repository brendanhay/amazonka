{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
      CreateHsmConfigurationMessage
    -- ** Request constructor
    , createHsmConfigurationMessage
    -- ** Request lenses
    , chcmDescription
    , chcmHsmConfigurationIdentifier
    , chcmHsmIpAddress
    , chcmHsmPartitionName
    , chcmHsmPartitionPassword
    , chcmHsmServerPublicCertificate

    -- * Response
    , CreateHsmConfigurationResult
    -- ** Response constructor
    , createHsmConfigurationResult
    -- ** Response lenses
    , chcrHsmConfiguration
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data CreateHsmConfigurationMessage = CreateHsmConfigurationMessage
    { _chcmDescription                :: Text
    , _chcmHsmConfigurationIdentifier :: Text
    , _chcmHsmIpAddress               :: Text
    , _chcmHsmPartitionName           :: Text
    , _chcmHsmPartitionPassword       :: Text
    , _chcmHsmServerPublicCertificate :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateHsmConfigurationMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chcmDescription' @::@ 'Text'
--
-- * 'chcmHsmConfigurationIdentifier' @::@ 'Text'
--
-- * 'chcmHsmIpAddress' @::@ 'Text'
--
-- * 'chcmHsmPartitionName' @::@ 'Text'
--
-- * 'chcmHsmPartitionPassword' @::@ 'Text'
--
-- * 'chcmHsmServerPublicCertificate' @::@ 'Text'
--
createHsmConfigurationMessage :: Text -- ^ 'chcmHsmConfigurationIdentifier'
                              -> Text -- ^ 'chcmDescription'
                              -> Text -- ^ 'chcmHsmIpAddress'
                              -> Text -- ^ 'chcmHsmPartitionName'
                              -> Text -- ^ 'chcmHsmPartitionPassword'
                              -> Text -- ^ 'chcmHsmServerPublicCertificate'
                              -> CreateHsmConfigurationMessage
createHsmConfigurationMessage p1 p2 p3 p4 p5 p6 = CreateHsmConfigurationMessage
    { _chcmHsmConfigurationIdentifier = p1
    , _chcmDescription                = p2
    , _chcmHsmIpAddress               = p3
    , _chcmHsmPartitionName           = p4
    , _chcmHsmPartitionPassword       = p5
    , _chcmHsmServerPublicCertificate = p6
    }

-- | A text description of the HSM configuration to be created.
chcmDescription :: Lens' CreateHsmConfigurationMessage Text
chcmDescription = lens _chcmDescription (\s a -> s { _chcmDescription = a })

-- | The identifier to be assigned to the new Amazon Redshift HSM
-- configuration.
chcmHsmConfigurationIdentifier :: Lens' CreateHsmConfigurationMessage Text
chcmHsmConfigurationIdentifier =
    lens _chcmHsmConfigurationIdentifier
        (\s a -> s { _chcmHsmConfigurationIdentifier = a })

-- | The IP address that the Amazon Redshift cluster must use to access the
-- HSM.
chcmHsmIpAddress :: Lens' CreateHsmConfigurationMessage Text
chcmHsmIpAddress = lens _chcmHsmIpAddress (\s a -> s { _chcmHsmIpAddress = a })

-- | The name of the partition in the HSM where the Amazon Redshift clusters
-- will store their database encryption keys.
chcmHsmPartitionName :: Lens' CreateHsmConfigurationMessage Text
chcmHsmPartitionName =
    lens _chcmHsmPartitionName (\s a -> s { _chcmHsmPartitionName = a })

-- | The password required to access the HSM partition.
chcmHsmPartitionPassword :: Lens' CreateHsmConfigurationMessage Text
chcmHsmPartitionPassword =
    lens _chcmHsmPartitionPassword
        (\s a -> s { _chcmHsmPartitionPassword = a })

-- | The HSMs public certificate file. When using Cloud HSM, the file name is
-- server.pem.
chcmHsmServerPublicCertificate :: Lens' CreateHsmConfigurationMessage Text
chcmHsmServerPublicCertificate =
    lens _chcmHsmServerPublicCertificate
        (\s a -> s { _chcmHsmServerPublicCertificate = a })
instance ToQuery CreateHsmConfigurationMessage

instance ToPath CreateHsmConfigurationMessage where
    toPath = const "/"

newtype CreateHsmConfigurationResult = CreateHsmConfigurationResult
    { _chcrHsmConfiguration :: Maybe HsmConfiguration
    } deriving (Eq, Show, Generic)

-- | 'CreateHsmConfigurationResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chcrHsmConfiguration' @::@ 'Maybe' 'HsmConfiguration'
--
createHsmConfigurationResult :: CreateHsmConfigurationResult
createHsmConfigurationResult = CreateHsmConfigurationResult
    { _chcrHsmConfiguration = Nothing
    }

chcrHsmConfiguration :: Lens' CreateHsmConfigurationResult (Maybe HsmConfiguration)
chcrHsmConfiguration =
    lens _chcrHsmConfiguration (\s a -> s { _chcrHsmConfiguration = a })
instance FromXML CreateHsmConfigurationResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateHsmConfigurationResult"

instance AWSRequest CreateHsmConfigurationMessage where
    type Sv CreateHsmConfigurationMessage = Redshift
    type Rs CreateHsmConfigurationMessage = CreateHsmConfigurationResult

    request  = post "CreateHsmConfiguration"
    response = xmlResponse $ \h x -> CreateHsmConfigurationResult
        <$> x %| "HsmConfiguration"
