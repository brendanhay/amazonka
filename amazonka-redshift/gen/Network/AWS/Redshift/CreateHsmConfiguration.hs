{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
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
-- information, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-HSM.html
-- Hardware Security Modules> in the Amazon Redshift Cluster Management Guide.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_CreateHsmConfiguration.html>
module Network.AWS.Redshift.CreateHsmConfiguration
    (
    -- * Request
      CreateHsmConfiguration
    -- ** Request constructor
    , createHsmConfiguration
    -- ** Request lenses
    , chcDescription
    , chcHsmConfigurationIdentifier
    , chcHsmIpAddress
    , chcHsmPartitionName
    , chcHsmPartitionPassword
    , chcHsmServerPublicCertificate
    , chcTags

    -- * Response
    , CreateHsmConfigurationResponse
    -- ** Response constructor
    , createHsmConfigurationResponse
    -- ** Response lenses
    , chcrHsmConfiguration
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

data CreateHsmConfiguration = CreateHsmConfiguration
    { _chcDescription                :: Text
    , _chcHsmConfigurationIdentifier :: Text
    , _chcHsmIpAddress               :: Text
    , _chcHsmPartitionName           :: Text
    , _chcHsmPartitionPassword       :: Text
    , _chcHsmServerPublicCertificate :: Text
    , _chcTags                       :: List "Tag" Tag
    } deriving (Eq, Show)

-- | 'CreateHsmConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chcDescription' @::@ 'Text'
--
-- * 'chcHsmConfigurationIdentifier' @::@ 'Text'
--
-- * 'chcHsmIpAddress' @::@ 'Text'
--
-- * 'chcHsmPartitionName' @::@ 'Text'
--
-- * 'chcHsmPartitionPassword' @::@ 'Text'
--
-- * 'chcHsmServerPublicCertificate' @::@ 'Text'
--
-- * 'chcTags' @::@ ['Tag']
--
createHsmConfiguration :: Text -- ^ 'chcHsmConfigurationIdentifier'
                       -> Text -- ^ 'chcDescription'
                       -> Text -- ^ 'chcHsmIpAddress'
                       -> Text -- ^ 'chcHsmPartitionName'
                       -> Text -- ^ 'chcHsmPartitionPassword'
                       -> Text -- ^ 'chcHsmServerPublicCertificate'
                       -> CreateHsmConfiguration
createHsmConfiguration p1 p2 p3 p4 p5 p6 = CreateHsmConfiguration
    { _chcHsmConfigurationIdentifier = p1
    , _chcDescription                = p2
    , _chcHsmIpAddress               = p3
    , _chcHsmPartitionName           = p4
    , _chcHsmPartitionPassword       = p5
    , _chcHsmServerPublicCertificate = p6
    , _chcTags                       = mempty
    }

-- | A text description of the HSM configuration to be created.
chcDescription :: Lens' CreateHsmConfiguration Text
chcDescription = lens _chcDescription (\s a -> s { _chcDescription = a })

-- | The identifier to be assigned to the new Amazon Redshift HSM
-- configuration.
chcHsmConfigurationIdentifier :: Lens' CreateHsmConfiguration Text
chcHsmConfigurationIdentifier =
    lens _chcHsmConfigurationIdentifier
        (\s a -> s { _chcHsmConfigurationIdentifier = a })

-- | The IP address that the Amazon Redshift cluster must use to access the
-- HSM.
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
    lens _chcHsmPartitionPassword (\s a -> s { _chcHsmPartitionPassword = a })

-- | The HSMs public certificate file. When using Cloud HSM, the file name is
-- server.pem.
chcHsmServerPublicCertificate :: Lens' CreateHsmConfiguration Text
chcHsmServerPublicCertificate =
    lens _chcHsmServerPublicCertificate
        (\s a -> s { _chcHsmServerPublicCertificate = a })

-- | A list of tag instances.
chcTags :: Lens' CreateHsmConfiguration [Tag]
chcTags = lens _chcTags (\s a -> s { _chcTags = a }) . _List

newtype CreateHsmConfigurationResponse = CreateHsmConfigurationResponse
    { _chcrHsmConfiguration :: Maybe HsmConfiguration
    } deriving (Eq, Show)

-- | 'CreateHsmConfigurationResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chcrHsmConfiguration' @::@ 'Maybe' 'HsmConfiguration'
--
createHsmConfigurationResponse :: CreateHsmConfigurationResponse
createHsmConfigurationResponse = CreateHsmConfigurationResponse
    { _chcrHsmConfiguration = Nothing
    }

chcrHsmConfiguration :: Lens' CreateHsmConfigurationResponse (Maybe HsmConfiguration)
chcrHsmConfiguration =
    lens _chcrHsmConfiguration (\s a -> s { _chcrHsmConfiguration = a })

instance ToPath CreateHsmConfiguration where
    toPath = const "/"

instance ToQuery CreateHsmConfiguration where
    toQuery CreateHsmConfiguration{..} = mconcat
        [ "Description"                =? _chcDescription
        , "HsmConfigurationIdentifier" =? _chcHsmConfigurationIdentifier
        , "HsmIpAddress"               =? _chcHsmIpAddress
        , "HsmPartitionName"           =? _chcHsmPartitionName
        , "HsmPartitionPassword"       =? _chcHsmPartitionPassword
        , "HsmServerPublicCertificate" =? _chcHsmServerPublicCertificate
        , "Tags"                       =? _chcTags
        ]

instance ToHeaders CreateHsmConfiguration

instance AWSRequest CreateHsmConfiguration where
    type Sv CreateHsmConfiguration = Redshift
    type Rs CreateHsmConfiguration = CreateHsmConfigurationResponse

    request  = post "CreateHsmConfiguration"
    response = xmlResponse

instance FromXML CreateHsmConfigurationResponse where
    parseXML = withElement "CreateHsmConfigurationResult" $ \x -> CreateHsmConfigurationResponse
        <$> x .@? "HsmConfiguration"
