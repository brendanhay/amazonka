{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.CreateHsmConfiguration
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
module Network.AWS.Redshift.V2012_12_01.CreateHsmConfiguration where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.Redshift.V2012_12_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data CreateHsmConfiguration = CreateHsmConfiguration
    { _chcmHsmServerPublicCertificate :: Text
      -- ^ The HSMs public certificate file. When using Cloud HSM, the file
      -- name is server.pem.
    , _chcmHsmConfigurationIdentifier :: Text
      -- ^ The identifier to be assigned to the new Amazon Redshift HSM
      -- configuration.
    , _chcmHsmPartitionName :: Text
      -- ^ The name of the partition in the HSM where the Amazon Redshift
      -- clusters will store their database encryption keys.
    , _chcmHsmPartitionPassword :: Text
      -- ^ The password required to access the HSM partition.
    , _chcmDescription :: Text
      -- ^ A text description of the HSM configuration to be created.
    , _chcmHsmIpAddress :: Text
      -- ^ The IP address that the Amazon Redshift cluster must use to
      -- access the HSM.
    } deriving (Generic)

instance ToQuery CreateHsmConfiguration where
    toQuery = genericToQuery def

instance AWSRequest CreateHsmConfiguration where
    type Sv CreateHsmConfiguration = Redshift
    type Rs CreateHsmConfiguration = CreateHsmConfigurationResponse

    request = post "CreateHsmConfiguration"
    response _ = xmlResponse

data CreateHsmConfigurationResponse = CreateHsmConfigurationResponse
    { _hcwHsmConfiguration :: Maybe HsmConfiguration
      -- ^ Returns information about an HSM configuration, which is an
      -- object that describes to Amazon Redshift clusters the information
      -- they require to connect to an HSM where they can store database
      -- encryption keys.
    } deriving (Generic)

instance FromXML CreateHsmConfigurationResponse where
    fromXMLOptions = xmlOptions
