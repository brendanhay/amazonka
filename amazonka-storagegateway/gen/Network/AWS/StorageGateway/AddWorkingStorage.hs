{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.AddWorkingStorage
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation configures one or more gateway local disks as working
-- storage for a gateway. This operation is supported only for the
-- gateway-stored volume architecture. This operation is deprecated method in
-- cached-volumes API version (20120630). Use AddUploadBuffer instead. In the
-- request, you specify the gateway Amazon Resource Name (ARN) to which you
-- want to add working storage, and one or more disk IDs that you want to
-- configure as working storage.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_AddWorkingStorage.html>
module Network.AWS.StorageGateway.AddWorkingStorage
    (
    -- * Request
      AddWorkingStorage
    -- ** Request constructor
    , addWorkingStorage
    -- ** Request lenses
    , awsDiskIds
    , awsGatewayARN

    -- * Response
    , AddWorkingStorageResponse
    -- ** Response constructor
    , addWorkingStorageResponse
    -- ** Response lenses
    , awsrGatewayARN
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

data AddWorkingStorage = AddWorkingStorage
    { _awsDiskIds    :: [Text]
    , _awsGatewayARN :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'AddWorkingStorage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'awsDiskIds' @::@ ['Text']
--
-- * 'awsGatewayARN' @::@ 'Text'
--
addWorkingStorage :: Text -- ^ 'awsGatewayARN'
                  -> AddWorkingStorage
addWorkingStorage p1 = AddWorkingStorage
    { _awsGatewayARN = p1
    , _awsDiskIds    = mempty
    }

-- | An array of strings that identify disks that are to be configured as
-- working storage. Each string have a minimum length of 1 and maximum
-- length of 300. You can get the disk IDs from the ListLocalDisks API.
awsDiskIds :: Lens' AddWorkingStorage [Text]
awsDiskIds = lens _awsDiskIds (\s a -> s { _awsDiskIds = a })

awsGatewayARN :: Lens' AddWorkingStorage Text
awsGatewayARN = lens _awsGatewayARN (\s a -> s { _awsGatewayARN = a })

newtype AddWorkingStorageResponse = AddWorkingStorageResponse
    { _awsrGatewayARN :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'AddWorkingStorageResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'awsrGatewayARN' @::@ 'Maybe' 'Text'
--
addWorkingStorageResponse :: AddWorkingStorageResponse
addWorkingStorageResponse = AddWorkingStorageResponse
    { _awsrGatewayARN = Nothing
    }

awsrGatewayARN :: Lens' AddWorkingStorageResponse (Maybe Text)
awsrGatewayARN = lens _awsrGatewayARN (\s a -> s { _awsrGatewayARN = a })

instance ToPath AddWorkingStorage where
    toPath = const "/"

instance ToQuery AddWorkingStorage where
    toQuery = const mempty

instance ToHeaders AddWorkingStorage

instance ToJSON AddWorkingStorage where
    toJSON AddWorkingStorage{..} = object
        [ "GatewayARN" .= _awsGatewayARN
        , "DiskIds"    .= _awsDiskIds
        ]

instance AWSRequest AddWorkingStorage where
    type Sv AddWorkingStorage = StorageGateway
    type Rs AddWorkingStorage = AddWorkingStorageResponse

    request  = post "AddWorkingStorage"
    response = jsonResponse

instance FromJSON AddWorkingStorageResponse where
    parseJSON = withObject "AddWorkingStorageResponse" $ \o -> AddWorkingStorageResponse
        <$> o .:? "GatewayARN"
