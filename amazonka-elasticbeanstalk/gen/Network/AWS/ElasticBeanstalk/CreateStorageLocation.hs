{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates the Amazon S3 storage location for the account. This location is
-- used to store user log files.
-- https://elasticbeanstalk.us-east-1.amazon.com/?Operation=CreateStorageLocation
-- &AuthParams elasticbeanstalk-us-east-1-780612358023
-- ef51b94a-f1d6-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk
    (
    -- * Request
      CreateStorageLocation
    -- ** Request constructor
    , mkCreateStorageLocation
    -- * Response
    , CreateStorageLocationResponse
    -- ** Response constructor
    , mkCreateStorageLocationResponse
    -- ** Response lenses
    , cslrS3Bucket
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.Prelude

data CreateStorageLocation = CreateStorageLocation
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateStorageLocation' request.
mkCreateStorageLocation :: CreateStorageLocation
mkCreateStorageLocation = CreateStorageLocation

instance ToQuery CreateStorageLocation where
    toQuery = genericQuery def

-- | Results of a CreateStorageLocationResult call.
newtype CreateStorageLocationResponse = CreateStorageLocationResponse
    { _cslrS3Bucket :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateStorageLocationResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @S3Bucket ::@ @Maybe Text@
--
mkCreateStorageLocationResponse :: CreateStorageLocationResponse
mkCreateStorageLocationResponse = CreateStorageLocationResponse
    { _cslrS3Bucket = Nothing
    }

-- | The name of the Amazon S3 bucket created.
cslrS3Bucket :: Lens' CreateStorageLocationResponse (Maybe Text)
cslrS3Bucket = lens _cslrS3Bucket (\s a -> s { _cslrS3Bucket = a })

instance FromXML CreateStorageLocationResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateStorageLocation where
    type Sv CreateStorageLocation = ElasticBeanstalk
    type Rs CreateStorageLocation = CreateStorageLocationResponse

    request = post "CreateStorageLocation"
    response _ = xmlResponse
