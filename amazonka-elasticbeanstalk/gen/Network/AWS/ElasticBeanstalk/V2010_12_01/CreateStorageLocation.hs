{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.CreateStorageLocation
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
module Network.AWS.ElasticBeanstalk.V2010_12_01.CreateStorageLocation
    (
    -- * Request
      CreateStorageLocation
    -- ** Request constructor
    , mkUnknown
    -- * Response
    , CreateStorageLocationResponse
    -- ** Response lenses
    , cslrmS3Bucket
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateStorageLocation' request.
mkUnknown :: CreateStorageLocation
mkUnknown = CreateStorageLocation
{-# INLINE mkUnknown #-}

data CreateStorageLocation = CreateStorageLocation
    deriving (Eq, Show, Generic)

instance ToQuery CreateStorageLocation where
    toQuery = genericQuery def

newtype CreateStorageLocationResponse = CreateStorageLocationResponse
    { _cslrmS3Bucket :: Maybe Text
      -- ^ The name of the Amazon S3 bucket created.
    } deriving (Show, Generic)

-- | The name of the Amazon S3 bucket created.
cslrmS3Bucket :: Lens' CreateStorageLocationResponse (Maybe Text)
cslrmS3Bucket = lens _cslrmS3Bucket (\s a -> s { _cslrmS3Bucket = a })
{-# INLINE cslrmS3Bucket #-}

instance FromXML CreateStorageLocationResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateStorageLocation where
    type Sv CreateStorageLocation = ElasticBeanstalk
    type Rs CreateStorageLocation = CreateStorageLocationResponse

    request = post "CreateStorageLocation"
    response _ = xmlResponse
