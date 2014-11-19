{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.CreateStorageLocation
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
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_CreateStorageLocation.html>
module Network.AWS.ElasticBeanstalk.CreateStorageLocation
    (
    -- * Request
      CreateStorageLocation
    -- ** Request constructor
    , createStorageLocation

    -- * Response
    , CreateStorageLocationResponse
    -- ** Response constructor
    , createStorageLocationResponse
    -- ** Response lenses
    , cslrS3Bucket
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import qualified GHC.Exts

data CreateStorageLocation = CreateStorageLocation
    deriving (Eq, Ord, Show, Generic)

-- | 'CreateStorageLocation' constructor.
createStorageLocation :: CreateStorageLocation
createStorageLocation = CreateStorageLocation

newtype CreateStorageLocationResponse = CreateStorageLocationResponse
    { _cslrS3Bucket :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'CreateStorageLocationResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cslrS3Bucket' @::@ 'Maybe' 'Text'
--
createStorageLocationResponse :: CreateStorageLocationResponse
createStorageLocationResponse = CreateStorageLocationResponse
    { _cslrS3Bucket = Nothing
    }

-- | The name of the Amazon S3 bucket created.
cslrS3Bucket :: Lens' CreateStorageLocationResponse (Maybe Text)
cslrS3Bucket = lens _cslrS3Bucket (\s a -> s { _cslrS3Bucket = a })

instance ToPath CreateStorageLocation where
    toPath = const "/"

instance ToQuery CreateStorageLocation where
    toQuery = const mempty

instance ToHeaders CreateStorageLocation

instance AWSRequest CreateStorageLocation where
    type Sv CreateStorageLocation = ElasticBeanstalk
    type Rs CreateStorageLocation = CreateStorageLocationResponse

    request  = post "CreateStorageLocation"
    response = xmlResponse

instance FromXML CreateStorageLocationResponse where
    parseXML = withElement "CreateStorageLocationResult" $ \x ->
        CreateStorageLocationResponse
            <$> x .@? "S3Bucket"
