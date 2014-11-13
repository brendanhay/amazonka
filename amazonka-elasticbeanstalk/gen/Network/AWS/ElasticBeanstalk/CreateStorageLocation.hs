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

data CreateStorageLocation = CreateStorageLocation
    deriving (Eq, Ord, Show, Generic)

-- | 'CreateStorageLocation' constructor.
createStorageLocation :: CreateStorageLocation
createStorageLocation = CreateStorageLocation

instance ToQuery CreateStorageLocation

instance ToPath CreateStorageLocation where
    toPath = const "/"

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

instance FromXML CreateStorageLocationResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateStorageLocationResponse"

instance AWSRequest CreateStorageLocation where
    type Sv CreateStorageLocation = ElasticBeanstalk
    type Rs CreateStorageLocation = CreateStorageLocationResponse

    request  = post "CreateStorageLocation"
    response = xmlResponse $ \h x -> CreateStorageLocationResponse
        <$> x %| "S3Bucket"
