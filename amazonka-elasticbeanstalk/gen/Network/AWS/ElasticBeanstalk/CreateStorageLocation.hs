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
    , CreateStorageLocationResultMessage
    -- ** Response constructor
    , createStorageLocationResultMessage
    -- ** Response lenses
    , cslrmS3Bucket
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

newtype CreateStorageLocationResultMessage = CreateStorageLocationResultMessage
    { _cslrmS3Bucket :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'CreateStorageLocationResultMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cslrmS3Bucket' @::@ 'Maybe' 'Text'
--
createStorageLocationResultMessage :: CreateStorageLocationResultMessage
createStorageLocationResultMessage = CreateStorageLocationResultMessage
    { _cslrmS3Bucket = Nothing
    }

-- | The name of the Amazon S3 bucket created.
cslrmS3Bucket :: Lens' CreateStorageLocationResultMessage (Maybe Text)
cslrmS3Bucket = lens _cslrmS3Bucket (\s a -> s { _cslrmS3Bucket = a })

instance FromXML CreateStorageLocationResultMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateStorageLocationResultMessage"

instance AWSRequest CreateStorageLocation where
    type Sv CreateStorageLocation = ElasticBeanstalk
    type Rs CreateStorageLocation = CreateStorageLocationResultMessage

    request  = post "CreateStorageLocation"
    response = xmlResponse $ \h x -> CreateStorageLocationResultMessage
        <$> x %| "S3Bucket"
