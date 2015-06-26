{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElasticBeanstalk.CreateStorageLocation
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates the Amazon S3 storage location for the account.
--
-- This location is used to store user log files.
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
    , cslrStatusCode
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createStorageLocation' smart constructor.
data CreateStorageLocation = CreateStorageLocation' deriving (Eq, Read, Show)

-- | 'CreateStorageLocation' smart constructor.
createStorageLocation :: CreateStorageLocation
createStorageLocation = CreateStorageLocation';

instance AWSRequest CreateStorageLocation where
        type Sv CreateStorageLocation = ElasticBeanstalk
        type Rs CreateStorageLocation =
             CreateStorageLocationResponse
        request = post
        response
          = receiveXMLWrapper "CreateStorageLocationResult"
              (\ s h x ->
                 CreateStorageLocationResponse' <$>
                   (x .@? "S3Bucket") <*> (pure (fromEnum s)))

instance ToHeaders CreateStorageLocation where
        toHeaders = const mempty

instance ToPath CreateStorageLocation where
        toPath = const "/"

instance ToQuery CreateStorageLocation where
        toQuery
          = const
              (mconcat
                 ["Action" =: ("CreateStorageLocation" :: ByteString),
                  "Version" =: ("2010-12-01" :: ByteString)])

-- | Results of a CreateStorageLocationResult call.
--
-- /See:/ 'createStorageLocationResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cslrS3Bucket'
--
-- * 'cslrStatusCode'
data CreateStorageLocationResponse = CreateStorageLocationResponse'{_cslrS3Bucket :: Maybe Text, _cslrStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'CreateStorageLocationResponse' smart constructor.
createStorageLocationResponse :: Int -> CreateStorageLocationResponse
createStorageLocationResponse pStatusCode = CreateStorageLocationResponse'{_cslrS3Bucket = Nothing, _cslrStatusCode = pStatusCode};

-- | The name of the Amazon S3 bucket created.
cslrS3Bucket :: Lens' CreateStorageLocationResponse (Maybe Text)
cslrS3Bucket = lens _cslrS3Bucket (\ s a -> s{_cslrS3Bucket = a});

-- | FIXME: Undocumented member.
cslrStatusCode :: Lens' CreateStorageLocationResponse Int
cslrStatusCode = lens _cslrStatusCode (\ s a -> s{_cslrStatusCode = a});
