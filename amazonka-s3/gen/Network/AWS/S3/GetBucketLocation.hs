{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.S3.GetBucketLocation
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

-- | Returns the region the bucket resides in.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketLocation.html>
module Network.AWS.S3.GetBucketLocation
    (
    -- * Request
      GetBucketLocation
    -- ** Request constructor
    , getBucketLocation
    -- ** Request lenses
    , gblBucket

    -- * Response
    , GetBucketLocationResponse
    -- ** Response constructor
    , getBucketLocationResponse
    -- ** Response lenses
    , gblrLocationConstraint
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.S3.Types

-- | /See:/ 'getBucketLocation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gblBucket'
newtype GetBucketLocation = GetBucketLocation'{_gblBucket :: BucketName} deriving (Eq, Read, Show)

-- | 'GetBucketLocation' smart constructor.
getBucketLocation :: BucketName -> GetBucketLocation
getBucketLocation pBucket = GetBucketLocation'{_gblBucket = pBucket};

-- | FIXME: Undocumented member.
gblBucket :: Lens' GetBucketLocation BucketName
gblBucket = lens _gblBucket (\ s a -> s{_gblBucket = a});

instance AWSRequest GetBucketLocation where
        type Sv GetBucketLocation = S3
        type Rs GetBucketLocation = GetBucketLocationResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetBucketLocationResponse' <$>
                   x .@? "LocationConstraint")

instance ToHeaders GetBucketLocation where
        toHeaders = const mempty

instance ToPath GetBucketLocation where
        toPath GetBucketLocation'{..}
          = mconcat ["/", toText _gblBucket]

instance ToQuery GetBucketLocation where
        toQuery = const (mconcat ["location"])

-- | /See:/ 'getBucketLocationResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gblrLocationConstraint'
newtype GetBucketLocationResponse = GetBucketLocationResponse'{_gblrLocationConstraint :: Maybe Region} deriving (Eq, Read, Show)

-- | 'GetBucketLocationResponse' smart constructor.
getBucketLocationResponse :: GetBucketLocationResponse
getBucketLocationResponse = GetBucketLocationResponse'{_gblrLocationConstraint = Nothing};

-- | FIXME: Undocumented member.
gblrLocationConstraint :: Lens' GetBucketLocationResponse (Maybe Region)
gblrLocationConstraint = lens _gblrLocationConstraint (\ s a -> s{_gblrLocationConstraint = a});
