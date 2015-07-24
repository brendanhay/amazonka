{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.HeadBucket
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation is useful to determine if a bucket exists and you have
-- permission to access it.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/HeadBucket.html>
module Network.AWS.S3.HeadBucket
    (
    -- * Request
      HeadBucket
    -- ** Request constructor
    , headBucket
    -- ** Request lenses
    , hbBucket

    -- * Response
    , HeadBucketResponse
    -- ** Response constructor
    , headBucketResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'headBucket' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hbBucket'
newtype HeadBucket = HeadBucket'
    { _hbBucket :: BucketName
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'HeadBucket' smart constructor.
headBucket :: BucketName -> HeadBucket
headBucket pBucket_ =
    HeadBucket'
    { _hbBucket = pBucket_
    }

-- | FIXME: Undocumented member.
hbBucket :: Lens' HeadBucket BucketName
hbBucket = lens _hbBucket (\ s a -> s{_hbBucket = a});

instance AWSRequest HeadBucket where
        type Sv HeadBucket = S3
        type Rs HeadBucket = HeadBucketResponse
        request = head' "HeadBucket"
        response = receiveNull HeadBucketResponse'

instance ToHeaders HeadBucket where
        toHeaders = const mempty

instance ToPath HeadBucket where
        toPath HeadBucket'{..}
          = mconcat ["/", toText _hbBucket]

instance ToQuery HeadBucket where
        toQuery = const mempty

-- | /See:/ 'headBucketResponse' smart constructor.
data HeadBucketResponse =
    HeadBucketResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'HeadBucketResponse' smart constructor.
headBucketResponse :: HeadBucketResponse
headBucketResponse = HeadBucketResponse'
