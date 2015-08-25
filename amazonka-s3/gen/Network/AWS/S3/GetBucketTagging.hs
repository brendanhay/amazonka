{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketTagging
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the tag set associated with the bucket.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketTagging.html AWS API Reference> for GetBucketTagging.
module Network.AWS.S3.GetBucketTagging
    (
    -- * Creating a Request
      getBucketTagging
    , GetBucketTagging
    -- * Request Lenses
    , gbtBucket

    -- * Destructuring the Response
    , getBucketTaggingResponse
    , GetBucketTaggingResponse
    -- * Response Lenses
    , gbtrsStatus
    , gbtrsTagSet
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types
import           Network.AWS.S3.Types.Product

-- | /See:/ 'getBucketTagging' smart constructor.
newtype GetBucketTagging = GetBucketTagging'
    { _gbtBucket :: BucketName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetBucketTagging' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbtBucket'
getBucketTagging
    :: BucketName -- ^ 'gbtBucket'
    -> GetBucketTagging
getBucketTagging pBucket_ =
    GetBucketTagging'
    { _gbtBucket = pBucket_
    }

-- | Undocumented member.
gbtBucket :: Lens' GetBucketTagging BucketName
gbtBucket = lens _gbtBucket (\ s a -> s{_gbtBucket = a});

instance AWSRequest GetBucketTagging where
        type Rs GetBucketTagging = GetBucketTaggingResponse
        request = get s3
        response
          = receiveXML
              (\ s h x ->
                 GetBucketTaggingResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .@? "TagSet" .!@ mempty >>= parseXMLList "Tag"))

instance ToHeaders GetBucketTagging where
        toHeaders = const mempty

instance ToPath GetBucketTagging where
        toPath GetBucketTagging'{..}
          = mconcat ["/", toBS _gbtBucket]

instance ToQuery GetBucketTagging where
        toQuery = const (mconcat ["tagging"])

-- | /See:/ 'getBucketTaggingResponse' smart constructor.
data GetBucketTaggingResponse = GetBucketTaggingResponse'
    { _gbtrsStatus :: !Int
    , _gbtrsTagSet :: ![Tag]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetBucketTaggingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbtrsStatus'
--
-- * 'gbtrsTagSet'
getBucketTaggingResponse
    :: Int -- ^ 'gbtrsStatus'
    -> GetBucketTaggingResponse
getBucketTaggingResponse pStatus_ =
    GetBucketTaggingResponse'
    { _gbtrsStatus = pStatus_
    , _gbtrsTagSet = mempty
    }

-- | The response status code.
gbtrsStatus :: Lens' GetBucketTaggingResponse Int
gbtrsStatus = lens _gbtrsStatus (\ s a -> s{_gbtrsStatus = a});

-- | Undocumented member.
gbtrsTagSet :: Lens' GetBucketTaggingResponse [Tag]
gbtrsTagSet = lens _gbtrsTagSet (\ s a -> s{_gbtrsTagSet = a}) . _Coerce;
