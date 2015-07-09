{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketTagging
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Sets the tags for a bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/PutBucketTagging.html>
module Network.AWS.S3.PutBucketTagging
    (
    -- * Request
      PutBucketTagging
    -- ** Request constructor
    , putBucketTagging
    -- ** Request lenses
    , pbtContentMD5
    , pbtBucket
    , pbtTagging

    -- * Response
    , PutBucketTaggingResponse
    -- ** Response constructor
    , putBucketTaggingResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'putBucketTagging' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pbtContentMD5'
--
-- * 'pbtBucket'
--
-- * 'pbtTagging'
data PutBucketTagging = PutBucketTagging'
    { _pbtContentMD5 :: !(Maybe Text)
    , _pbtBucket     :: !BucketName
    , _pbtTagging    :: !Tagging
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'PutBucketTagging' smart constructor.
putBucketTagging :: BucketName -> Tagging -> PutBucketTagging
putBucketTagging pBucket pTagging =
    PutBucketTagging'
    { _pbtContentMD5 = Nothing
    , _pbtBucket = pBucket
    , _pbtTagging = pTagging
    }

-- | FIXME: Undocumented member.
pbtContentMD5 :: Lens' PutBucketTagging (Maybe Text)
pbtContentMD5 = lens _pbtContentMD5 (\ s a -> s{_pbtContentMD5 = a});

-- | FIXME: Undocumented member.
pbtBucket :: Lens' PutBucketTagging BucketName
pbtBucket = lens _pbtBucket (\ s a -> s{_pbtBucket = a});

-- | FIXME: Undocumented member.
pbtTagging :: Lens' PutBucketTagging Tagging
pbtTagging = lens _pbtTagging (\ s a -> s{_pbtTagging = a});

instance AWSRequest PutBucketTagging where
        type Sv PutBucketTagging = S3
        type Rs PutBucketTagging = PutBucketTaggingResponse
        request = putXML
        response = receiveNull PutBucketTaggingResponse'

instance ToElement PutBucketTagging where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}Tagging"
              .
              _pbtTagging

instance ToHeaders PutBucketTagging where
        toHeaders PutBucketTagging'{..}
          = mconcat ["Content-MD5" =# _pbtContentMD5]

instance ToPath PutBucketTagging where
        toPath PutBucketTagging'{..}
          = mconcat ["/", toText _pbtBucket]

instance ToQuery PutBucketTagging where
        toQuery = const (mconcat ["tagging"])

-- | /See:/ 'putBucketTaggingResponse' smart constructor.
data PutBucketTaggingResponse =
    PutBucketTaggingResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutBucketTaggingResponse' smart constructor.
putBucketTaggingResponse :: PutBucketTaggingResponse
putBucketTaggingResponse = PutBucketTaggingResponse'
