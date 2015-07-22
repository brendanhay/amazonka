{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketTagging
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Sets the tags for a bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/PutBucketTagging.html>
module Network.AWS.S3.PutBucketTagging
    (
    -- * Request
      PutBucketTagging
    -- ** Request constructor
    , putBucketTagging
    -- ** Request lenses
    , pbtrqContentMD5
    , pbtrqBucket
    , pbtrqTagging

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
-- * 'pbtrqContentMD5'
--
-- * 'pbtrqBucket'
--
-- * 'pbtrqTagging'
data PutBucketTagging = PutBucketTagging'
    { _pbtrqContentMD5 :: !(Maybe Text)
    , _pbtrqBucket     :: !BucketName
    , _pbtrqTagging    :: !Tagging
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'PutBucketTagging' smart constructor.
putBucketTagging :: BucketName -> Tagging -> PutBucketTagging
putBucketTagging pBucket pTagging =
    PutBucketTagging'
    { _pbtrqContentMD5 = Nothing
    , _pbtrqBucket = pBucket
    , _pbtrqTagging = pTagging
    }

-- | FIXME: Undocumented member.
pbtrqContentMD5 :: Lens' PutBucketTagging (Maybe Text)
pbtrqContentMD5 = lens _pbtrqContentMD5 (\ s a -> s{_pbtrqContentMD5 = a});

-- | FIXME: Undocumented member.
pbtrqBucket :: Lens' PutBucketTagging BucketName
pbtrqBucket = lens _pbtrqBucket (\ s a -> s{_pbtrqBucket = a});

-- | FIXME: Undocumented member.
pbtrqTagging :: Lens' PutBucketTagging Tagging
pbtrqTagging = lens _pbtrqTagging (\ s a -> s{_pbtrqTagging = a});

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
              _pbtrqTagging

instance ToHeaders PutBucketTagging where
        toHeaders PutBucketTagging'{..}
          = mconcat ["Content-MD5" =# _pbtrqContentMD5]

instance ToPath PutBucketTagging where
        toPath PutBucketTagging'{..}
          = mconcat ["/", toText _pbtrqBucket]

instance ToQuery PutBucketTagging where
        toQuery = const (mconcat ["tagging"])

-- | /See:/ 'putBucketTaggingResponse' smart constructor.
data PutBucketTaggingResponse =
    PutBucketTaggingResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutBucketTaggingResponse' smart constructor.
putBucketTaggingResponse :: PutBucketTaggingResponse
putBucketTaggingResponse = PutBucketTaggingResponse'
