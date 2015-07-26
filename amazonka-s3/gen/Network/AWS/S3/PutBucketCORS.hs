{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketCORS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Sets the cors configuration for a bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/PutBucketCORS.html>
module Network.AWS.S3.PutBucketCORS
    (
    -- * Request
      PutBucketCORS
    -- ** Request constructor
    , putBucketCORS
    -- ** Request lenses
    , pbcContentMD5
    , pbcCORSConfiguration
    , pbcBucket

    -- * Response
    , PutBucketCORSResponse
    -- ** Response constructor
    , putBucketCORSResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'putBucketCORS' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pbcContentMD5'
--
-- * 'pbcCORSConfiguration'
--
-- * 'pbcBucket'
data PutBucketCORS = PutBucketCORS'
    { _pbcContentMD5        :: !(Maybe Text)
    , _pbcCORSConfiguration :: !(Maybe CORSConfiguration)
    , _pbcBucket            :: !BucketName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutBucketCORS' smart constructor.
putBucketCORS :: BucketName -> PutBucketCORS
putBucketCORS pBucket_ =
    PutBucketCORS'
    { _pbcContentMD5 = Nothing
    , _pbcCORSConfiguration = Nothing
    , _pbcBucket = pBucket_
    }

-- | FIXME: Undocumented member.
pbcContentMD5 :: Lens' PutBucketCORS (Maybe Text)
pbcContentMD5 = lens _pbcContentMD5 (\ s a -> s{_pbcContentMD5 = a});

-- | FIXME: Undocumented member.
pbcCORSConfiguration :: Lens' PutBucketCORS (Maybe CORSConfiguration)
pbcCORSConfiguration = lens _pbcCORSConfiguration (\ s a -> s{_pbcCORSConfiguration = a});

-- | FIXME: Undocumented member.
pbcBucket :: Lens' PutBucketCORS BucketName
pbcBucket = lens _pbcBucket (\ s a -> s{_pbcBucket = a});

instance AWSRequest PutBucketCORS where
        type Sv PutBucketCORS = S3
        type Rs PutBucketCORS = PutBucketCORSResponse
        request = contentMD5 . putXML
        response = receiveNull PutBucketCORSResponse'

instance ToElement PutBucketCORS where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}CORSConfiguration"
              .
              _pbcCORSConfiguration

instance ToHeaders PutBucketCORS where
        toHeaders PutBucketCORS'{..}
          = mconcat ["Content-MD5" =# _pbcContentMD5]

instance ToPath PutBucketCORS where
        toPath PutBucketCORS'{..}
          = mconcat ["/", toPath _pbcBucket]

instance ToQuery PutBucketCORS where
        toQuery = const (mconcat ["cors"])

-- | /See:/ 'putBucketCORSResponse' smart constructor.
data PutBucketCORSResponse =
    PutBucketCORSResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutBucketCORSResponse' smart constructor.
putBucketCORSResponse :: PutBucketCORSResponse
putBucketCORSResponse = PutBucketCORSResponse'
