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
-- Module      : Network.AWS.S3.PutBucketCORS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the cors configuration for a bucket.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonS3/latest/API/PutBucketCORS.html AWS API Reference> for PutBucketCORS.
module Network.AWS.S3.PutBucketCORS
    (
    -- * Creating a Request
      putBucketCORS
    , PutBucketCORS
    -- * Request Lenses
    , pbcContentMD5
    , pbcCORSConfiguration
    , pbcBucket

    -- * Destructuring the Response
    , putBucketCORSResponse
    , PutBucketCORSResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types
import           Network.AWS.S3.Types.Product

-- | /See:/ 'putBucketCORS' smart constructor.
data PutBucketCORS = PutBucketCORS'
    { _pbcContentMD5        :: !(Maybe Text)
    , _pbcCORSConfiguration :: !(Maybe CORSConfiguration)
    , _pbcBucket            :: !BucketName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutBucketCORS' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbcContentMD5'
--
-- * 'pbcCORSConfiguration'
--
-- * 'pbcBucket'
putBucketCORS
    :: BucketName -- ^ 'pbcBucket'
    -> PutBucketCORS
putBucketCORS pBucket_ =
    PutBucketCORS'
    { _pbcContentMD5 = Nothing
    , _pbcCORSConfiguration = Nothing
    , _pbcBucket = pBucket_
    }

-- | Undocumented member.
pbcContentMD5 :: Lens' PutBucketCORS (Maybe Text)
pbcContentMD5 = lens _pbcContentMD5 (\ s a -> s{_pbcContentMD5 = a});

-- | Undocumented member.
pbcCORSConfiguration :: Lens' PutBucketCORS (Maybe CORSConfiguration)
pbcCORSConfiguration = lens _pbcCORSConfiguration (\ s a -> s{_pbcCORSConfiguration = a});

-- | Undocumented member.
pbcBucket :: Lens' PutBucketCORS BucketName
pbcBucket = lens _pbcBucket (\ s a -> s{_pbcBucket = a});

instance AWSRequest PutBucketCORS where
        type Rs PutBucketCORS = PutBucketCORSResponse
        request = contentMD5 . putXML s3
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
          = mconcat ["/", toBS _pbcBucket]

instance ToQuery PutBucketCORS where
        toQuery = const (mconcat ["cors"])

-- | /See:/ 'putBucketCORSResponse' smart constructor.
data PutBucketCORSResponse =
    PutBucketCORSResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutBucketCORSResponse' with the minimum fields required to make a request.
--
putBucketCORSResponse
    :: PutBucketCORSResponse
putBucketCORSResponse = PutBucketCORSResponse'
