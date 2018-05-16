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
-- Module      : Network.AWS.S3.PutBucketLogging
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Set the logging parameters for a bucket and to specify permissions for who can view and modify the logging parameters. To set the logging status of a bucket, you must be the bucket owner.
module Network.AWS.S3.PutBucketLogging
    (
    -- * Creating a Request
      putBucketLogging
    , PutBucketLogging
    -- * Request Lenses
    , pblContentMD5
    , pblBucket
    , pblBucketLoggingStatus

    -- * Destructuring the Response
    , putBucketLoggingResponse
    , PutBucketLoggingResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'putBucketLogging' smart constructor.
data PutBucketLogging = PutBucketLogging'
  { _pblContentMD5          :: !(Maybe Text)
  , _pblBucket              :: !BucketName
  , _pblBucketLoggingStatus :: !BucketLoggingStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutBucketLogging' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pblContentMD5' - Undocumented member.
--
-- * 'pblBucket' - Undocumented member.
--
-- * 'pblBucketLoggingStatus' - Undocumented member.
putBucketLogging
    :: BucketName -- ^ 'pblBucket'
    -> BucketLoggingStatus -- ^ 'pblBucketLoggingStatus'
    -> PutBucketLogging
putBucketLogging pBucket_ pBucketLoggingStatus_ =
  PutBucketLogging'
    { _pblContentMD5 = Nothing
    , _pblBucket = pBucket_
    , _pblBucketLoggingStatus = pBucketLoggingStatus_
    }


-- | Undocumented member.
pblContentMD5 :: Lens' PutBucketLogging (Maybe Text)
pblContentMD5 = lens _pblContentMD5 (\ s a -> s{_pblContentMD5 = a})

-- | Undocumented member.
pblBucket :: Lens' PutBucketLogging BucketName
pblBucket = lens _pblBucket (\ s a -> s{_pblBucket = a})

-- | Undocumented member.
pblBucketLoggingStatus :: Lens' PutBucketLogging BucketLoggingStatus
pblBucketLoggingStatus = lens _pblBucketLoggingStatus (\ s a -> s{_pblBucketLoggingStatus = a})

instance AWSRequest PutBucketLogging where
        type Rs PutBucketLogging = PutBucketLoggingResponse
        request = putXML s3
        response = receiveNull PutBucketLoggingResponse'

instance Hashable PutBucketLogging where

instance NFData PutBucketLogging where

instance ToElement PutBucketLogging where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}BucketLoggingStatus"
              .
              _pblBucketLoggingStatus

instance ToHeaders PutBucketLogging where
        toHeaders PutBucketLogging'{..}
          = mconcat ["Content-MD5" =# _pblContentMD5]

instance ToPath PutBucketLogging where
        toPath PutBucketLogging'{..}
          = mconcat ["/", toBS _pblBucket]

instance ToQuery PutBucketLogging where
        toQuery = const (mconcat ["logging"])

-- | /See:/ 'putBucketLoggingResponse' smart constructor.
data PutBucketLoggingResponse =
  PutBucketLoggingResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutBucketLoggingResponse' with the minimum fields required to make a request.
--
putBucketLoggingResponse
    :: PutBucketLoggingResponse
putBucketLoggingResponse = PutBucketLoggingResponse'


instance NFData PutBucketLoggingResponse where
