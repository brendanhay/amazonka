{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.S3.PutBucketVersioning
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

-- | Sets the versioning state of an existing bucket. To set the versioning
-- state, you must be the bucket owner.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/PutBucketVersioning.html>
module Network.AWS.S3.PutBucketVersioning
    (
    -- * Request
      PutBucketVersioning
    -- ** Request constructor
    , putBucketVersioning
    -- ** Request lenses
    , pbvMFA
    , pbvContentMD5
    , pbvBucket
    , pbvVersioningConfiguration

    -- * Response
    , PutBucketVersioningResponse
    -- ** Response constructor
    , putBucketVersioningResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'putBucketVersioning' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pbvMFA'
--
-- * 'pbvContentMD5'
--
-- * 'pbvBucket'
--
-- * 'pbvVersioningConfiguration'
data PutBucketVersioning = PutBucketVersioning'
    { _pbvMFA                     :: !(Maybe Text)
    , _pbvContentMD5              :: !(Maybe Text)
    , _pbvBucket                  :: !BucketName
    , _pbvVersioningConfiguration :: !VersioningConfiguration
    } deriving (Eq,Show)

-- | 'PutBucketVersioning' smart constructor.
putBucketVersioning :: BucketName -> VersioningConfiguration -> PutBucketVersioning
putBucketVersioning pBucket pVersioningConfiguration =
    PutBucketVersioning'
    { _pbvMFA = Nothing
    , _pbvContentMD5 = Nothing
    , _pbvBucket = pBucket
    , _pbvVersioningConfiguration = pVersioningConfiguration
    }

-- | The concatenation of the authentication device\'s serial number, a
-- space, and the value that is displayed on your authentication device.
pbvMFA :: Lens' PutBucketVersioning (Maybe Text)
pbvMFA = lens _pbvMFA (\ s a -> s{_pbvMFA = a});

-- | FIXME: Undocumented member.
pbvContentMD5 :: Lens' PutBucketVersioning (Maybe Text)
pbvContentMD5 = lens _pbvContentMD5 (\ s a -> s{_pbvContentMD5 = a});

-- | FIXME: Undocumented member.
pbvBucket :: Lens' PutBucketVersioning BucketName
pbvBucket = lens _pbvBucket (\ s a -> s{_pbvBucket = a});

-- | FIXME: Undocumented member.
pbvVersioningConfiguration :: Lens' PutBucketVersioning VersioningConfiguration
pbvVersioningConfiguration = lens _pbvVersioningConfiguration (\ s a -> s{_pbvVersioningConfiguration = a});

instance AWSRequest PutBucketVersioning where
        type Sv PutBucketVersioning = S3
        type Rs PutBucketVersioning =
             PutBucketVersioningResponse
        request = putXML
        response = receiveNull PutBucketVersioningResponse'

instance ToElement PutBucketVersioning where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}VersioningConfiguration"
              .
              _pbvVersioningConfiguration

instance ToHeaders PutBucketVersioning where
        toHeaders PutBucketVersioning'{..}
          = mconcat
              ["x-amz-mfa" =# _pbvMFA,
               "Content-MD5" =# _pbvContentMD5]

instance ToPath PutBucketVersioning where
        toPath PutBucketVersioning'{..}
          = mconcat ["/", toText _pbvBucket]

instance ToQuery PutBucketVersioning where
        toQuery = const (mconcat ["versioning"])

-- | /See:/ 'putBucketVersioningResponse' smart constructor.
data PutBucketVersioningResponse =
    PutBucketVersioningResponse'
    deriving (Eq,Read,Show)

-- | 'PutBucketVersioningResponse' smart constructor.
putBucketVersioningResponse :: PutBucketVersioningResponse
putBucketVersioningResponse = PutBucketVersioningResponse'
