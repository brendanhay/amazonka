{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketVersioning
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Sets the versioning state of an existing bucket. To set the versioning
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
    , pbvrqMFA
    , pbvrqContentMD5
    , pbvrqBucket
    , pbvrqVersioningConfiguration

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
-- * 'pbvrqMFA'
--
-- * 'pbvrqContentMD5'
--
-- * 'pbvrqBucket'
--
-- * 'pbvrqVersioningConfiguration'
data PutBucketVersioning = PutBucketVersioning'
    { _pbvrqMFA                     :: !(Maybe Text)
    , _pbvrqContentMD5              :: !(Maybe Text)
    , _pbvrqBucket                  :: !BucketName
    , _pbvrqVersioningConfiguration :: !VersioningConfiguration
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'PutBucketVersioning' smart constructor.
putBucketVersioning :: BucketName -> VersioningConfiguration -> PutBucketVersioning
putBucketVersioning pBucket pVersioningConfiguration =
    PutBucketVersioning'
    { _pbvrqMFA = Nothing
    , _pbvrqContentMD5 = Nothing
    , _pbvrqBucket = pBucket
    , _pbvrqVersioningConfiguration = pVersioningConfiguration
    }

-- | The concatenation of the authentication device\'s serial number, a
-- space, and the value that is displayed on your authentication device.
pbvrqMFA :: Lens' PutBucketVersioning (Maybe Text)
pbvrqMFA = lens _pbvrqMFA (\ s a -> s{_pbvrqMFA = a});

-- | FIXME: Undocumented member.
pbvrqContentMD5 :: Lens' PutBucketVersioning (Maybe Text)
pbvrqContentMD5 = lens _pbvrqContentMD5 (\ s a -> s{_pbvrqContentMD5 = a});

-- | FIXME: Undocumented member.
pbvrqBucket :: Lens' PutBucketVersioning BucketName
pbvrqBucket = lens _pbvrqBucket (\ s a -> s{_pbvrqBucket = a});

-- | FIXME: Undocumented member.
pbvrqVersioningConfiguration :: Lens' PutBucketVersioning VersioningConfiguration
pbvrqVersioningConfiguration = lens _pbvrqVersioningConfiguration (\ s a -> s{_pbvrqVersioningConfiguration = a});

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
              _pbvrqVersioningConfiguration

instance ToHeaders PutBucketVersioning where
        toHeaders PutBucketVersioning'{..}
          = mconcat
              ["x-amz-mfa" =# _pbvrqMFA,
               "Content-MD5" =# _pbvrqContentMD5]

instance ToPath PutBucketVersioning where
        toPath PutBucketVersioning'{..}
          = mconcat ["/", toText _pbvrqBucket]

instance ToQuery PutBucketVersioning where
        toQuery = const (mconcat ["versioning"])

-- | /See:/ 'putBucketVersioningResponse' smart constructor.
data PutBucketVersioningResponse =
    PutBucketVersioningResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutBucketVersioningResponse' smart constructor.
putBucketVersioningResponse :: PutBucketVersioningResponse
putBucketVersioningResponse = PutBucketVersioningResponse'
