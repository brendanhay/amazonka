{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketACL
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Sets the permissions on a bucket using access control lists (ACL).
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/PutBucketACL.html>
module Network.AWS.S3.PutBucketACL
    (
    -- * Request
      PutBucketACL
    -- ** Request constructor
    , putBucketACL
    -- ** Request lenses
    , pbarqGrantReadACP
    , pbarqGrantWriteACP
    , pbarqGrantRead
    , pbarqGrantFullControl
    , pbarqContentMD5
    , pbarqAccessControlPolicy
    , pbarqGrantWrite
    , pbarqACL
    , pbarqBucket

    -- * Response
    , PutBucketACLResponse
    -- ** Response constructor
    , putBucketACLResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'putBucketACL' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pbarqGrantReadACP'
--
-- * 'pbarqGrantWriteACP'
--
-- * 'pbarqGrantRead'
--
-- * 'pbarqGrantFullControl'
--
-- * 'pbarqContentMD5'
--
-- * 'pbarqAccessControlPolicy'
--
-- * 'pbarqGrantWrite'
--
-- * 'pbarqACL'
--
-- * 'pbarqBucket'
data PutBucketACL = PutBucketACL'
    { _pbarqGrantReadACP        :: !(Maybe Text)
    , _pbarqGrantWriteACP       :: !(Maybe Text)
    , _pbarqGrantRead           :: !(Maybe Text)
    , _pbarqGrantFullControl    :: !(Maybe Text)
    , _pbarqContentMD5          :: !(Maybe Text)
    , _pbarqAccessControlPolicy :: !(Maybe AccessControlPolicy)
    , _pbarqGrantWrite          :: !(Maybe Text)
    , _pbarqACL                 :: !(Maybe BucketCannedACL)
    , _pbarqBucket              :: !BucketName
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'PutBucketACL' smart constructor.
putBucketACL :: BucketName -> PutBucketACL
putBucketACL pBucket =
    PutBucketACL'
    { _pbarqGrantReadACP = Nothing
    , _pbarqGrantWriteACP = Nothing
    , _pbarqGrantRead = Nothing
    , _pbarqGrantFullControl = Nothing
    , _pbarqContentMD5 = Nothing
    , _pbarqAccessControlPolicy = Nothing
    , _pbarqGrantWrite = Nothing
    , _pbarqACL = Nothing
    , _pbarqBucket = pBucket
    }

-- | Allows grantee to read the bucket ACL.
pbarqGrantReadACP :: Lens' PutBucketACL (Maybe Text)
pbarqGrantReadACP = lens _pbarqGrantReadACP (\ s a -> s{_pbarqGrantReadACP = a});

-- | Allows grantee to write the ACL for the applicable bucket.
pbarqGrantWriteACP :: Lens' PutBucketACL (Maybe Text)
pbarqGrantWriteACP = lens _pbarqGrantWriteACP (\ s a -> s{_pbarqGrantWriteACP = a});

-- | Allows grantee to list the objects in the bucket.
pbarqGrantRead :: Lens' PutBucketACL (Maybe Text)
pbarqGrantRead = lens _pbarqGrantRead (\ s a -> s{_pbarqGrantRead = a});

-- | Allows grantee the read, write, read ACP, and write ACP permissions on
-- the bucket.
pbarqGrantFullControl :: Lens' PutBucketACL (Maybe Text)
pbarqGrantFullControl = lens _pbarqGrantFullControl (\ s a -> s{_pbarqGrantFullControl = a});

-- | FIXME: Undocumented member.
pbarqContentMD5 :: Lens' PutBucketACL (Maybe Text)
pbarqContentMD5 = lens _pbarqContentMD5 (\ s a -> s{_pbarqContentMD5 = a});

-- | FIXME: Undocumented member.
pbarqAccessControlPolicy :: Lens' PutBucketACL (Maybe AccessControlPolicy)
pbarqAccessControlPolicy = lens _pbarqAccessControlPolicy (\ s a -> s{_pbarqAccessControlPolicy = a});

-- | Allows grantee to create, overwrite, and delete any object in the
-- bucket.
pbarqGrantWrite :: Lens' PutBucketACL (Maybe Text)
pbarqGrantWrite = lens _pbarqGrantWrite (\ s a -> s{_pbarqGrantWrite = a});

-- | The canned ACL to apply to the bucket.
pbarqACL :: Lens' PutBucketACL (Maybe BucketCannedACL)
pbarqACL = lens _pbarqACL (\ s a -> s{_pbarqACL = a});

-- | FIXME: Undocumented member.
pbarqBucket :: Lens' PutBucketACL BucketName
pbarqBucket = lens _pbarqBucket (\ s a -> s{_pbarqBucket = a});

instance AWSRequest PutBucketACL where
        type Sv PutBucketACL = S3
        type Rs PutBucketACL = PutBucketACLResponse
        request = putXML
        response = receiveNull PutBucketACLResponse'

instance ToElement PutBucketACL where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}AccessControlPolicy"
              .
              _pbarqAccessControlPolicy

instance ToHeaders PutBucketACL where
        toHeaders PutBucketACL'{..}
          = mconcat
              ["x-amz-grant-read-acp" =# _pbarqGrantReadACP,
               "x-amz-grant-write-acp" =# _pbarqGrantWriteACP,
               "x-amz-grant-read" =# _pbarqGrantRead,
               "x-amz-grant-full-control" =# _pbarqGrantFullControl,
               "Content-MD5" =# _pbarqContentMD5,
               "x-amz-grant-write" =# _pbarqGrantWrite,
               "x-amz-acl" =# _pbarqACL]

instance ToPath PutBucketACL where
        toPath PutBucketACL'{..}
          = mconcat ["/", toText _pbarqBucket]

instance ToQuery PutBucketACL where
        toQuery = const (mconcat ["acl"])

-- | /See:/ 'putBucketACLResponse' smart constructor.
data PutBucketACLResponse =
    PutBucketACLResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutBucketACLResponse' smart constructor.
putBucketACLResponse :: PutBucketACLResponse
putBucketACLResponse = PutBucketACLResponse'
