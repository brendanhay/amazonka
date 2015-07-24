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
    , pbaGrantReadACP
    , pbaGrantWriteACP
    , pbaGrantRead
    , pbaGrantFullControl
    , pbaContentMD5
    , pbaAccessControlPolicy
    , pbaGrantWrite
    , pbaACL
    , pbaBucket

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
-- * 'pbaGrantReadACP'
--
-- * 'pbaGrantWriteACP'
--
-- * 'pbaGrantRead'
--
-- * 'pbaGrantFullControl'
--
-- * 'pbaContentMD5'
--
-- * 'pbaAccessControlPolicy'
--
-- * 'pbaGrantWrite'
--
-- * 'pbaACL'
--
-- * 'pbaBucket'
data PutBucketACL = PutBucketACL'
    { _pbaGrantReadACP        :: !(Maybe Text)
    , _pbaGrantWriteACP       :: !(Maybe Text)
    , _pbaGrantRead           :: !(Maybe Text)
    , _pbaGrantFullControl    :: !(Maybe Text)
    , _pbaContentMD5          :: !(Maybe Text)
    , _pbaAccessControlPolicy :: !(Maybe AccessControlPolicy)
    , _pbaGrantWrite          :: !(Maybe Text)
    , _pbaACL                 :: !(Maybe BucketCannedACL)
    , _pbaBucket              :: !BucketName
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'PutBucketACL' smart constructor.
putBucketACL :: BucketName -> PutBucketACL
putBucketACL pBucket_ =
    PutBucketACL'
    { _pbaGrantReadACP = Nothing
    , _pbaGrantWriteACP = Nothing
    , _pbaGrantRead = Nothing
    , _pbaGrantFullControl = Nothing
    , _pbaContentMD5 = Nothing
    , _pbaAccessControlPolicy = Nothing
    , _pbaGrantWrite = Nothing
    , _pbaACL = Nothing
    , _pbaBucket = pBucket_
    }

-- | Allows grantee to read the bucket ACL.
pbaGrantReadACP :: Lens' PutBucketACL (Maybe Text)
pbaGrantReadACP = lens _pbaGrantReadACP (\ s a -> s{_pbaGrantReadACP = a});

-- | Allows grantee to write the ACL for the applicable bucket.
pbaGrantWriteACP :: Lens' PutBucketACL (Maybe Text)
pbaGrantWriteACP = lens _pbaGrantWriteACP (\ s a -> s{_pbaGrantWriteACP = a});

-- | Allows grantee to list the objects in the bucket.
pbaGrantRead :: Lens' PutBucketACL (Maybe Text)
pbaGrantRead = lens _pbaGrantRead (\ s a -> s{_pbaGrantRead = a});

-- | Allows grantee the read, write, read ACP, and write ACP permissions on
-- the bucket.
pbaGrantFullControl :: Lens' PutBucketACL (Maybe Text)
pbaGrantFullControl = lens _pbaGrantFullControl (\ s a -> s{_pbaGrantFullControl = a});

-- | FIXME: Undocumented member.
pbaContentMD5 :: Lens' PutBucketACL (Maybe Text)
pbaContentMD5 = lens _pbaContentMD5 (\ s a -> s{_pbaContentMD5 = a});

-- | FIXME: Undocumented member.
pbaAccessControlPolicy :: Lens' PutBucketACL (Maybe AccessControlPolicy)
pbaAccessControlPolicy = lens _pbaAccessControlPolicy (\ s a -> s{_pbaAccessControlPolicy = a});

-- | Allows grantee to create, overwrite, and delete any object in the
-- bucket.
pbaGrantWrite :: Lens' PutBucketACL (Maybe Text)
pbaGrantWrite = lens _pbaGrantWrite (\ s a -> s{_pbaGrantWrite = a});

-- | The canned ACL to apply to the bucket.
pbaACL :: Lens' PutBucketACL (Maybe BucketCannedACL)
pbaACL = lens _pbaACL (\ s a -> s{_pbaACL = a});

-- | FIXME: Undocumented member.
pbaBucket :: Lens' PutBucketACL BucketName
pbaBucket = lens _pbaBucket (\ s a -> s{_pbaBucket = a});

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
              _pbaAccessControlPolicy

instance ToHeaders PutBucketACL where
        toHeaders PutBucketACL'{..}
          = mconcat
              ["x-amz-grant-read-acp" =# _pbaGrantReadACP,
               "x-amz-grant-write-acp" =# _pbaGrantWriteACP,
               "x-amz-grant-read" =# _pbaGrantRead,
               "x-amz-grant-full-control" =# _pbaGrantFullControl,
               "Content-MD5" =# _pbaContentMD5,
               "x-amz-grant-write" =# _pbaGrantWrite,
               "x-amz-acl" =# _pbaACL]

instance ToPath PutBucketACL where
        toPath PutBucketACL'{..}
          = mconcat ["/", toText _pbaBucket]

instance ToQuery PutBucketACL where
        toQuery = const (mconcat ["acl"])

-- | /See:/ 'putBucketACLResponse' smart constructor.
data PutBucketACLResponse =
    PutBucketACLResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutBucketACLResponse' smart constructor.
putBucketACLResponse :: PutBucketACLResponse
putBucketACLResponse = PutBucketACLResponse'
