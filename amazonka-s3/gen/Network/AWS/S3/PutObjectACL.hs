{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutObjectACL
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- uses the acl subresource to set the access control list (ACL)
-- permissions for an object that already exists in a bucket
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/PutObjectACL.html>
module Network.AWS.S3.PutObjectACL
    (
    -- * Request
      PutObjectACL
    -- ** Request constructor
    , putObjectACL
    -- ** Request lenses
    , poarqGrantReadACP
    , poarqRequestPayer
    , poarqGrantWriteACP
    , poarqGrantRead
    , poarqGrantFullControl
    , poarqContentMD5
    , poarqAccessControlPolicy
    , poarqGrantWrite
    , poarqACL
    , poarqBucket
    , poarqKey

    -- * Response
    , PutObjectACLResponse
    -- ** Response constructor
    , putObjectACLResponse
    -- ** Response lenses
    , poarsRequestCharged
    , poarsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'putObjectACL' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'poarqGrantReadACP'
--
-- * 'poarqRequestPayer'
--
-- * 'poarqGrantWriteACP'
--
-- * 'poarqGrantRead'
--
-- * 'poarqGrantFullControl'
--
-- * 'poarqContentMD5'
--
-- * 'poarqAccessControlPolicy'
--
-- * 'poarqGrantWrite'
--
-- * 'poarqACL'
--
-- * 'poarqBucket'
--
-- * 'poarqKey'
data PutObjectACL = PutObjectACL'
    { _poarqGrantReadACP        :: !(Maybe Text)
    , _poarqRequestPayer        :: !(Maybe RequestPayer)
    , _poarqGrantWriteACP       :: !(Maybe Text)
    , _poarqGrantRead           :: !(Maybe Text)
    , _poarqGrantFullControl    :: !(Maybe Text)
    , _poarqContentMD5          :: !(Maybe Text)
    , _poarqAccessControlPolicy :: !(Maybe AccessControlPolicy)
    , _poarqGrantWrite          :: !(Maybe Text)
    , _poarqACL                 :: !(Maybe ObjectCannedACL)
    , _poarqBucket              :: !BucketName
    , _poarqKey                 :: !ObjectKey
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'PutObjectACL' smart constructor.
putObjectACL :: BucketName -> ObjectKey -> PutObjectACL
putObjectACL pBucket_ pKey_ =
    PutObjectACL'
    { _poarqGrantReadACP = Nothing
    , _poarqRequestPayer = Nothing
    , _poarqGrantWriteACP = Nothing
    , _poarqGrantRead = Nothing
    , _poarqGrantFullControl = Nothing
    , _poarqContentMD5 = Nothing
    , _poarqAccessControlPolicy = Nothing
    , _poarqGrantWrite = Nothing
    , _poarqACL = Nothing
    , _poarqBucket = pBucket_
    , _poarqKey = pKey_
    }

-- | Allows grantee to read the bucket ACL.
poarqGrantReadACP :: Lens' PutObjectACL (Maybe Text)
poarqGrantReadACP = lens _poarqGrantReadACP (\ s a -> s{_poarqGrantReadACP = a});

-- | FIXME: Undocumented member.
poarqRequestPayer :: Lens' PutObjectACL (Maybe RequestPayer)
poarqRequestPayer = lens _poarqRequestPayer (\ s a -> s{_poarqRequestPayer = a});

-- | Allows grantee to write the ACL for the applicable bucket.
poarqGrantWriteACP :: Lens' PutObjectACL (Maybe Text)
poarqGrantWriteACP = lens _poarqGrantWriteACP (\ s a -> s{_poarqGrantWriteACP = a});

-- | Allows grantee to list the objects in the bucket.
poarqGrantRead :: Lens' PutObjectACL (Maybe Text)
poarqGrantRead = lens _poarqGrantRead (\ s a -> s{_poarqGrantRead = a});

-- | Allows grantee the read, write, read ACP, and write ACP permissions on
-- the bucket.
poarqGrantFullControl :: Lens' PutObjectACL (Maybe Text)
poarqGrantFullControl = lens _poarqGrantFullControl (\ s a -> s{_poarqGrantFullControl = a});

-- | FIXME: Undocumented member.
poarqContentMD5 :: Lens' PutObjectACL (Maybe Text)
poarqContentMD5 = lens _poarqContentMD5 (\ s a -> s{_poarqContentMD5 = a});

-- | FIXME: Undocumented member.
poarqAccessControlPolicy :: Lens' PutObjectACL (Maybe AccessControlPolicy)
poarqAccessControlPolicy = lens _poarqAccessControlPolicy (\ s a -> s{_poarqAccessControlPolicy = a});

-- | Allows grantee to create, overwrite, and delete any object in the
-- bucket.
poarqGrantWrite :: Lens' PutObjectACL (Maybe Text)
poarqGrantWrite = lens _poarqGrantWrite (\ s a -> s{_poarqGrantWrite = a});

-- | The canned ACL to apply to the object.
poarqACL :: Lens' PutObjectACL (Maybe ObjectCannedACL)
poarqACL = lens _poarqACL (\ s a -> s{_poarqACL = a});

-- | FIXME: Undocumented member.
poarqBucket :: Lens' PutObjectACL BucketName
poarqBucket = lens _poarqBucket (\ s a -> s{_poarqBucket = a});

-- | FIXME: Undocumented member.
poarqKey :: Lens' PutObjectACL ObjectKey
poarqKey = lens _poarqKey (\ s a -> s{_poarqKey = a});

instance AWSRequest PutObjectACL where
        type Sv PutObjectACL = S3
        type Rs PutObjectACL = PutObjectACLResponse
        request = putXML
        response
          = receiveXML
              (\ s h x ->
                 PutObjectACLResponse' <$>
                   (h .#? "x-amz-request-charged") <*>
                     (pure (fromEnum s)))

instance ToElement PutObjectACL where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}AccessControlPolicy"
              .
              _poarqAccessControlPolicy

instance ToHeaders PutObjectACL where
        toHeaders PutObjectACL'{..}
          = mconcat
              ["x-amz-grant-read-acp" =# _poarqGrantReadACP,
               "x-amz-request-payer" =# _poarqRequestPayer,
               "x-amz-grant-write-acp" =# _poarqGrantWriteACP,
               "x-amz-grant-read" =# _poarqGrantRead,
               "x-amz-grant-full-control" =# _poarqGrantFullControl,
               "Content-MD5" =# _poarqContentMD5,
               "x-amz-grant-write" =# _poarqGrantWrite,
               "x-amz-acl" =# _poarqACL]

instance ToPath PutObjectACL where
        toPath PutObjectACL'{..}
          = mconcat
              ["/", toText _poarqBucket, "/", toText _poarqKey]

instance ToQuery PutObjectACL where
        toQuery = const (mconcat ["acl"])

-- | /See:/ 'putObjectACLResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'poarsRequestCharged'
--
-- * 'poarsStatus'
data PutObjectACLResponse = PutObjectACLResponse'
    { _poarsRequestCharged :: !(Maybe RequestCharged)
    , _poarsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutObjectACLResponse' smart constructor.
putObjectACLResponse :: Int -> PutObjectACLResponse
putObjectACLResponse pStatus_ =
    PutObjectACLResponse'
    { _poarsRequestCharged = Nothing
    , _poarsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
poarsRequestCharged :: Lens' PutObjectACLResponse (Maybe RequestCharged)
poarsRequestCharged = lens _poarsRequestCharged (\ s a -> s{_poarsRequestCharged = a});

-- | FIXME: Undocumented member.
poarsStatus :: Lens' PutObjectACLResponse Int
poarsStatus = lens _poarsStatus (\ s a -> s{_poarsStatus = a});
