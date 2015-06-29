{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.S3.PutObjectACL
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

-- | uses the acl subresource to set the access control list (ACL)
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
    , poaGrantReadACP
    , poaRequestPayer
    , poaGrantWriteACP
    , poaGrantRead
    , poaGrantFullControl
    , poaContentMD5
    , poaAccessControlPolicy
    , poaGrantWrite
    , poaACL
    , poaBucket
    , poaKey

    -- * Response
    , PutObjectACLResponse
    -- ** Response constructor
    , putObjectACLResponse
    -- ** Response lenses
    , poarRequestCharged
    , poarStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'putObjectACL' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'poaGrantReadACP'
--
-- * 'poaRequestPayer'
--
-- * 'poaGrantWriteACP'
--
-- * 'poaGrantRead'
--
-- * 'poaGrantFullControl'
--
-- * 'poaContentMD5'
--
-- * 'poaAccessControlPolicy'
--
-- * 'poaGrantWrite'
--
-- * 'poaACL'
--
-- * 'poaBucket'
--
-- * 'poaKey'
data PutObjectACL = PutObjectACL'
    { _poaGrantReadACP        :: !(Maybe Text)
    , _poaRequestPayer        :: !(Maybe RequestPayer)
    , _poaGrantWriteACP       :: !(Maybe Text)
    , _poaGrantRead           :: !(Maybe Text)
    , _poaGrantFullControl    :: !(Maybe Text)
    , _poaContentMD5          :: !(Maybe Text)
    , _poaAccessControlPolicy :: !(Maybe AccessControlPolicy)
    , _poaGrantWrite          :: !(Maybe Text)
    , _poaACL                 :: !(Maybe ObjectCannedACL)
    , _poaBucket              :: !BucketName
    , _poaKey                 :: !ObjectKey
    } deriving (Eq,Show)

-- | 'PutObjectACL' smart constructor.
putObjectACL :: BucketName -> ObjectKey -> PutObjectACL
putObjectACL pBucket pKey =
    PutObjectACL'
    { _poaGrantReadACP = Nothing
    , _poaRequestPayer = Nothing
    , _poaGrantWriteACP = Nothing
    , _poaGrantRead = Nothing
    , _poaGrantFullControl = Nothing
    , _poaContentMD5 = Nothing
    , _poaAccessControlPolicy = Nothing
    , _poaGrantWrite = Nothing
    , _poaACL = Nothing
    , _poaBucket = pBucket
    , _poaKey = pKey
    }

-- | Allows grantee to read the bucket ACL.
poaGrantReadACP :: Lens' PutObjectACL (Maybe Text)
poaGrantReadACP = lens _poaGrantReadACP (\ s a -> s{_poaGrantReadACP = a});

-- | FIXME: Undocumented member.
poaRequestPayer :: Lens' PutObjectACL (Maybe RequestPayer)
poaRequestPayer = lens _poaRequestPayer (\ s a -> s{_poaRequestPayer = a});

-- | Allows grantee to write the ACL for the applicable bucket.
poaGrantWriteACP :: Lens' PutObjectACL (Maybe Text)
poaGrantWriteACP = lens _poaGrantWriteACP (\ s a -> s{_poaGrantWriteACP = a});

-- | Allows grantee to list the objects in the bucket.
poaGrantRead :: Lens' PutObjectACL (Maybe Text)
poaGrantRead = lens _poaGrantRead (\ s a -> s{_poaGrantRead = a});

-- | Allows grantee the read, write, read ACP, and write ACP permissions on
-- the bucket.
poaGrantFullControl :: Lens' PutObjectACL (Maybe Text)
poaGrantFullControl = lens _poaGrantFullControl (\ s a -> s{_poaGrantFullControl = a});

-- | FIXME: Undocumented member.
poaContentMD5 :: Lens' PutObjectACL (Maybe Text)
poaContentMD5 = lens _poaContentMD5 (\ s a -> s{_poaContentMD5 = a});

-- | FIXME: Undocumented member.
poaAccessControlPolicy :: Lens' PutObjectACL (Maybe AccessControlPolicy)
poaAccessControlPolicy = lens _poaAccessControlPolicy (\ s a -> s{_poaAccessControlPolicy = a});

-- | Allows grantee to create, overwrite, and delete any object in the
-- bucket.
poaGrantWrite :: Lens' PutObjectACL (Maybe Text)
poaGrantWrite = lens _poaGrantWrite (\ s a -> s{_poaGrantWrite = a});

-- | The canned ACL to apply to the object.
poaACL :: Lens' PutObjectACL (Maybe ObjectCannedACL)
poaACL = lens _poaACL (\ s a -> s{_poaACL = a});

-- | FIXME: Undocumented member.
poaBucket :: Lens' PutObjectACL BucketName
poaBucket = lens _poaBucket (\ s a -> s{_poaBucket = a});

-- | FIXME: Undocumented member.
poaKey :: Lens' PutObjectACL ObjectKey
poaKey = lens _poaKey (\ s a -> s{_poaKey = a});

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
              _poaAccessControlPolicy

instance ToHeaders PutObjectACL where
        toHeaders PutObjectACL'{..}
          = mconcat
              ["x-amz-grant-read-acp" =# _poaGrantReadACP,
               "x-amz-request-payer" =# _poaRequestPayer,
               "x-amz-grant-write-acp" =# _poaGrantWriteACP,
               "x-amz-grant-read" =# _poaGrantRead,
               "x-amz-grant-full-control" =# _poaGrantFullControl,
               "Content-MD5" =# _poaContentMD5,
               "x-amz-grant-write" =# _poaGrantWrite,
               "x-amz-acl" =# _poaACL]

instance ToPath PutObjectACL where
        toPath PutObjectACL'{..}
          = mconcat
              ["/", toText _poaBucket, "/", toText _poaKey]

instance ToQuery PutObjectACL where
        toQuery = const (mconcat ["acl"])

-- | /See:/ 'putObjectACLResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'poarRequestCharged'
--
-- * 'poarStatus'
data PutObjectACLResponse = PutObjectACLResponse'
    { _poarRequestCharged :: !(Maybe RequestCharged)
    , _poarStatus         :: !Int
    } deriving (Eq,Read,Show)

-- | 'PutObjectACLResponse' smart constructor.
putObjectACLResponse :: Int -> PutObjectACLResponse
putObjectACLResponse pStatus =
    PutObjectACLResponse'
    { _poarRequestCharged = Nothing
    , _poarStatus = pStatus
    }

-- | FIXME: Undocumented member.
poarRequestCharged :: Lens' PutObjectACLResponse (Maybe RequestCharged)
poarRequestCharged = lens _poarRequestCharged (\ s a -> s{_poarRequestCharged = a});

-- | FIXME: Undocumented member.
poarStatus :: Lens' PutObjectACLResponse Int
poarStatus = lens _poarStatus (\ s a -> s{_poarStatus = a});
