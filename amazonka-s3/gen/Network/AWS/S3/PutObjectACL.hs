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
-- Module      : Network.AWS.S3.PutObjectACL
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- uses the acl subresource to set the access control list (ACL)
-- permissions for an object that already exists in a bucket
--
-- /See:/ <http://docs.aws.amazon.com/AmazonS3/latest/API/PutObjectACL.html AWS API Reference> for PutObjectACL.
module Network.AWS.S3.PutObjectACL
    (
    -- * Creating a Request
      putObjectACL
    , PutObjectACL
    -- * Request Lenses
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

    -- * Destructuring the Response
    , putObjectACLResponse
    , PutObjectACLResponse
    -- * Response Lenses
    , poarsRequestCharged
    , poarsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types
import           Network.AWS.S3.Types.Product

-- | /See:/ 'putObjectACL' smart constructor.
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutObjectACL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
putObjectACL
    :: BucketName -- ^ 'poaBucket'
    -> ObjectKey -- ^ 'poaKey'
    -> PutObjectACL
putObjectACL pBucket_ pKey_ =
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
    , _poaBucket = pBucket_
    , _poaKey = pKey_
    }

-- | Allows grantee to read the bucket ACL.
poaGrantReadACP :: Lens' PutObjectACL (Maybe Text)
poaGrantReadACP = lens _poaGrantReadACP (\ s a -> s{_poaGrantReadACP = a});

-- | Undocumented member.
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

-- | Undocumented member.
poaContentMD5 :: Lens' PutObjectACL (Maybe Text)
poaContentMD5 = lens _poaContentMD5 (\ s a -> s{_poaContentMD5 = a});

-- | Undocumented member.
poaAccessControlPolicy :: Lens' PutObjectACL (Maybe AccessControlPolicy)
poaAccessControlPolicy = lens _poaAccessControlPolicy (\ s a -> s{_poaAccessControlPolicy = a});

-- | Allows grantee to create, overwrite, and delete any object in the
-- bucket.
poaGrantWrite :: Lens' PutObjectACL (Maybe Text)
poaGrantWrite = lens _poaGrantWrite (\ s a -> s{_poaGrantWrite = a});

-- | The canned ACL to apply to the object.
poaACL :: Lens' PutObjectACL (Maybe ObjectCannedACL)
poaACL = lens _poaACL (\ s a -> s{_poaACL = a});

-- | Undocumented member.
poaBucket :: Lens' PutObjectACL BucketName
poaBucket = lens _poaBucket (\ s a -> s{_poaBucket = a});

-- | Undocumented member.
poaKey :: Lens' PutObjectACL ObjectKey
poaKey = lens _poaKey (\ s a -> s{_poaKey = a});

instance AWSRequest PutObjectACL where
        type Rs PutObjectACL = PutObjectACLResponse
        request = putXML s3
        response
          = receiveEmpty
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
          = mconcat ["/", toBS _poaBucket, "/", toBS _poaKey]

instance ToQuery PutObjectACL where
        toQuery = const (mconcat ["acl"])

-- | /See:/ 'putObjectACLResponse' smart constructor.
data PutObjectACLResponse = PutObjectACLResponse'
    { _poarsRequestCharged :: !(Maybe RequestCharged)
    , _poarsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutObjectACLResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'poarsRequestCharged'
--
-- * 'poarsStatus'
putObjectACLResponse
    :: Int -- ^ 'poarsStatus'
    -> PutObjectACLResponse
putObjectACLResponse pStatus_ =
    PutObjectACLResponse'
    { _poarsRequestCharged = Nothing
    , _poarsStatus = pStatus_
    }

-- | Undocumented member.
poarsRequestCharged :: Lens' PutObjectACLResponse (Maybe RequestCharged)
poarsRequestCharged = lens _poarsRequestCharged (\ s a -> s{_poarsRequestCharged = a});

-- | The response status code.
poarsStatus :: Lens' PutObjectACLResponse Int
poarsStatus = lens _poarsStatus (\ s a -> s{_poarsStatus = a});
