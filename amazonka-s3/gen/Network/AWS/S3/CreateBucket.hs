{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.CreateBucket
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a new bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/CreateBucket.html>
module Network.AWS.S3.CreateBucket
    (
    -- * Request
      CreateBucket
    -- ** Request constructor
    , createBucket
    -- ** Request lenses
    , cbrqGrantReadACP
    , cbrqGrantWriteACP
    , cbrqGrantRead
    , cbrqGrantFullControl
    , cbrqCreateBucketConfiguration
    , cbrqGrantWrite
    , cbrqACL
    , cbrqBucket

    -- * Response
    , CreateBucketResponse
    -- ** Response constructor
    , createBucketResponse
    -- ** Response lenses
    , cbrsLocation
    , cbrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'createBucket' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cbrqGrantReadACP'
--
-- * 'cbrqGrantWriteACP'
--
-- * 'cbrqGrantRead'
--
-- * 'cbrqGrantFullControl'
--
-- * 'cbrqCreateBucketConfiguration'
--
-- * 'cbrqGrantWrite'
--
-- * 'cbrqACL'
--
-- * 'cbrqBucket'
data CreateBucket = CreateBucket'
    { _cbrqGrantReadACP              :: !(Maybe Text)
    , _cbrqGrantWriteACP             :: !(Maybe Text)
    , _cbrqGrantRead                 :: !(Maybe Text)
    , _cbrqGrantFullControl          :: !(Maybe Text)
    , _cbrqCreateBucketConfiguration :: !(Maybe CreateBucketConfiguration)
    , _cbrqGrantWrite                :: !(Maybe Text)
    , _cbrqACL                       :: !(Maybe BucketCannedACL)
    , _cbrqBucket                    :: !BucketName
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'CreateBucket' smart constructor.
createBucket :: BucketName -> CreateBucket
createBucket pBucket =
    CreateBucket'
    { _cbrqGrantReadACP = Nothing
    , _cbrqGrantWriteACP = Nothing
    , _cbrqGrantRead = Nothing
    , _cbrqGrantFullControl = Nothing
    , _cbrqCreateBucketConfiguration = Nothing
    , _cbrqGrantWrite = Nothing
    , _cbrqACL = Nothing
    , _cbrqBucket = pBucket
    }

-- | Allows grantee to read the bucket ACL.
cbrqGrantReadACP :: Lens' CreateBucket (Maybe Text)
cbrqGrantReadACP = lens _cbrqGrantReadACP (\ s a -> s{_cbrqGrantReadACP = a});

-- | Allows grantee to write the ACL for the applicable bucket.
cbrqGrantWriteACP :: Lens' CreateBucket (Maybe Text)
cbrqGrantWriteACP = lens _cbrqGrantWriteACP (\ s a -> s{_cbrqGrantWriteACP = a});

-- | Allows grantee to list the objects in the bucket.
cbrqGrantRead :: Lens' CreateBucket (Maybe Text)
cbrqGrantRead = lens _cbrqGrantRead (\ s a -> s{_cbrqGrantRead = a});

-- | Allows grantee the read, write, read ACP, and write ACP permissions on
-- the bucket.
cbrqGrantFullControl :: Lens' CreateBucket (Maybe Text)
cbrqGrantFullControl = lens _cbrqGrantFullControl (\ s a -> s{_cbrqGrantFullControl = a});

-- | FIXME: Undocumented member.
cbrqCreateBucketConfiguration :: Lens' CreateBucket (Maybe CreateBucketConfiguration)
cbrqCreateBucketConfiguration = lens _cbrqCreateBucketConfiguration (\ s a -> s{_cbrqCreateBucketConfiguration = a});

-- | Allows grantee to create, overwrite, and delete any object in the
-- bucket.
cbrqGrantWrite :: Lens' CreateBucket (Maybe Text)
cbrqGrantWrite = lens _cbrqGrantWrite (\ s a -> s{_cbrqGrantWrite = a});

-- | The canned ACL to apply to the bucket.
cbrqACL :: Lens' CreateBucket (Maybe BucketCannedACL)
cbrqACL = lens _cbrqACL (\ s a -> s{_cbrqACL = a});

-- | FIXME: Undocumented member.
cbrqBucket :: Lens' CreateBucket BucketName
cbrqBucket = lens _cbrqBucket (\ s a -> s{_cbrqBucket = a});

instance AWSRequest CreateBucket where
        type Sv CreateBucket = S3
        type Rs CreateBucket = CreateBucketResponse
        request = putXML
        response
          = receiveXML
              (\ s h x ->
                 CreateBucketResponse' <$>
                   (h .#? "Location") <*> (pure (fromEnum s)))

instance ToElement CreateBucket where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}CreateBucketConfiguration"
              .
              _cbrqCreateBucketConfiguration

instance ToHeaders CreateBucket where
        toHeaders CreateBucket'{..}
          = mconcat
              ["x-amz-grant-read-acp" =# _cbrqGrantReadACP,
               "x-amz-grant-write-acp" =# _cbrqGrantWriteACP,
               "x-amz-grant-read" =# _cbrqGrantRead,
               "x-amz-grant-full-control" =# _cbrqGrantFullControl,
               "x-amz-grant-write" =# _cbrqGrantWrite,
               "x-amz-acl" =# _cbrqACL]

instance ToPath CreateBucket where
        toPath CreateBucket'{..}
          = mconcat ["/", toText _cbrqBucket]

instance ToQuery CreateBucket where
        toQuery = const mempty

-- | /See:/ 'createBucketResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cbrsLocation'
--
-- * 'cbrsStatus'
data CreateBucketResponse = CreateBucketResponse'
    { _cbrsLocation :: !(Maybe Text)
    , _cbrsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateBucketResponse' smart constructor.
createBucketResponse :: Int -> CreateBucketResponse
createBucketResponse pStatus =
    CreateBucketResponse'
    { _cbrsLocation = Nothing
    , _cbrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
cbrsLocation :: Lens' CreateBucketResponse (Maybe Text)
cbrsLocation = lens _cbrsLocation (\ s a -> s{_cbrsLocation = a});

-- | FIXME: Undocumented member.
cbrsStatus :: Lens' CreateBucketResponse Int
cbrsStatus = lens _cbrsStatus (\ s a -> s{_cbrsStatus = a});
