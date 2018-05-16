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
-- Module      : Network.AWS.S3.CreateBucket
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new bucket.
module Network.AWS.S3.CreateBucket
    (
    -- * Creating a Request
      createBucket
    , CreateBucket
    -- * Request Lenses
    , cbGrantReadACP
    , cbGrantWriteACP
    , cbGrantRead
    , cbGrantFullControl
    , cbCreateBucketConfiguration
    , cbGrantWrite
    , cbACL
    , cbBucket

    -- * Destructuring the Response
    , createBucketResponse
    , CreateBucketResponse
    -- * Response Lenses
    , cbrsLocation
    , cbrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'createBucket' smart constructor.
data CreateBucket = CreateBucket'
  { _cbGrantReadACP              :: !(Maybe Text)
  , _cbGrantWriteACP             :: !(Maybe Text)
  , _cbGrantRead                 :: !(Maybe Text)
  , _cbGrantFullControl          :: !(Maybe Text)
  , _cbCreateBucketConfiguration :: !(Maybe CreateBucketConfiguration)
  , _cbGrantWrite                :: !(Maybe Text)
  , _cbACL                       :: !(Maybe BucketCannedACL)
  , _cbBucket                    :: !BucketName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateBucket' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbGrantReadACP' - Allows grantee to read the bucket ACL.
--
-- * 'cbGrantWriteACP' - Allows grantee to write the ACL for the applicable bucket.
--
-- * 'cbGrantRead' - Allows grantee to list the objects in the bucket.
--
-- * 'cbGrantFullControl' - Allows grantee the read, write, read ACP, and write ACP permissions on the bucket.
--
-- * 'cbCreateBucketConfiguration' - Undocumented member.
--
-- * 'cbGrantWrite' - Allows grantee to create, overwrite, and delete any object in the bucket.
--
-- * 'cbACL' - The canned ACL to apply to the bucket.
--
-- * 'cbBucket' - Undocumented member.
createBucket
    :: BucketName -- ^ 'cbBucket'
    -> CreateBucket
createBucket pBucket_ =
  CreateBucket'
    { _cbGrantReadACP = Nothing
    , _cbGrantWriteACP = Nothing
    , _cbGrantRead = Nothing
    , _cbGrantFullControl = Nothing
    , _cbCreateBucketConfiguration = Nothing
    , _cbGrantWrite = Nothing
    , _cbACL = Nothing
    , _cbBucket = pBucket_
    }


-- | Allows grantee to read the bucket ACL.
cbGrantReadACP :: Lens' CreateBucket (Maybe Text)
cbGrantReadACP = lens _cbGrantReadACP (\ s a -> s{_cbGrantReadACP = a})

-- | Allows grantee to write the ACL for the applicable bucket.
cbGrantWriteACP :: Lens' CreateBucket (Maybe Text)
cbGrantWriteACP = lens _cbGrantWriteACP (\ s a -> s{_cbGrantWriteACP = a})

-- | Allows grantee to list the objects in the bucket.
cbGrantRead :: Lens' CreateBucket (Maybe Text)
cbGrantRead = lens _cbGrantRead (\ s a -> s{_cbGrantRead = a})

-- | Allows grantee the read, write, read ACP, and write ACP permissions on the bucket.
cbGrantFullControl :: Lens' CreateBucket (Maybe Text)
cbGrantFullControl = lens _cbGrantFullControl (\ s a -> s{_cbGrantFullControl = a})

-- | Undocumented member.
cbCreateBucketConfiguration :: Lens' CreateBucket (Maybe CreateBucketConfiguration)
cbCreateBucketConfiguration = lens _cbCreateBucketConfiguration (\ s a -> s{_cbCreateBucketConfiguration = a})

-- | Allows grantee to create, overwrite, and delete any object in the bucket.
cbGrantWrite :: Lens' CreateBucket (Maybe Text)
cbGrantWrite = lens _cbGrantWrite (\ s a -> s{_cbGrantWrite = a})

-- | The canned ACL to apply to the bucket.
cbACL :: Lens' CreateBucket (Maybe BucketCannedACL)
cbACL = lens _cbACL (\ s a -> s{_cbACL = a})

-- | Undocumented member.
cbBucket :: Lens' CreateBucket BucketName
cbBucket = lens _cbBucket (\ s a -> s{_cbBucket = a})

instance AWSRequest CreateBucket where
        type Rs CreateBucket = CreateBucketResponse
        request = putXML s3
        response
          = receiveEmpty
              (\ s h x ->
                 CreateBucketResponse' <$>
                   (h .#? "Location") <*> (pure (fromEnum s)))

instance Hashable CreateBucket where

instance NFData CreateBucket where

instance ToElement CreateBucket where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}CreateBucketConfiguration"
              .
              _cbCreateBucketConfiguration

instance ToHeaders CreateBucket where
        toHeaders CreateBucket'{..}
          = mconcat
              ["x-amz-grant-read-acp" =# _cbGrantReadACP,
               "x-amz-grant-write-acp" =# _cbGrantWriteACP,
               "x-amz-grant-read" =# _cbGrantRead,
               "x-amz-grant-full-control" =# _cbGrantFullControl,
               "x-amz-grant-write" =# _cbGrantWrite,
               "x-amz-acl" =# _cbACL]

instance ToPath CreateBucket where
        toPath CreateBucket'{..}
          = mconcat ["/", toBS _cbBucket]

instance ToQuery CreateBucket where
        toQuery = const mempty

-- | /See:/ 'createBucketResponse' smart constructor.
data CreateBucketResponse = CreateBucketResponse'
  { _cbrsLocation       :: !(Maybe Text)
  , _cbrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateBucketResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbrsLocation' - Undocumented member.
--
-- * 'cbrsResponseStatus' - -- | The response status code.
createBucketResponse
    :: Int -- ^ 'cbrsResponseStatus'
    -> CreateBucketResponse
createBucketResponse pResponseStatus_ =
  CreateBucketResponse'
    {_cbrsLocation = Nothing, _cbrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
cbrsLocation :: Lens' CreateBucketResponse (Maybe Text)
cbrsLocation = lens _cbrsLocation (\ s a -> s{_cbrsLocation = a})

-- | -- | The response status code.
cbrsResponseStatus :: Lens' CreateBucketResponse Int
cbrsResponseStatus = lens _cbrsResponseStatus (\ s a -> s{_cbrsResponseStatus = a})

instance NFData CreateBucketResponse where
