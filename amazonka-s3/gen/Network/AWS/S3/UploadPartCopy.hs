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
-- Module      : Network.AWS.S3.UploadPartCopy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads a part by copying data from an existing object as data source.
--
--
module Network.AWS.S3.UploadPartCopy
    (
    -- * Creating a Request
      uploadPartCopy
    , UploadPartCopy
    -- * Request Lenses
    , upcCopySourceIfModifiedSince
    , upcCopySourceIfUnmodifiedSince
    , upcCopySourceRange
    , upcCopySourceSSECustomerKeyMD5
    , upcCopySourceIfNoneMatch
    , upcSSECustomerAlgorithm
    , upcSSECustomerKey
    , upcRequestPayer
    , upcCopySourceIfMatch
    , upcSSECustomerKeyMD5
    , upcCopySourceSSECustomerKey
    , upcCopySourceSSECustomerAlgorithm
    , upcBucket
    , upcCopySource
    , upcKey
    , upcPartNumber
    , upcUploadId

    -- * Destructuring the Response
    , uploadPartCopyResponse
    , UploadPartCopyResponse
    -- * Response Lenses
    , upcrsRequestCharged
    , upcrsCopyPartResult
    , upcrsSSECustomerAlgorithm
    , upcrsCopySourceVersionId
    , upcrsSSECustomerKeyMD5
    , upcrsSSEKMSKeyId
    , upcrsServerSideEncryption
    , upcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'uploadPartCopy' smart constructor.
data UploadPartCopy = UploadPartCopy'
  { _upcCopySourceIfModifiedSince      :: !(Maybe ISO8601)
  , _upcCopySourceIfUnmodifiedSince    :: !(Maybe ISO8601)
  , _upcCopySourceRange                :: !(Maybe Text)
  , _upcCopySourceSSECustomerKeyMD5    :: !(Maybe Text)
  , _upcCopySourceIfNoneMatch          :: !(Maybe Text)
  , _upcSSECustomerAlgorithm           :: !(Maybe Text)
  , _upcSSECustomerKey                 :: !(Maybe (Sensitive Text))
  , _upcRequestPayer                   :: !(Maybe RequestPayer)
  , _upcCopySourceIfMatch              :: !(Maybe Text)
  , _upcSSECustomerKeyMD5              :: !(Maybe Text)
  , _upcCopySourceSSECustomerKey       :: !(Maybe (Sensitive Text))
  , _upcCopySourceSSECustomerAlgorithm :: !(Maybe Text)
  , _upcBucket                         :: !BucketName
  , _upcCopySource                     :: !Text
  , _upcKey                            :: !ObjectKey
  , _upcPartNumber                     :: !Int
  , _upcUploadId                       :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UploadPartCopy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upcCopySourceIfModifiedSince' - Copies the object if it has been modified since the specified time.
--
-- * 'upcCopySourceIfUnmodifiedSince' - Copies the object if it hasn't been modified since the specified time.
--
-- * 'upcCopySourceRange' - The range of bytes to copy from the source object. The range value must use the form bytes=first-last, where the first and last are the zero-based byte offsets to copy. For example, bytes=0-9 indicates that you want to copy the first ten bytes of the source. You can copy a range only if the source object is greater than 5 MB.
--
-- * 'upcCopySourceSSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure the encryption key was transmitted without error.
--
-- * 'upcCopySourceIfNoneMatch' - Copies the object if its entity tag (ETag) is different than the specified ETag.
--
-- * 'upcSSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (e.g., AES256).
--
-- * 'upcSSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon does not store the encryption key. The key must be appropriate for use with the algorithm specified in the x-amz-server-side
