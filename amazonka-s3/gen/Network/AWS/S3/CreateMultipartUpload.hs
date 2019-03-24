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
-- Module      : Network.AWS.S3.CreateMultipartUpload
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a multipart upload and returns an upload ID.
--
--
-- __Note:__ After you initiate multipart upload and upload one or more parts, you must either complete or abort multipart upload in order to stop getting charged for storage of the uploaded parts. Only after you either complete or abort multipart upload, Amazon S3 frees up the parts storage and stops charging you for the parts storage.
--
module Network.AWS.S3.CreateMultipartUpload
    (
    -- * Creating a Request
      createMultipartUpload
    , CreateMultipartUpload
    -- * Request Lenses
    , cmuObjectLockMode
    , cmuExpires
    , cmuGrantReadACP
    , cmuSSECustomerAlgorithm
    , cmuSSECustomerKey
    , cmuRequestPayer
    , cmuGrantWriteACP
    , cmuWebsiteRedirectLocation
    , cmuGrantRead
    , cmuStorageClass
    , cmuSSECustomerKeyMD5
    , cmuSSEKMSKeyId
    , cmuGrantFullControl
    , cmuContentEncoding
    , cmuTagging
    , cmuObjectLockRetainUntilDate
    , cmuMetadata
    , cmuCacheControl
    , cmuContentLanguage
    , cmuObjectLockLegalHoldStatus
    , cmuACL
    , cmuContentDisposition
    , cmuServerSideEncryption
    , cmuContentType
    , cmuBucket
    , cmuKey

    -- * Destructuring the Response
    , createMultipartUploadResponse
    , CreateMultipartUploadResponse
    -- * Response Lenses
    , cmursRequestCharged
    , cmursBucket
    , cmursSSECustomerAlgorithm
    , cmursAbortDate
    , cmursAbortRuleId
    , cmursKey
    , cmursSSECustomerKeyMD5
    , cmursSSEKMSKeyId
    , cmursUploadId
    , cmursServerSideEncryption
    , cmursResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'createMultipartUpload' smart constructor.
data CreateMultipartUpload = CreateMultipartUpload'
  { _cmuObjectLockMode            :: !(Maybe ObjectLockMode)
  , _cmuExpires                   :: !(Maybe ISO8601)
  , _cmuGrantReadACP              :: !(Maybe Text)
  , _cmuSSECustomerAlgorithm      :: !(Maybe Text)
  , _cmuSSECustomerKey            :: !(Maybe (Sensitive Text))
  , _cmuRequestPayer              :: !(Maybe RequestPayer)
  , _cmuGrantWriteACP             :: !(Maybe Text)
  , _cmuWebsiteRedirectLocation   :: !(Maybe Text)
  , _cmuGrantRead                 :: !(Maybe Text)
  , _cmuStorageClass              :: !(Maybe StorageClass)
  , _cmuSSECustomerKeyMD5         :: !(Maybe Text)
  , _cmuSSEKMSKeyId               :: !(Maybe (Sensitive Text))
  , _cmuGrantFullControl          :: !(Maybe Text)
  , _cmuContentEncoding           :: !(Maybe Text)
  , _cmuTagging                   :: !(Maybe Text)
  , _cmuObjectLockRetainUntilDate :: !(Maybe ISO8601)
  , _cmuMetadata                  :: !(Map Text Text)
  , _cmuCacheControl              :: !(Maybe Text)
  , _cmuContentLanguage           :: !(Maybe Text)
  , _cmuObjectLockLegalHoldStatus :: !(Maybe ObjectLockLegalHoldStatus)
  , _cmuACL                       :: !(Maybe ObjectCannedACL)
  , _cmuContentDisposition        :: !(Maybe Text)
  , _cmuServerSideEncryption      :: !(Maybe ServerSideEncryption)
  , _cmuContentType               :: !(Maybe Text)
  , _cmuBucket                    :: !BucketName
  , _cmuKey                       :: !ObjectKey
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateMultipartUpload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmuObjectLockMode' - Specifies the Object Lock mode that you want to apply to the uploaded object.
--
-- * 'cmuExpires' - The date and time at which the object is no longer cacheable.
--
-- * 'cmuGrantReadACP' - Allows grantee to read the object ACL.
--
-- * 'cmuSSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (e.g., AES256).
--
-- * 'cmuSSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon does not store the encryption key. The key must be appropriate for use with the algorithm specified in the x-amz-server-side
