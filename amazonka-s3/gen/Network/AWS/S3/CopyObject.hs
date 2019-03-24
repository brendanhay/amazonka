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
-- Module      : Network.AWS.S3.CopyObject
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a copy of an object that is already stored in Amazon S3.
--
--
module Network.AWS.S3.CopyObject
    (
    -- * Creating a Request
      copyObject
    , CopyObject
    -- * Request Lenses
    , coCopySourceIfModifiedSince
    , coCopySourceIfUnmodifiedSince
    , coCopySourceSSECustomerKeyMD5
    , coTaggingDirective
    , coMetadataDirective
    , coObjectLockMode
    , coExpires
    , coGrantReadACP
    , coCopySourceIfNoneMatch
    , coSSECustomerAlgorithm
    , coSSECustomerKey
    , coRequestPayer
    , coGrantWriteACP
    , coCopySourceIfMatch
    , coWebsiteRedirectLocation
    , coGrantRead
    , coStorageClass
    , coSSECustomerKeyMD5
    , coSSEKMSKeyId
    , coGrantFullControl
    , coContentEncoding
    , coTagging
    , coObjectLockRetainUntilDate
    , coMetadata
    , coCacheControl
    , coContentLanguage
    , coCopySourceSSECustomerKey
    , coObjectLockLegalHoldStatus
    , coCopySourceSSECustomerAlgorithm
    , coACL
    , coContentDisposition
    , coServerSideEncryption
    , coContentType
    , coBucket
    , coCopySource
    , coKey

    -- * Destructuring the Response
    , copyObjectResponse
    , CopyObjectResponse
    -- * Response Lenses
    , corsRequestCharged
    , corsVersionId
    , corsExpiration
    , corsSSECustomerAlgorithm
    , corsCopySourceVersionId
    , corsSSECustomerKeyMD5
    , corsSSEKMSKeyId
    , corsServerSideEncryption
    , corsCopyObjectResult
    , corsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'copyObject' smart constructor.
data CopyObject = CopyObject'
  { _coCopySourceIfModifiedSince      :: !(Maybe ISO8601)
  , _coCopySourceIfUnmodifiedSince    :: !(Maybe ISO8601)
  , _coCopySourceSSECustomerKeyMD5    :: !(Maybe Text)
  , _coTaggingDirective               :: !(Maybe TaggingDirective)
  , _coMetadataDirective              :: !(Maybe MetadataDirective)
  , _coObjectLockMode                 :: !(Maybe ObjectLockMode)
  , _coExpires                        :: !(Maybe ISO8601)
  , _coGrantReadACP                   :: !(Maybe Text)
  , _coCopySourceIfNoneMatch          :: !(Maybe Text)
  , _coSSECustomerAlgorithm           :: !(Maybe Text)
  , _coSSECustomerKey                 :: !(Maybe (Sensitive Text))
  , _coRequestPayer                   :: !(Maybe RequestPayer)
  , _coGrantWriteACP                  :: !(Maybe Text)
  , _coCopySourceIfMatch              :: !(Maybe Text)
  , _coWebsiteRedirectLocation        :: !(Maybe Text)
  , _coGrantRead                      :: !(Maybe Text)
  , _coStorageClass                   :: !(Maybe StorageClass)
  , _coSSECustomerKeyMD5              :: !(Maybe Text)
  , _coSSEKMSKeyId                    :: !(Maybe (Sensitive Text))
  , _coGrantFullControl               :: !(Maybe Text)
  , _coContentEncoding                :: !(Maybe Text)
  , _coTagging                        :: !(Maybe Text)
  , _coObjectLockRetainUntilDate      :: !(Maybe ISO8601)
  , _coMetadata                       :: !(Map Text Text)
  , _coCacheControl                   :: !(Maybe Text)
  , _coContentLanguage                :: !(Maybe Text)
  , _coCopySourceSSECustomerKey       :: !(Maybe (Sensitive Text))
  , _coObjectLockLegalHoldStatus      :: !(Maybe ObjectLockLegalHoldStatus)
  , _coCopySourceSSECustomerAlgorithm :: !(Maybe Text)
  , _coACL                            :: !(Maybe ObjectCannedACL)
  , _coContentDisposition             :: !(Maybe Text)
  , _coServerSideEncryption           :: !(Maybe ServerSideEncryption)
  , _coContentType                    :: !(Maybe Text)
  , _coBucket                         :: !BucketName
  , _coCopySource                     :: !Text
  , _coKey                            :: !ObjectKey
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CopyObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coCopySourceIfModifiedSince' - Copies the object if it has been modified since the specified time.
--
-- * 'coCopySourceIfUnmodifiedSince' - Copies the object if it hasn't been modified since the specified time.
--
-- * 'coCopySourceSSECustomerKeyMD5' - Specifies the 128-bit MD5 digest of the encryption key according to RFC 1321. Amazon S3 uses this header for a message integrity check to ensure the encryption key was transmitted without error.
--
-- * 'coTaggingDirective' - Specifies whether the object tag-set are copied from the source object or replaced with tag-set provided in the request.
--
-- * 'coMetadataDirective' - Specifies whether the metadata is copied from the source object or replaced with metadata provided in the request.
--
-- * 'coObjectLockMode' - The Object Lock mode that you want to apply to the copied object.
--
-- * 'coExpires' - The date and time at which the object is no longer cacheable.
--
-- * 'coGrantReadACP' - Allows grantee to read the object ACL.
--
-- * 'coCopySourceIfNoneMatch' - Copies the object if its entity tag (ETag) is different than the specified ETag.
--
-- * 'coSSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (e.g., AES256).
--
-- * 'coSSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon does not store the encryption key. The key must be appropriate for use with the algorithm specified in the x-amz-server-side
