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
-- Module      : Network.AWS.S3.HeadObject
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The HEAD operation retrieves metadata from an object without returning the object itself. This operation is useful if you're only interested in an object's metadata. To use HEAD, you must have READ access to the object.
--
--
module Network.AWS.S3.HeadObject
    (
    -- * Creating a Request
      headObject
    , HeadObject
    -- * Request Lenses
    , hoIfMatch
    , hoVersionId
    , hoSSECustomerAlgorithm
    , hoSSECustomerKey
    , hoRequestPayer
    , hoIfModifiedSince
    , hoPartNumber
    , hoRange
    , hoIfUnmodifiedSince
    , hoSSECustomerKeyMD5
    , hoIfNoneMatch
    , hoBucket
    , hoKey

    -- * Destructuring the Response
    , headObjectResponse
    , HeadObjectResponse
    -- * Response Lenses
    , horsRequestCharged
    , horsPartsCount
    , horsETag
    , horsVersionId
    , horsContentLength
    , horsObjectLockMode
    , horsExpires
    , horsRestore
    , horsExpiration
    , horsDeleteMarker
    , horsSSECustomerAlgorithm
    , horsMissingMeta
    , horsWebsiteRedirectLocation
    , horsAcceptRanges
    , horsStorageClass
    , horsSSECustomerKeyMD5
    , horsSSEKMSKeyId
    , horsContentEncoding
    , horsObjectLockRetainUntilDate
    , horsMetadata
    , horsReplicationStatus
    , horsCacheControl
    , horsContentLanguage
    , horsLastModified
    , horsObjectLockLegalHoldStatus
    , horsContentDisposition
    , horsServerSideEncryption
    , horsContentType
    , horsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'headObject' smart constructor.
data HeadObject = HeadObject'
  { _hoIfMatch              :: !(Maybe Text)
  , _hoVersionId            :: !(Maybe ObjectVersionId)
  , _hoSSECustomerAlgorithm :: !(Maybe Text)
  , _hoSSECustomerKey       :: !(Maybe (Sensitive Text))
  , _hoRequestPayer         :: !(Maybe RequestPayer)
  , _hoIfModifiedSince      :: !(Maybe ISO8601)
  , _hoPartNumber           :: !(Maybe Int)
  , _hoRange                :: !(Maybe Text)
  , _hoIfUnmodifiedSince    :: !(Maybe ISO8601)
  , _hoSSECustomerKeyMD5    :: !(Maybe Text)
  , _hoIfNoneMatch          :: !(Maybe Text)
  , _hoBucket               :: !BucketName
  , _hoKey                  :: !ObjectKey
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'HeadObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hoIfMatch' - Return the object only if its entity tag (ETag) is the same as the one specified, otherwise return a 412 (precondition failed).
--
-- * 'hoVersionId' - VersionId used to reference a specific version of the object.
--
-- * 'hoSSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (e.g., AES256).
--
-- * 'hoSSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon does not store the encryption key. The key must be appropriate for use with the algorithm specified in the x-amz-server-side
