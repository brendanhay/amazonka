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
-- Module      : Network.AWS.S3.GetObject
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves objects from Amazon S3.
--
--
module Network.AWS.S3.GetObject
    (
    -- * Creating a Request
      getObject
    , GetObject
    -- * Request Lenses
    , goIfMatch
    , goVersionId
    , goResponseContentType
    , goResponseContentDisposition
    , goResponseContentLanguage
    , goSSECustomerAlgorithm
    , goSSECustomerKey
    , goRequestPayer
    , goResponseContentEncoding
    , goIfModifiedSince
    , goPartNumber
    , goRange
    , goIfUnmodifiedSince
    , goSSECustomerKeyMD5
    , goResponseCacheControl
    , goResponseExpires
    , goIfNoneMatch
    , goBucket
    , goKey

    -- * Destructuring the Response
    , getObjectResponse
    , GetObjectResponse
    -- * Response Lenses
    , gorsRequestCharged
    , gorsPartsCount
    , gorsETag
    , gorsVersionId
    , gorsContentLength
    , gorsObjectLockMode
    , gorsExpires
    , gorsRestore
    , gorsExpiration
    , gorsDeleteMarker
    , gorsSSECustomerAlgorithm
    , gorsTagCount
    , gorsMissingMeta
    , gorsWebsiteRedirectLocation
    , gorsAcceptRanges
    , gorsStorageClass
    , gorsSSECustomerKeyMD5
    , gorsSSEKMSKeyId
    , gorsContentEncoding
    , gorsObjectLockRetainUntilDate
    , gorsMetadata
    , gorsReplicationStatus
    , gorsCacheControl
    , gorsContentLanguage
    , gorsLastModified
    , gorsObjectLockLegalHoldStatus
    , gorsContentDisposition
    , gorsContentRange
    , gorsServerSideEncryption
    , gorsContentType
    , gorsResponseStatus
    , gorsBody
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'getObject' smart constructor.
data GetObject = GetObject'
  { _goIfMatch                    :: !(Maybe Text)
  , _goVersionId                  :: !(Maybe ObjectVersionId)
  , _goResponseContentType        :: !(Maybe Text)
  , _goResponseContentDisposition :: !(Maybe Text)
  , _goResponseContentLanguage    :: !(Maybe Text)
  , _goSSECustomerAlgorithm       :: !(Maybe Text)
  , _goSSECustomerKey             :: !(Maybe (Sensitive Text))
  , _goRequestPayer               :: !(Maybe RequestPayer)
  , _goResponseContentEncoding    :: !(Maybe Text)
  , _goIfModifiedSince            :: !(Maybe ISO8601)
  , _goPartNumber                 :: !(Maybe Int)
  , _goRange                      :: !(Maybe Text)
  , _goIfUnmodifiedSince          :: !(Maybe ISO8601)
  , _goSSECustomerKeyMD5          :: !(Maybe Text)
  , _goResponseCacheControl       :: !(Maybe Text)
  , _goResponseExpires            :: !(Maybe ISO8601)
  , _goIfNoneMatch                :: !(Maybe Text)
  , _goBucket                     :: !BucketName
  , _goKey                        :: !ObjectKey
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'goIfMatch' - Return the object only if its entity tag (ETag) is the same as the one specified, otherwise return a 412 (precondition failed).
--
-- * 'goVersionId' - VersionId used to reference a specific version of the object.
--
-- * 'goResponseContentType' - Sets the Content-Type header of the response.
--
-- * 'goResponseContentDisposition' - Sets the Content-Disposition header of the response
--
-- * 'goResponseContentLanguage' - Sets the Content-Language header of the response.
--
-- * 'goSSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (e.g., AES256).
--
-- * 'goSSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon does not store the encryption key. The key must be appropriate for use with the algorithm specified in the x-amz-server-side
