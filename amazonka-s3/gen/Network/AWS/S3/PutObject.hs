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
-- Module      : Network.AWS.S3.PutObject
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an object to a bucket.
--
--
module Network.AWS.S3.PutObject
    (
    -- * Creating a Request
      putObject
    , PutObject
    -- * Request Lenses
    , poContentLength
    , poObjectLockMode
    , poExpires
    , poGrantReadACP
    , poSSECustomerAlgorithm
    , poSSECustomerKey
    , poRequestPayer
    , poGrantWriteACP
    , poWebsiteRedirectLocation
    , poGrantRead
    , poStorageClass
    , poSSECustomerKeyMD5
    , poSSEKMSKeyId
    , poGrantFullControl
    , poContentEncoding
    , poTagging
    , poContentMD5
    , poObjectLockRetainUntilDate
    , poMetadata
    , poCacheControl
    , poContentLanguage
    , poObjectLockLegalHoldStatus
    , poACL
    , poContentDisposition
    , poServerSideEncryption
    , poContentType
    , poBucket
    , poKey
    , poBody

    -- * Destructuring the Response
    , putObjectResponse
    , PutObjectResponse
    -- * Response Lenses
    , porsRequestCharged
    , porsETag
    , porsVersionId
    , porsExpiration
    , porsSSECustomerAlgorithm
    , porsSSECustomerKeyMD5
    , porsSSEKMSKeyId
    , porsServerSideEncryption
    , porsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'putObject' smart constructor.
data PutObject = PutObject'
  { _poContentLength             :: !(Maybe Integer)
  , _poObjectLockMode            :: !(Maybe ObjectLockMode)
  , _poExpires                   :: !(Maybe ISO8601)
  , _poGrantReadACP              :: !(Maybe Text)
  , _poSSECustomerAlgorithm      :: !(Maybe Text)
  , _poSSECustomerKey            :: !(Maybe (Sensitive Text))
  , _poRequestPayer              :: !(Maybe RequestPayer)
  , _poGrantWriteACP             :: !(Maybe Text)
  , _poWebsiteRedirectLocation   :: !(Maybe Text)
  , _poGrantRead                 :: !(Maybe Text)
  , _poStorageClass              :: !(Maybe StorageClass)
  , _poSSECustomerKeyMD5         :: !(Maybe Text)
  , _poSSEKMSKeyId               :: !(Maybe (Sensitive Text))
  , _poGrantFullControl          :: !(Maybe Text)
  , _poContentEncoding           :: !(Maybe Text)
  , _poTagging                   :: !(Maybe Text)
  , _poContentMD5                :: !(Maybe Text)
  , _poObjectLockRetainUntilDate :: !(Maybe ISO8601)
  , _poMetadata                  :: !(Map Text Text)
  , _poCacheControl              :: !(Maybe Text)
  , _poContentLanguage           :: !(Maybe Text)
  , _poObjectLockLegalHoldStatus :: !(Maybe ObjectLockLegalHoldStatus)
  , _poACL                       :: !(Maybe ObjectCannedACL)
  , _poContentDisposition        :: !(Maybe Text)
  , _poServerSideEncryption      :: !(Maybe ServerSideEncryption)
  , _poContentType               :: !(Maybe Text)
  , _poBucket                    :: !BucketName
  , _poKey                       :: !ObjectKey
  , _poBody                      :: !RqBody
  } deriving (Show, Generic)


-- | Creates a value of 'PutObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'poContentLength' - Size of the body in bytes. This parameter is useful when the size of the body cannot be determined automatically.
--
-- * 'poObjectLockMode' - The Object Lock mode that you want to apply to this object.
--
-- * 'poExpires' - The date and time at which the object is no longer cacheable.
--
-- * 'poGrantReadACP' - Allows grantee to read the object ACL.
--
-- * 'poSSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (e.g., AES256).
--
-- * 'poSSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon does not store the encryption key. The key must be appropriate for use with the algorithm specified in the x-amz-server-side
