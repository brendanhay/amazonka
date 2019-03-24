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
-- Module      : Network.AWS.S3.UploadPart
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads a part in a multipart upload.
--
--
-- __Note:__ After you initiate multipart upload and upload one or more parts, you must either complete or abort multipart upload in order to stop getting charged for storage of the uploaded parts. Only after you either complete or abort multipart upload, Amazon S3 frees up the parts storage and stops charging you for the parts storage.
--
module Network.AWS.S3.UploadPart
    (
    -- * Creating a Request
      uploadPart
    , UploadPart
    -- * Request Lenses
    , upContentLength
    , upSSECustomerAlgorithm
    , upSSECustomerKey
    , upRequestPayer
    , upSSECustomerKeyMD5
    , upContentMD5
    , upBucket
    , upKey
    , upPartNumber
    , upUploadId
    , upBody

    -- * Destructuring the Response
    , uploadPartResponse
    , UploadPartResponse
    -- * Response Lenses
    , uprsRequestCharged
    , uprsETag
    , uprsSSECustomerAlgorithm
    , uprsSSECustomerKeyMD5
    , uprsSSEKMSKeyId
    , uprsServerSideEncryption
    , uprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'uploadPart' smart constructor.
data UploadPart = UploadPart'
  { _upContentLength        :: !(Maybe Integer)
  , _upSSECustomerAlgorithm :: !(Maybe Text)
  , _upSSECustomerKey       :: !(Maybe (Sensitive Text))
  , _upRequestPayer         :: !(Maybe RequestPayer)
  , _upSSECustomerKeyMD5    :: !(Maybe Text)
  , _upContentMD5           :: !(Maybe Text)
  , _upBucket               :: !BucketName
  , _upKey                  :: !ObjectKey
  , _upPartNumber           :: !Int
  , _upUploadId             :: !Text
  , _upBody                 :: !RqBody
  } deriving (Show, Generic)


-- | Creates a value of 'UploadPart' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upContentLength' - Size of the body in bytes. This parameter is useful when the size of the body cannot be determined automatically.
--
-- * 'upSSECustomerAlgorithm' - Specifies the algorithm to use to when encrypting the object (e.g., AES256).
--
-- * 'upSSECustomerKey' - Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon does not store the encryption key. The key must be appropriate for use with the algorithm specified in the x-amz-server-side
