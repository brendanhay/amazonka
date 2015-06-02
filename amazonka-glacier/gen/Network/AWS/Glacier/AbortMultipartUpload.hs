{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Glacier.AbortMultipartUpload
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation aborts a multipart upload identified by the upload ID.
--
-- After the Abort Multipart Upload request succeeds, you cannot upload any
-- more parts to the multipart upload or complete the multipart upload. Aborting
-- a completed upload fails. However, aborting an already-aborted upload will
-- succeed, for a short time. For more information about uploading a part and
-- completing a multipart upload, see 'UploadMultipartPart' and 'CompleteMultipartUpload'.
--
-- This operation is idempotent.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don't have any
-- permissions by default. You must grant them explicit permission to perform
-- specific actions. For more information, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identityand Access Management (IAM)>.
--
-- For conceptual information and underlying REST API, go to <http://docs.aws.amazon.com/amazonglacier/latest/dev/working-with-archives.html Working withArchives in Amazon Glacier> and <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-multipart-abort-upload.html Abort Multipart Upload> in the /Amazon GlacierDeveloper Guide/.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-AbortMultipartUpload.html>
module Network.AWS.Glacier.AbortMultipartUpload
    (
    -- * Request
      AbortMultipartUpload
    -- ** Request constructor
    , abortMultipartUpload
    -- ** Request lenses
    , amuAccountId
    , amuUploadId
    , amuVaultName

    -- * Response
    , AbortMultipartUploadResponse
    -- ** Response constructor
    , abortMultipartUploadResponse
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Glacier.Types
import qualified GHC.Exts

data AbortMultipartUpload = AbortMultipartUpload
    { _amuAccountId :: Text
    , _amuUploadId  :: Text
    , _amuVaultName :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'AbortMultipartUpload' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'amuAccountId' @::@ 'Text'
--
-- * 'amuUploadId' @::@ 'Text'
--
-- * 'amuVaultName' @::@ 'Text'
--
abortMultipartUpload :: Text -- ^ 'amuAccountId'
                     -> Text -- ^ 'amuVaultName'
                     -> Text -- ^ 'amuUploadId'
                     -> AbortMultipartUpload
abortMultipartUpload p1 p2 p3 = AbortMultipartUpload
    { _amuAccountId = p1
    , _amuVaultName = p2
    , _amuUploadId  = p3
    }

-- | The 'AccountId' value is the AWS account ID of the account that owns the vault.
-- You can either specify an AWS account ID or optionally a single apos'-'apos
-- (hyphen), in which case Amazon Glacier uses the AWS account ID associated
-- with the credentials used to sign the request. If you use an account ID, do
-- not include any hyphens (apos-apos) in the ID.
amuAccountId :: Lens' AbortMultipartUpload Text
amuAccountId = lens _amuAccountId (\s a -> s { _amuAccountId = a })

-- | The upload ID of the multipart upload to delete.
amuUploadId :: Lens' AbortMultipartUpload Text
amuUploadId = lens _amuUploadId (\s a -> s { _amuUploadId = a })

-- | The name of the vault.
amuVaultName :: Lens' AbortMultipartUpload Text
amuVaultName = lens _amuVaultName (\s a -> s { _amuVaultName = a })

data AbortMultipartUploadResponse = AbortMultipartUploadResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'AbortMultipartUploadResponse' constructor.
abortMultipartUploadResponse :: AbortMultipartUploadResponse
abortMultipartUploadResponse = AbortMultipartUploadResponse

instance ToPath AbortMultipartUpload where
    toPath AbortMultipartUpload{..} = mconcat
        [ "/"
        , toText _amuAccountId
        , "/vaults/"
        , toText _amuVaultName
        , "/multipart-uploads/"
        , toText _amuUploadId
        ]

instance ToQuery AbortMultipartUpload where
    toQuery = const mempty

instance ToHeaders AbortMultipartUpload

instance ToJSON AbortMultipartUpload where
    toJSON = const (toJSON Empty)

instance AWSRequest AbortMultipartUpload where
    type Sv AbortMultipartUpload = Glacier
    type Rs AbortMultipartUpload = AbortMultipartUploadResponse

    request  = delete
    response = nullResponse AbortMultipartUploadResponse
