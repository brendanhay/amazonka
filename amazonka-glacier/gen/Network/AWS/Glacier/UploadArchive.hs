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

-- Module      : Network.AWS.Glacier.UploadArchive
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

-- | This operation adds an archive to a vault. This is a synchronous operation,
-- and for a successful upload, your data is durably persisted. Amazon Glacier
-- returns the archive ID in the 'x-amz-archive-id' header of the response.
--
-- You must use the archive ID to access your data in Amazon Glacier. After you
-- upload an archive, you should save the archive ID returned so that you can
-- retrieve or delete the archive later. Besides saving the archive ID, you can
-- also index it and give it a friendly name to allow for better searching. You
-- can also use the optional archive description field to specify how the
-- archive is referred to in an external index of archives, such as you might
-- create in Amazon DynamoDB. You can also get the vault inventory to obtain a
-- list of archive IDs in a vault. For more information, see 'InitiateJob'.
--
-- You must provide a SHA256 tree hash of the data you are uploading. For
-- information about computing a SHA256 tree hash, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/checksum-calculations.html Computing Checksums>.
--
-- You can optionally specify an archive description of up to 1,024 printable
-- ASCII characters. You can get the archive description when you either
-- retrieve the archive or get the vault inventory. For more information, see 'InitiateJob'. Amazon Glacier does not interpret the description in any way. An archive
-- description does not need to be unique. You cannot use the description to
-- retrieve or sort the archive list.
--
-- Archives are immutable. After you upload an archive, you cannot edit the
-- archive or its description.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don't have any
-- permissions by default. You must grant them explicit permission to perform
-- specific actions. For more information, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identityand Access Management (IAM)>.
--
-- For conceptual information and underlying REST API, go to <http://docs.aws.amazon.com/amazonglacier/latest/dev/uploading-an-archive.html Uploading anArchive in Amazon Glacier> and <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-archive-post.html Upload Archive> in the /Amazon Glacier DeveloperGuide/.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-UploadArchive.html>
module Network.AWS.Glacier.UploadArchive
    (
    -- * Request
      UploadArchive
    -- ** Request constructor
    , uploadArchive
    -- ** Request lenses
    , uaAccountId
    , uaArchiveDescription
    , uaBody
    , uaChecksum
    , uaVaultName

    -- * Response
    , UploadArchiveResponse
    -- ** Response constructor
    , uploadArchiveResponse
    -- ** Response lenses
    , uarArchiveId
    , uarChecksum
    , uarLocation
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Glacier.Types
import qualified GHC.Exts

data UploadArchive = UploadArchive
    { _uaAccountId          :: Text
    , _uaArchiveDescription :: Maybe Text
    , _uaBody               :: RqBody
    , _uaChecksum           :: Maybe Text
    , _uaVaultName          :: Text
    } deriving (Show)

-- | 'UploadArchive' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uaAccountId' @::@ 'Text'
--
-- * 'uaArchiveDescription' @::@ 'Maybe' 'Text'
--
-- * 'uaBody' @::@ 'RqBody'
--
-- * 'uaChecksum' @::@ 'Maybe' 'Text'
--
-- * 'uaVaultName' @::@ 'Text'
--
uploadArchive :: Text -- ^ 'uaVaultName'
              -> Text -- ^ 'uaAccountId'
              -> RqBody -- ^ 'uaBody'
              -> UploadArchive
uploadArchive p1 p2 p3 = UploadArchive
    { _uaVaultName          = p1
    , _uaAccountId          = p2
    , _uaBody               = p3
    , _uaArchiveDescription = Nothing
    , _uaChecksum           = Nothing
    }

-- | The 'AccountId' is the AWS Account ID. You can specify either the AWS Account
-- ID or optionally a '-', in which case Amazon Glacier uses the AWS Account ID
-- associated with the credentials used to sign the request. If you specify your
-- Account ID, do not include hyphens in it.
uaAccountId :: Lens' UploadArchive Text
uaAccountId = lens _uaAccountId (\s a -> s { _uaAccountId = a })

-- | The optional description of the archive you are uploading.
uaArchiveDescription :: Lens' UploadArchive (Maybe Text)
uaArchiveDescription =
    lens _uaArchiveDescription (\s a -> s { _uaArchiveDescription = a })

-- | The data to upload.
uaBody :: Lens' UploadArchive RqBody
uaBody = lens _uaBody (\s a -> s { _uaBody = a })

-- | The SHA256 checksum (a linear hash) of the payload.
uaChecksum :: Lens' UploadArchive (Maybe Text)
uaChecksum = lens _uaChecksum (\s a -> s { _uaChecksum = a })

-- | The name of the vault.
uaVaultName :: Lens' UploadArchive Text
uaVaultName = lens _uaVaultName (\s a -> s { _uaVaultName = a })

data UploadArchiveResponse = UploadArchiveResponse
    { _uarArchiveId :: Maybe Text
    , _uarChecksum  :: Maybe Text
    , _uarLocation  :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'UploadArchiveResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uarArchiveId' @::@ 'Maybe' 'Text'
--
-- * 'uarChecksum' @::@ 'Maybe' 'Text'
--
-- * 'uarLocation' @::@ 'Maybe' 'Text'
--
uploadArchiveResponse :: UploadArchiveResponse
uploadArchiveResponse = UploadArchiveResponse
    { _uarLocation  = Nothing
    , _uarChecksum  = Nothing
    , _uarArchiveId = Nothing
    }

-- | The ID of the archive. This value is also included as part of the location.
uarArchiveId :: Lens' UploadArchiveResponse (Maybe Text)
uarArchiveId = lens _uarArchiveId (\s a -> s { _uarArchiveId = a })

-- | The checksum of the archive computed by Amazon Glacier.
uarChecksum :: Lens' UploadArchiveResponse (Maybe Text)
uarChecksum = lens _uarChecksum (\s a -> s { _uarChecksum = a })

-- | The relative URI path of the newly added archive resource.
uarLocation :: Lens' UploadArchiveResponse (Maybe Text)
uarLocation = lens _uarLocation (\s a -> s { _uarLocation = a })

instance ToPath UploadArchive where
    toPath UploadArchive{..} = mconcat
        [ "/"
        , toText _uaAccountId
        , "/vaults/"
        , toText _uaVaultName
        , "/archives"
        ]

instance ToQuery UploadArchive where
    toQuery = const mempty

instance ToHeaders UploadArchive where
    toHeaders UploadArchive{..} = mconcat
        [ "x-amz-archive-description" =: _uaArchiveDescription
        , "x-amz-sha256-tree-hash"    =: _uaChecksum
        ]

instance ToBody UploadArchive where
    toBody = toBody . _uaBody

instance AWSRequest UploadArchive where
    type Sv UploadArchive = Glacier
    type Rs UploadArchive = UploadArchiveResponse

    request  = stream POST
    response = headerResponse $ \h -> UploadArchiveResponse
        <$> h ~:? "x-amz-archive-id"
        <*> h ~:? "x-amz-sha256-tree-hash"
        <*> h ~:? "Location"
