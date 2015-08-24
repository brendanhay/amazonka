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
-- Module      : Network.AWS.Glacier.UploadArchive
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation adds an archive to a vault. This is a synchronous
-- operation, and for a successful upload, your data is durably persisted.
-- Amazon Glacier returns the archive ID in the 'x-amz-archive-id' header
-- of the response.
--
-- You must use the archive ID to access your data in Amazon Glacier. After
-- you upload an archive, you should save the archive ID returned so that
-- you can retrieve or delete the archive later. Besides saving the archive
-- ID, you can also index it and give it a friendly name to allow for
-- better searching. You can also use the optional archive description
-- field to specify how the archive is referred to in an external index of
-- archives, such as you might create in Amazon DynamoDB. You can also get
-- the vault inventory to obtain a list of archive IDs in a vault. For more
-- information, see InitiateJob.
--
-- You must provide a SHA256 tree hash of the data you are uploading. For
-- information about computing a SHA256 tree hash, see
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/checksum-calculations.html Computing Checksums>.
--
-- You can optionally specify an archive description of up to 1,024
-- printable ASCII characters. You can get the archive description when you
-- either retrieve the archive or get the vault inventory. For more
-- information, see InitiateJob. Amazon Glacier does not interpret the
-- description in any way. An archive description does not need to be
-- unique. You cannot use the description to retrieve or sort the archive
-- list.
--
-- Archives are immutable. After you upload an archive, you cannot edit the
-- archive or its description.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don\'t have any
-- permissions by default. You must grant them explicit permission to
-- perform specific actions. For more information, see
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)>.
--
-- For conceptual information and underlying REST API, go to
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/uploading-an-archive.html Uploading an Archive in Amazon Glacier>
-- and
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-archive-post.html Upload Archive>
-- in the /Amazon Glacier Developer Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-UploadArchive.html AWS API Reference> for UploadArchive.
module Network.AWS.Glacier.UploadArchive
    (
    -- * Creating a Request
      uploadArchive
    , UploadArchive
    -- * Request Lenses
    , uaChecksum
    , uaArchiveDescription
    , uaVaultName
    , uaAccountId
    , uaBody

    -- * Destructuring the Response
    , archiveCreationOutput
    , ArchiveCreationOutput
    -- * Response Lenses
    , acoArchiveId
    , acoChecksum
    , acoLocation
    ) where

import           Network.AWS.Glacier.Types
import           Network.AWS.Glacier.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Provides options to add an archive to a vault.
--
-- /See:/ 'uploadArchive' smart constructor.
data UploadArchive = UploadArchive'
    { _uaChecksum           :: !(Maybe Text)
    , _uaArchiveDescription :: !(Maybe Text)
    , _uaVaultName          :: !Text
    , _uaAccountId          :: !Text
    , _uaBody               :: !RqBody
    } deriving (Show,Generic)

-- | Creates a value of 'UploadArchive' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaChecksum'
--
-- * 'uaArchiveDescription'
--
-- * 'uaVaultName'
--
-- * 'uaAccountId'
--
-- * 'uaBody'
uploadArchive
    :: Text -- ^ 'uaVaultName'
    -> Text -- ^ 'uaAccountId'
    -> RqBody -- ^ 'uaBody'
    -> UploadArchive
uploadArchive pVaultName_ pAccountId_ pBody_ =
    UploadArchive'
    { _uaChecksum = Nothing
    , _uaArchiveDescription = Nothing
    , _uaVaultName = pVaultName_
    , _uaAccountId = pAccountId_
    , _uaBody = pBody_
    }

-- | The SHA256 tree hash of the data being uploaded.
uaChecksum :: Lens' UploadArchive (Maybe Text)
uaChecksum = lens _uaChecksum (\ s a -> s{_uaChecksum = a});

-- | The optional description of the archive you are uploading.
uaArchiveDescription :: Lens' UploadArchive (Maybe Text)
uaArchiveDescription = lens _uaArchiveDescription (\ s a -> s{_uaArchiveDescription = a});

-- | The name of the vault.
uaVaultName :: Lens' UploadArchive Text
uaVaultName = lens _uaVaultName (\ s a -> s{_uaVaultName = a});

-- | The 'AccountId' value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- apos'-'apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (apos-apos) in the ID.
uaAccountId :: Lens' UploadArchive Text
uaAccountId = lens _uaAccountId (\ s a -> s{_uaAccountId = a});

-- | The data to upload.
uaBody :: Lens' UploadArchive RqBody
uaBody = lens _uaBody (\ s a -> s{_uaBody = a});

instance AWSRequest UploadArchive where
        type Rs UploadArchive = ArchiveCreationOutput
        request = postBody glacier
        response
          = receiveEmpty
              (\ s h x ->
                 ArchiveCreationOutput' <$>
                   (h .#? "x-amz-archive-id") <*>
                     (h .#? "x-amz-sha256-tree-hash")
                     <*> (h .#? "Location"))

instance ToBody UploadArchive where
        toBody = _uaBody

instance ToHeaders UploadArchive where
        toHeaders UploadArchive'{..}
          = mconcat
              ["x-amz-sha256-tree-hash" =# _uaChecksum,
               "x-amz-archive-description" =# _uaArchiveDescription]

instance ToPath UploadArchive where
        toPath UploadArchive'{..}
          = mconcat
              ["/", toBS _uaAccountId, "/vaults/",
               toBS _uaVaultName, "/archives"]

instance ToQuery UploadArchive where
        toQuery = const mempty
