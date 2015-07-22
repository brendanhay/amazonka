{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.UploadArchive
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation adds an archive to a vault. This is a synchronous
-- operation, and for a successful upload, your data is durably persisted.
-- Amazon Glacier returns the archive ID in the @x-amz-archive-id@ header
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
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-UploadArchive.html>
module Network.AWS.Glacier.UploadArchive
    (
    -- * Request
      UploadArchive
    -- ** Request constructor
    , uploadArchive
    -- ** Request lenses
    , uarqChecksum
    , uarqArchiveDescription
    , uarqVaultName
    , uarqAccountId
    , uarqBody

    -- * Response
    , ArchiveCreationOutput
    -- ** Response constructor
    , archiveCreationOutput
    -- ** Response lenses
    , uarsArchiveId
    , uarsChecksum
    , uarsLocation
    ) where

import           Network.AWS.Glacier.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Provides options to add an archive to a vault.
--
-- /See:/ 'uploadArchive' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uarqChecksum'
--
-- * 'uarqArchiveDescription'
--
-- * 'uarqVaultName'
--
-- * 'uarqAccountId'
--
-- * 'uarqBody'
data UploadArchive = UploadArchive'
    { _uarqChecksum           :: !(Maybe Text)
    , _uarqArchiveDescription :: !(Maybe Text)
    , _uarqVaultName          :: !Text
    , _uarqAccountId          :: !Text
    , _uarqBody               :: !RqBody
    } deriving (Show,Generic)

-- | 'UploadArchive' smart constructor.
uploadArchive :: Text -> Text -> RqBody -> UploadArchive
uploadArchive pVaultName pAccountId pBody =
    UploadArchive'
    { _uarqChecksum = Nothing
    , _uarqArchiveDescription = Nothing
    , _uarqVaultName = pVaultName
    , _uarqAccountId = pAccountId
    , _uarqBody = pBody
    }

-- | The SHA256 tree hash of the data being uploaded.
uarqChecksum :: Lens' UploadArchive (Maybe Text)
uarqChecksum = lens _uarqChecksum (\ s a -> s{_uarqChecksum = a});

-- | The optional description of the archive you are uploading.
uarqArchiveDescription :: Lens' UploadArchive (Maybe Text)
uarqArchiveDescription = lens _uarqArchiveDescription (\ s a -> s{_uarqArchiveDescription = a});

-- | The name of the vault.
uarqVaultName :: Lens' UploadArchive Text
uarqVaultName = lens _uarqVaultName (\ s a -> s{_uarqVaultName = a});

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- apos@-@apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (apos-apos) in the ID.
uarqAccountId :: Lens' UploadArchive Text
uarqAccountId = lens _uarqAccountId (\ s a -> s{_uarqAccountId = a});

-- | The data to upload.
uarqBody :: Lens' UploadArchive RqBody
uarqBody = lens _uarqBody (\ s a -> s{_uarqBody = a});

instance AWSRequest UploadArchive where
        type Sv UploadArchive = Glacier
        type Rs UploadArchive = ArchiveCreationOutput
        request = postBody
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToBody UploadArchive where
        toBody = _uarqBody

instance ToHeaders UploadArchive where
        toHeaders UploadArchive'{..}
          = mconcat
              ["x-amz-sha256-tree-hash" =# _uarqChecksum,
               "x-amz-archive-description" =#
                 _uarqArchiveDescription]

instance ToPath UploadArchive where
        toPath UploadArchive'{..}
          = mconcat
              ["/", toText _uarqAccountId, "/vaults/",
               toText _uarqVaultName, "/archives"]

instance ToQuery UploadArchive where
        toQuery = const mempty
