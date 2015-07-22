{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.DeleteArchive
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation deletes an archive from a vault. Subsequent requests to
-- initiate a retrieval of this archive will fail. Archive retrievals that
-- are in progress for this archive ID may or may not succeed according to
-- the following scenarios:
--
-- -   If the archive retrieval job is actively preparing the data for
--     download when Amazon Glacier receives the delete archive request,
--     the archival retrieval operation might fail.
-- -   If the archive retrieval job has successfully prepared the archive
--     for download when Amazon Glacier receives the delete archive
--     request, you will be able to download the output.
--
-- This operation is idempotent. Attempting to delete an already-deleted
-- archive does not result in an error.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don\'t have any
-- permissions by default. You must grant them explicit permission to
-- perform specific actions. For more information, see
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)>.
--
-- For conceptual information and underlying REST API, go to
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/deleting-an-archive.html Deleting an Archive in Amazon Glacier>
-- and
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-archive-delete.html Delete Archive>
-- in the /Amazon Glacier Developer Guide/.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-DeleteArchive.html>
module Network.AWS.Glacier.DeleteArchive
    (
    -- * Request
      DeleteArchive
    -- ** Request constructor
    , deleteArchive
    -- ** Request lenses
    , darqAccountId
    , darqVaultName
    , darqArchiveId

    -- * Response
    , DeleteArchiveResponse
    -- ** Response constructor
    , deleteArchiveResponse
    ) where

import           Network.AWS.Glacier.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Provides options for deleting an archive from an Amazon Glacier vault.
--
-- /See:/ 'deleteArchive' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'darqAccountId'
--
-- * 'darqVaultName'
--
-- * 'darqArchiveId'
data DeleteArchive = DeleteArchive'
    { _darqAccountId :: !Text
    , _darqVaultName :: !Text
    , _darqArchiveId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteArchive' smart constructor.
deleteArchive :: Text -> Text -> Text -> DeleteArchive
deleteArchive pAccountId_ pVaultName_ pArchiveId_ =
    DeleteArchive'
    { _darqAccountId = pAccountId_
    , _darqVaultName = pVaultName_
    , _darqArchiveId = pArchiveId_
    }

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- apos@-@apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (apos-apos) in the ID.
darqAccountId :: Lens' DeleteArchive Text
darqAccountId = lens _darqAccountId (\ s a -> s{_darqAccountId = a});

-- | The name of the vault.
darqVaultName :: Lens' DeleteArchive Text
darqVaultName = lens _darqVaultName (\ s a -> s{_darqVaultName = a});

-- | The ID of the archive to delete.
darqArchiveId :: Lens' DeleteArchive Text
darqArchiveId = lens _darqArchiveId (\ s a -> s{_darqArchiveId = a});

instance AWSRequest DeleteArchive where
        type Sv DeleteArchive = Glacier
        type Rs DeleteArchive = DeleteArchiveResponse
        request = delete
        response = receiveNull DeleteArchiveResponse'

instance ToHeaders DeleteArchive where
        toHeaders = const mempty

instance ToPath DeleteArchive where
        toPath DeleteArchive'{..}
          = mconcat
              ["/", toText _darqAccountId, "/vaults/",
               toText _darqVaultName, "/archives/",
               toText _darqArchiveId]

instance ToQuery DeleteArchive where
        toQuery = const mempty

-- | /See:/ 'deleteArchiveResponse' smart constructor.
data DeleteArchiveResponse =
    DeleteArchiveResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteArchiveResponse' smart constructor.
deleteArchiveResponse :: DeleteArchiveResponse
deleteArchiveResponse = DeleteArchiveResponse'
