{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

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
    , daAccountId
    , daVaultName
    , daArchiveId

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
-- * 'daAccountId'
--
-- * 'daVaultName'
--
-- * 'daArchiveId'
data DeleteArchive = DeleteArchive'
    { _daAccountId :: !Text
    , _daVaultName :: !Text
    , _daArchiveId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteArchive' smart constructor.
deleteArchive :: Text -> Text -> Text -> DeleteArchive
deleteArchive pAccountId pVaultName pArchiveId =
    DeleteArchive'
    { _daAccountId = pAccountId
    , _daVaultName = pVaultName
    , _daArchiveId = pArchiveId
    }

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- apos@-@apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (apos-apos) in the ID.
daAccountId :: Lens' DeleteArchive Text
daAccountId = lens _daAccountId (\ s a -> s{_daAccountId = a});

-- | The name of the vault.
daVaultName :: Lens' DeleteArchive Text
daVaultName = lens _daVaultName (\ s a -> s{_daVaultName = a});

-- | The ID of the archive to delete.
daArchiveId :: Lens' DeleteArchive Text
daArchiveId = lens _daArchiveId (\ s a -> s{_daArchiveId = a});

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
              ["/", toText _daAccountId, "/vaults/",
               toText _daVaultName, "/archives/",
               toText _daArchiveId]

instance ToQuery DeleteArchive where
        toQuery = const mempty

-- | /See:/ 'deleteArchiveResponse' smart constructor.
data DeleteArchiveResponse =
    DeleteArchiveResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteArchiveResponse' smart constructor.
deleteArchiveResponse :: DeleteArchiveResponse
deleteArchiveResponse = DeleteArchiveResponse'
