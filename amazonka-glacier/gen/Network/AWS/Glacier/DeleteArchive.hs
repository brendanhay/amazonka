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
-- Module      : Network.AWS.Glacier.DeleteArchive
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation deletes an archive from a vault. Subsequent requests to initiate a retrieval of this archive will fail. Archive retrievals that are in progress for this archive ID may or may not succeed according to the following scenarios:
--
--
--     * If the archive retrieval job is actively preparing the data for download when Amazon Glacier receives the delete archive request, the archival retrieval operation might fail.
--
--     * If the archive retrieval job has successfully prepared the archive for download when Amazon Glacier receives the delete archive request, you will be able to download the output.
--
--
--
-- This operation is idempotent. Attempting to delete an already-deleted archive does not result in an error.
--
-- An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)> .
--
-- For conceptual information and underlying REST API, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/deleting-an-archive.html Deleting an Archive in Amazon Glacier> and <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-archive-delete.html Delete Archive> in the /Amazon Glacier Developer Guide/ .
--
module Network.AWS.Glacier.DeleteArchive
    (
    -- * Creating a Request
      deleteArchive
    , DeleteArchive
    -- * Request Lenses
    , daAccountId
    , daVaultName
    , daArchiveId

    -- * Destructuring the Response
    , deleteArchiveResponse
    , DeleteArchiveResponse
    ) where

import Network.AWS.Glacier.Types
import Network.AWS.Glacier.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Provides options for deleting an archive from an Amazon Glacier vault.
--
--
--
-- /See:/ 'deleteArchive' smart constructor.
data DeleteArchive = DeleteArchive'
  { _daAccountId :: !Text
  , _daVaultName :: !Text
  , _daArchiveId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteArchive' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daAccountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- * 'daVaultName' - The name of the vault.
--
-- * 'daArchiveId' - The ID of the archive to delete.
deleteArchive
    :: Text -- ^ 'daAccountId'
    -> Text -- ^ 'daVaultName'
    -> Text -- ^ 'daArchiveId'
    -> DeleteArchive
deleteArchive pAccountId_ pVaultName_ pArchiveId_ =
  DeleteArchive'
    { _daAccountId = pAccountId_
    , _daVaultName = pVaultName_
    , _daArchiveId = pArchiveId_
    }


-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
daAccountId :: Lens' DeleteArchive Text
daAccountId = lens _daAccountId (\ s a -> s{_daAccountId = a})

-- | The name of the vault.
daVaultName :: Lens' DeleteArchive Text
daVaultName = lens _daVaultName (\ s a -> s{_daVaultName = a})

-- | The ID of the archive to delete.
daArchiveId :: Lens' DeleteArchive Text
daArchiveId = lens _daArchiveId (\ s a -> s{_daArchiveId = a})

instance AWSRequest DeleteArchive where
        type Rs DeleteArchive = DeleteArchiveResponse
        request = delete glacier
        response = receiveNull DeleteArchiveResponse'

instance Hashable DeleteArchive where

instance NFData DeleteArchive where

instance ToHeaders DeleteArchive where
        toHeaders = const mempty

instance ToPath DeleteArchive where
        toPath DeleteArchive'{..}
          = mconcat
              ["/", toBS _daAccountId, "/vaults/",
               toBS _daVaultName, "/archives/", toBS _daArchiveId]

instance ToQuery DeleteArchive where
        toQuery = const mempty

-- | /See:/ 'deleteArchiveResponse' smart constructor.
data DeleteArchiveResponse =
  DeleteArchiveResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteArchiveResponse' with the minimum fields required to make a request.
--
deleteArchiveResponse
    :: DeleteArchiveResponse
deleteArchiveResponse = DeleteArchiveResponse'


instance NFData DeleteArchiveResponse where
