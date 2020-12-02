{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.DeleteFileSystemPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the @FileSystemPolicy@ for the specified file system. The default @FileSystemPolicy@ goes into effect once the existing policy is deleted. For more information about the default file system policy, see <https://docs.aws.amazon.com/efs/latest/ug/res-based-policies-efs.html Using Resource-based Policies with EFS> .
--
--
-- This operation requires permissions for the @elasticfilesystem:DeleteFileSystemPolicy@ action.
module Network.AWS.EFS.DeleteFileSystemPolicy
  ( -- * Creating a Request
    deleteFileSystemPolicy,
    DeleteFileSystemPolicy,

    -- * Request Lenses
    dfspFileSystemId,

    -- * Destructuring the Response
    deleteFileSystemPolicyResponse,
    DeleteFileSystemPolicyResponse,
  )
where

import Network.AWS.EFS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteFileSystemPolicy' smart constructor.
newtype DeleteFileSystemPolicy = DeleteFileSystemPolicy'
  { _dfspFileSystemId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteFileSystemPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfspFileSystemId' - Specifies the EFS file system for which to delete the @FileSystemPolicy@ .
deleteFileSystemPolicy ::
  -- | 'dfspFileSystemId'
  Text ->
  DeleteFileSystemPolicy
deleteFileSystemPolicy pFileSystemId_ =
  DeleteFileSystemPolicy' {_dfspFileSystemId = pFileSystemId_}

-- | Specifies the EFS file system for which to delete the @FileSystemPolicy@ .
dfspFileSystemId :: Lens' DeleteFileSystemPolicy Text
dfspFileSystemId = lens _dfspFileSystemId (\s a -> s {_dfspFileSystemId = a})

instance AWSRequest DeleteFileSystemPolicy where
  type Rs DeleteFileSystemPolicy = DeleteFileSystemPolicyResponse
  request = delete efs
  response = receiveNull DeleteFileSystemPolicyResponse'

instance Hashable DeleteFileSystemPolicy

instance NFData DeleteFileSystemPolicy

instance ToHeaders DeleteFileSystemPolicy where
  toHeaders = const mempty

instance ToPath DeleteFileSystemPolicy where
  toPath DeleteFileSystemPolicy' {..} =
    mconcat
      ["/2015-02-01/file-systems/", toBS _dfspFileSystemId, "/policy"]

instance ToQuery DeleteFileSystemPolicy where
  toQuery = const mempty

-- | /See:/ 'deleteFileSystemPolicyResponse' smart constructor.
data DeleteFileSystemPolicyResponse = DeleteFileSystemPolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteFileSystemPolicyResponse' with the minimum fields required to make a request.
deleteFileSystemPolicyResponse ::
  DeleteFileSystemPolicyResponse
deleteFileSystemPolicyResponse = DeleteFileSystemPolicyResponse'

instance NFData DeleteFileSystemPolicyResponse
