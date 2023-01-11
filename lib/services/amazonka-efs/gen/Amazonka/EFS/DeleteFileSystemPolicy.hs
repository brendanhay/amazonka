{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EFS.DeleteFileSystemPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the @FileSystemPolicy@ for the specified file system. The
-- default @FileSystemPolicy@ goes into effect once the existing policy is
-- deleted. For more information about the default file system policy, see
-- <https://docs.aws.amazon.com/efs/latest/ug/res-based-policies-efs.html Using Resource-based Policies with EFS>.
--
-- This operation requires permissions for the
-- @elasticfilesystem:DeleteFileSystemPolicy@ action.
module Amazonka.EFS.DeleteFileSystemPolicy
  ( -- * Creating a Request
    DeleteFileSystemPolicy (..),
    newDeleteFileSystemPolicy,

    -- * Request Lenses
    deleteFileSystemPolicy_fileSystemId,

    -- * Destructuring the Response
    DeleteFileSystemPolicyResponse (..),
    newDeleteFileSystemPolicyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EFS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteFileSystemPolicy' smart constructor.
data DeleteFileSystemPolicy = DeleteFileSystemPolicy'
  { -- | Specifies the EFS file system for which to delete the
    -- @FileSystemPolicy@.
    fileSystemId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFileSystemPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileSystemId', 'deleteFileSystemPolicy_fileSystemId' - Specifies the EFS file system for which to delete the
-- @FileSystemPolicy@.
newDeleteFileSystemPolicy ::
  -- | 'fileSystemId'
  Prelude.Text ->
  DeleteFileSystemPolicy
newDeleteFileSystemPolicy pFileSystemId_ =
  DeleteFileSystemPolicy'
    { fileSystemId =
        pFileSystemId_
    }

-- | Specifies the EFS file system for which to delete the
-- @FileSystemPolicy@.
deleteFileSystemPolicy_fileSystemId :: Lens.Lens' DeleteFileSystemPolicy Prelude.Text
deleteFileSystemPolicy_fileSystemId = Lens.lens (\DeleteFileSystemPolicy' {fileSystemId} -> fileSystemId) (\s@DeleteFileSystemPolicy' {} a -> s {fileSystemId = a} :: DeleteFileSystemPolicy)

instance Core.AWSRequest DeleteFileSystemPolicy where
  type
    AWSResponse DeleteFileSystemPolicy =
      DeleteFileSystemPolicyResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteFileSystemPolicyResponse'

instance Prelude.Hashable DeleteFileSystemPolicy where
  hashWithSalt _salt DeleteFileSystemPolicy' {..} =
    _salt `Prelude.hashWithSalt` fileSystemId

instance Prelude.NFData DeleteFileSystemPolicy where
  rnf DeleteFileSystemPolicy' {..} =
    Prelude.rnf fileSystemId

instance Data.ToHeaders DeleteFileSystemPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteFileSystemPolicy where
  toPath DeleteFileSystemPolicy' {..} =
    Prelude.mconcat
      [ "/2015-02-01/file-systems/",
        Data.toBS fileSystemId,
        "/policy"
      ]

instance Data.ToQuery DeleteFileSystemPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFileSystemPolicyResponse' smart constructor.
data DeleteFileSystemPolicyResponse = DeleteFileSystemPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFileSystemPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteFileSystemPolicyResponse ::
  DeleteFileSystemPolicyResponse
newDeleteFileSystemPolicyResponse =
  DeleteFileSystemPolicyResponse'

instance
  Prelude.NFData
    DeleteFileSystemPolicyResponse
  where
  rnf _ = ()
