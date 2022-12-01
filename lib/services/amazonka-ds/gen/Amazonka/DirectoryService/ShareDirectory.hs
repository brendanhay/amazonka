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
-- Module      : Amazonka.DirectoryService.ShareDirectory
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shares a specified directory (@DirectoryId@) in your Amazon Web Services
-- account (directory owner) with another Amazon Web Services account
-- (directory consumer). With this operation you can use your directory
-- from any Amazon Web Services account and from any Amazon VPC within an
-- Amazon Web Services Region.
--
-- When you share your Managed Microsoft AD directory, Directory Service
-- creates a shared directory in the directory consumer account. This
-- shared directory contains the metadata to provide access to the
-- directory within the directory owner account. The shared directory is
-- visible in all VPCs in the directory consumer account.
--
-- The @ShareMethod@ parameter determines whether the specified directory
-- can be shared between Amazon Web Services accounts inside the same
-- Amazon Web Services organization (@ORGANIZATIONS@). It also determines
-- whether you can share the directory with any other Amazon Web Services
-- account either inside or outside of the organization (@HANDSHAKE@).
--
-- The @ShareNotes@ parameter is only used when @HANDSHAKE@ is called,
-- which sends a directory sharing request to the directory consumer.
module Amazonka.DirectoryService.ShareDirectory
  ( -- * Creating a Request
    ShareDirectory (..),
    newShareDirectory,

    -- * Request Lenses
    shareDirectory_shareNotes,
    shareDirectory_directoryId,
    shareDirectory_shareTarget,
    shareDirectory_shareMethod,

    -- * Destructuring the Response
    ShareDirectoryResponse (..),
    newShareDirectoryResponse,

    -- * Response Lenses
    shareDirectoryResponse_sharedDirectoryId,
    shareDirectoryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newShareDirectory' smart constructor.
data ShareDirectory = ShareDirectory'
  { -- | A directory share request that is sent by the directory owner to the
    -- directory consumer. The request includes a typed message to help the
    -- directory consumer administrator determine whether to approve or reject
    -- the share invitation.
    shareNotes :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Identifier of the Managed Microsoft AD directory that you want to share
    -- with other Amazon Web Services accounts.
    directoryId :: Prelude.Text,
    -- | Identifier for the directory consumer account with whom the directory is
    -- to be shared.
    shareTarget :: ShareTarget,
    -- | The method used when sharing a directory to determine whether the
    -- directory should be shared within your Amazon Web Services organization
    -- (@ORGANIZATIONS@) or with any Amazon Web Services account by sending a
    -- directory sharing request (@HANDSHAKE@).
    shareMethod :: ShareMethod
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ShareDirectory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shareNotes', 'shareDirectory_shareNotes' - A directory share request that is sent by the directory owner to the
-- directory consumer. The request includes a typed message to help the
-- directory consumer administrator determine whether to approve or reject
-- the share invitation.
--
-- 'directoryId', 'shareDirectory_directoryId' - Identifier of the Managed Microsoft AD directory that you want to share
-- with other Amazon Web Services accounts.
--
-- 'shareTarget', 'shareDirectory_shareTarget' - Identifier for the directory consumer account with whom the directory is
-- to be shared.
--
-- 'shareMethod', 'shareDirectory_shareMethod' - The method used when sharing a directory to determine whether the
-- directory should be shared within your Amazon Web Services organization
-- (@ORGANIZATIONS@) or with any Amazon Web Services account by sending a
-- directory sharing request (@HANDSHAKE@).
newShareDirectory ::
  -- | 'directoryId'
  Prelude.Text ->
  -- | 'shareTarget'
  ShareTarget ->
  -- | 'shareMethod'
  ShareMethod ->
  ShareDirectory
newShareDirectory
  pDirectoryId_
  pShareTarget_
  pShareMethod_ =
    ShareDirectory'
      { shareNotes = Prelude.Nothing,
        directoryId = pDirectoryId_,
        shareTarget = pShareTarget_,
        shareMethod = pShareMethod_
      }

-- | A directory share request that is sent by the directory owner to the
-- directory consumer. The request includes a typed message to help the
-- directory consumer administrator determine whether to approve or reject
-- the share invitation.
shareDirectory_shareNotes :: Lens.Lens' ShareDirectory (Prelude.Maybe Prelude.Text)
shareDirectory_shareNotes = Lens.lens (\ShareDirectory' {shareNotes} -> shareNotes) (\s@ShareDirectory' {} a -> s {shareNotes = a} :: ShareDirectory) Prelude.. Lens.mapping Core._Sensitive

-- | Identifier of the Managed Microsoft AD directory that you want to share
-- with other Amazon Web Services accounts.
shareDirectory_directoryId :: Lens.Lens' ShareDirectory Prelude.Text
shareDirectory_directoryId = Lens.lens (\ShareDirectory' {directoryId} -> directoryId) (\s@ShareDirectory' {} a -> s {directoryId = a} :: ShareDirectory)

-- | Identifier for the directory consumer account with whom the directory is
-- to be shared.
shareDirectory_shareTarget :: Lens.Lens' ShareDirectory ShareTarget
shareDirectory_shareTarget = Lens.lens (\ShareDirectory' {shareTarget} -> shareTarget) (\s@ShareDirectory' {} a -> s {shareTarget = a} :: ShareDirectory)

-- | The method used when sharing a directory to determine whether the
-- directory should be shared within your Amazon Web Services organization
-- (@ORGANIZATIONS@) or with any Amazon Web Services account by sending a
-- directory sharing request (@HANDSHAKE@).
shareDirectory_shareMethod :: Lens.Lens' ShareDirectory ShareMethod
shareDirectory_shareMethod = Lens.lens (\ShareDirectory' {shareMethod} -> shareMethod) (\s@ShareDirectory' {} a -> s {shareMethod = a} :: ShareDirectory)

instance Core.AWSRequest ShareDirectory where
  type
    AWSResponse ShareDirectory =
      ShareDirectoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ShareDirectoryResponse'
            Prelude.<$> (x Core..?> "SharedDirectoryId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ShareDirectory where
  hashWithSalt _salt ShareDirectory' {..} =
    _salt `Prelude.hashWithSalt` shareNotes
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` shareTarget
      `Prelude.hashWithSalt` shareMethod

instance Prelude.NFData ShareDirectory where
  rnf ShareDirectory' {..} =
    Prelude.rnf shareNotes
      `Prelude.seq` Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf shareTarget
      `Prelude.seq` Prelude.rnf shareMethod

instance Core.ToHeaders ShareDirectory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.ShareDirectory" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ShareDirectory where
  toJSON ShareDirectory' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ShareNotes" Core..=) Prelude.<$> shareNotes,
            Prelude.Just ("DirectoryId" Core..= directoryId),
            Prelude.Just ("ShareTarget" Core..= shareTarget),
            Prelude.Just ("ShareMethod" Core..= shareMethod)
          ]
      )

instance Core.ToPath ShareDirectory where
  toPath = Prelude.const "/"

instance Core.ToQuery ShareDirectory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newShareDirectoryResponse' smart constructor.
data ShareDirectoryResponse = ShareDirectoryResponse'
  { -- | Identifier of the directory that is stored in the directory consumer
    -- account that is shared from the specified directory (@DirectoryId@).
    sharedDirectoryId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ShareDirectoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sharedDirectoryId', 'shareDirectoryResponse_sharedDirectoryId' - Identifier of the directory that is stored in the directory consumer
-- account that is shared from the specified directory (@DirectoryId@).
--
-- 'httpStatus', 'shareDirectoryResponse_httpStatus' - The response's http status code.
newShareDirectoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ShareDirectoryResponse
newShareDirectoryResponse pHttpStatus_ =
  ShareDirectoryResponse'
    { sharedDirectoryId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Identifier of the directory that is stored in the directory consumer
-- account that is shared from the specified directory (@DirectoryId@).
shareDirectoryResponse_sharedDirectoryId :: Lens.Lens' ShareDirectoryResponse (Prelude.Maybe Prelude.Text)
shareDirectoryResponse_sharedDirectoryId = Lens.lens (\ShareDirectoryResponse' {sharedDirectoryId} -> sharedDirectoryId) (\s@ShareDirectoryResponse' {} a -> s {sharedDirectoryId = a} :: ShareDirectoryResponse)

-- | The response's http status code.
shareDirectoryResponse_httpStatus :: Lens.Lens' ShareDirectoryResponse Prelude.Int
shareDirectoryResponse_httpStatus = Lens.lens (\ShareDirectoryResponse' {httpStatus} -> httpStatus) (\s@ShareDirectoryResponse' {} a -> s {httpStatus = a} :: ShareDirectoryResponse)

instance Prelude.NFData ShareDirectoryResponse where
  rnf ShareDirectoryResponse' {..} =
    Prelude.rnf sharedDirectoryId
      `Prelude.seq` Prelude.rnf httpStatus
