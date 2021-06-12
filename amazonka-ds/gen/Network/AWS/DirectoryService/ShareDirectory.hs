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
-- Module      : Network.AWS.DirectoryService.ShareDirectory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shares a specified directory (@DirectoryId@) in your AWS account
-- (directory owner) with another AWS account (directory consumer). With
-- this operation you can use your directory from any AWS account and from
-- any Amazon VPC within an AWS Region.
--
-- When you share your AWS Managed Microsoft AD directory, AWS Directory
-- Service creates a shared directory in the directory consumer account.
-- This shared directory contains the metadata to provide access to the
-- directory within the directory owner account. The shared directory is
-- visible in all VPCs in the directory consumer account.
--
-- The @ShareMethod@ parameter determines whether the specified directory
-- can be shared between AWS accounts inside the same AWS organization
-- (@ORGANIZATIONS@). It also determines whether you can share the
-- directory with any other AWS account either inside or outside of the
-- organization (@HANDSHAKE@).
--
-- The @ShareNotes@ parameter is only used when @HANDSHAKE@ is called,
-- which sends a directory sharing request to the directory consumer.
module Network.AWS.DirectoryService.ShareDirectory
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

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newShareDirectory' smart constructor.
data ShareDirectory = ShareDirectory'
  { -- | A directory share request that is sent by the directory owner to the
    -- directory consumer. The request includes a typed message to help the
    -- directory consumer administrator determine whether to approve or reject
    -- the share invitation.
    shareNotes :: Core.Maybe (Core.Sensitive Core.Text),
    -- | Identifier of the AWS Managed Microsoft AD directory that you want to
    -- share with other AWS accounts.
    directoryId :: Core.Text,
    -- | Identifier for the directory consumer account with whom the directory is
    -- to be shared.
    shareTarget :: ShareTarget,
    -- | The method used when sharing a directory to determine whether the
    -- directory should be shared within your AWS organization
    -- (@ORGANIZATIONS@) or with any AWS account by sending a directory sharing
    -- request (@HANDSHAKE@).
    shareMethod :: ShareMethod
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
-- 'directoryId', 'shareDirectory_directoryId' - Identifier of the AWS Managed Microsoft AD directory that you want to
-- share with other AWS accounts.
--
-- 'shareTarget', 'shareDirectory_shareTarget' - Identifier for the directory consumer account with whom the directory is
-- to be shared.
--
-- 'shareMethod', 'shareDirectory_shareMethod' - The method used when sharing a directory to determine whether the
-- directory should be shared within your AWS organization
-- (@ORGANIZATIONS@) or with any AWS account by sending a directory sharing
-- request (@HANDSHAKE@).
newShareDirectory ::
  -- | 'directoryId'
  Core.Text ->
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
      { shareNotes = Core.Nothing,
        directoryId = pDirectoryId_,
        shareTarget = pShareTarget_,
        shareMethod = pShareMethod_
      }

-- | A directory share request that is sent by the directory owner to the
-- directory consumer. The request includes a typed message to help the
-- directory consumer administrator determine whether to approve or reject
-- the share invitation.
shareDirectory_shareNotes :: Lens.Lens' ShareDirectory (Core.Maybe Core.Text)
shareDirectory_shareNotes = Lens.lens (\ShareDirectory' {shareNotes} -> shareNotes) (\s@ShareDirectory' {} a -> s {shareNotes = a} :: ShareDirectory) Core.. Lens.mapping Core._Sensitive

-- | Identifier of the AWS Managed Microsoft AD directory that you want to
-- share with other AWS accounts.
shareDirectory_directoryId :: Lens.Lens' ShareDirectory Core.Text
shareDirectory_directoryId = Lens.lens (\ShareDirectory' {directoryId} -> directoryId) (\s@ShareDirectory' {} a -> s {directoryId = a} :: ShareDirectory)

-- | Identifier for the directory consumer account with whom the directory is
-- to be shared.
shareDirectory_shareTarget :: Lens.Lens' ShareDirectory ShareTarget
shareDirectory_shareTarget = Lens.lens (\ShareDirectory' {shareTarget} -> shareTarget) (\s@ShareDirectory' {} a -> s {shareTarget = a} :: ShareDirectory)

-- | The method used when sharing a directory to determine whether the
-- directory should be shared within your AWS organization
-- (@ORGANIZATIONS@) or with any AWS account by sending a directory sharing
-- request (@HANDSHAKE@).
shareDirectory_shareMethod :: Lens.Lens' ShareDirectory ShareMethod
shareDirectory_shareMethod = Lens.lens (\ShareDirectory' {shareMethod} -> shareMethod) (\s@ShareDirectory' {} a -> s {shareMethod = a} :: ShareDirectory)

instance Core.AWSRequest ShareDirectory where
  type
    AWSResponse ShareDirectory =
      ShareDirectoryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ShareDirectoryResponse'
            Core.<$> (x Core..?> "SharedDirectoryId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ShareDirectory

instance Core.NFData ShareDirectory

instance Core.ToHeaders ShareDirectory where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.ShareDirectory" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ShareDirectory where
  toJSON ShareDirectory' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ShareNotes" Core..=) Core.<$> shareNotes,
            Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just ("ShareTarget" Core..= shareTarget),
            Core.Just ("ShareMethod" Core..= shareMethod)
          ]
      )

instance Core.ToPath ShareDirectory where
  toPath = Core.const "/"

instance Core.ToQuery ShareDirectory where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newShareDirectoryResponse' smart constructor.
data ShareDirectoryResponse = ShareDirectoryResponse'
  { -- | Identifier of the directory that is stored in the directory consumer
    -- account that is shared from the specified directory (@DirectoryId@).
    sharedDirectoryId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ShareDirectoryResponse
newShareDirectoryResponse pHttpStatus_ =
  ShareDirectoryResponse'
    { sharedDirectoryId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Identifier of the directory that is stored in the directory consumer
-- account that is shared from the specified directory (@DirectoryId@).
shareDirectoryResponse_sharedDirectoryId :: Lens.Lens' ShareDirectoryResponse (Core.Maybe Core.Text)
shareDirectoryResponse_sharedDirectoryId = Lens.lens (\ShareDirectoryResponse' {sharedDirectoryId} -> sharedDirectoryId) (\s@ShareDirectoryResponse' {} a -> s {sharedDirectoryId = a} :: ShareDirectoryResponse)

-- | The response's http status code.
shareDirectoryResponse_httpStatus :: Lens.Lens' ShareDirectoryResponse Core.Int
shareDirectoryResponse_httpStatus = Lens.lens (\ShareDirectoryResponse' {httpStatus} -> httpStatus) (\s@ShareDirectoryResponse' {} a -> s {httpStatus = a} :: ShareDirectoryResponse)

instance Core.NFData ShareDirectoryResponse
