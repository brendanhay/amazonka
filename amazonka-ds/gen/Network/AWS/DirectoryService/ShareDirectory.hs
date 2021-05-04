{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newShareDirectory' smart constructor.
data ShareDirectory = ShareDirectory'
  { -- | A directory share request that is sent by the directory owner to the
    -- directory consumer. The request includes a typed message to help the
    -- directory consumer administrator determine whether to approve or reject
    -- the share invitation.
    shareNotes :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | Identifier of the AWS Managed Microsoft AD directory that you want to
    -- share with other AWS accounts.
    directoryId :: Prelude.Text,
    -- | Identifier for the directory consumer account with whom the directory is
    -- to be shared.
    shareTarget :: ShareTarget,
    -- | The method used when sharing a directory to determine whether the
    -- directory should be shared within your AWS organization
    -- (@ORGANIZATIONS@) or with any AWS account by sending a directory sharing
    -- request (@HANDSHAKE@).
    shareMethod :: ShareMethod
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
shareDirectory_shareNotes = Lens.lens (\ShareDirectory' {shareNotes} -> shareNotes) (\s@ShareDirectory' {} a -> s {shareNotes = a} :: ShareDirectory) Prelude.. Lens.mapping Prelude._Sensitive

-- | Identifier of the AWS Managed Microsoft AD directory that you want to
-- share with other AWS accounts.
shareDirectory_directoryId :: Lens.Lens' ShareDirectory Prelude.Text
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

instance Prelude.AWSRequest ShareDirectory where
  type Rs ShareDirectory = ShareDirectoryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ShareDirectoryResponse'
            Prelude.<$> (x Prelude..?> "SharedDirectoryId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ShareDirectory

instance Prelude.NFData ShareDirectory

instance Prelude.ToHeaders ShareDirectory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DirectoryService_20150416.ShareDirectory" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ShareDirectory where
  toJSON ShareDirectory' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ShareNotes" Prelude..=) Prelude.<$> shareNotes,
            Prelude.Just ("DirectoryId" Prelude..= directoryId),
            Prelude.Just ("ShareTarget" Prelude..= shareTarget),
            Prelude.Just ("ShareMethod" Prelude..= shareMethod)
          ]
      )

instance Prelude.ToPath ShareDirectory where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ShareDirectory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newShareDirectoryResponse' smart constructor.
data ShareDirectoryResponse = ShareDirectoryResponse'
  { -- | Identifier of the directory that is stored in the directory consumer
    -- account that is shared from the specified directory (@DirectoryId@).
    sharedDirectoryId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData ShareDirectoryResponse
