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
-- Module      : Network.AWS.CodeCommit.GetBlob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the base-64 encoded content of an individual blob in a
-- repository.
module Network.AWS.CodeCommit.GetBlob
  ( -- * Creating a Request
    GetBlob (..),
    newGetBlob,

    -- * Request Lenses
    getBlob_repositoryName,
    getBlob_blobId,

    -- * Destructuring the Response
    GetBlobResponse (..),
    newGetBlobResponse,

    -- * Response Lenses
    getBlobResponse_httpStatus,
    getBlobResponse_content,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a get blob operation.
--
-- /See:/ 'newGetBlob' smart constructor.
data GetBlob = GetBlob'
  { -- | The name of the repository that contains the blob.
    repositoryName :: Core.Text,
    -- | The ID of the blob, which is its SHA-1 pointer.
    blobId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetBlob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryName', 'getBlob_repositoryName' - The name of the repository that contains the blob.
--
-- 'blobId', 'getBlob_blobId' - The ID of the blob, which is its SHA-1 pointer.
newGetBlob ::
  -- | 'repositoryName'
  Core.Text ->
  -- | 'blobId'
  Core.Text ->
  GetBlob
newGetBlob pRepositoryName_ pBlobId_ =
  GetBlob'
    { repositoryName = pRepositoryName_,
      blobId = pBlobId_
    }

-- | The name of the repository that contains the blob.
getBlob_repositoryName :: Lens.Lens' GetBlob Core.Text
getBlob_repositoryName = Lens.lens (\GetBlob' {repositoryName} -> repositoryName) (\s@GetBlob' {} a -> s {repositoryName = a} :: GetBlob)

-- | The ID of the blob, which is its SHA-1 pointer.
getBlob_blobId :: Lens.Lens' GetBlob Core.Text
getBlob_blobId = Lens.lens (\GetBlob' {blobId} -> blobId) (\s@GetBlob' {} a -> s {blobId = a} :: GetBlob)

instance Core.AWSRequest GetBlob where
  type AWSResponse GetBlob = GetBlobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBlobResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "content")
      )

instance Core.Hashable GetBlob

instance Core.NFData GetBlob

instance Core.ToHeaders GetBlob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("CodeCommit_20150413.GetBlob" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetBlob where
  toJSON GetBlob' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just ("blobId" Core..= blobId)
          ]
      )

instance Core.ToPath GetBlob where
  toPath = Core.const "/"

instance Core.ToQuery GetBlob where
  toQuery = Core.const Core.mempty

-- | Represents the output of a get blob operation.
--
-- /See:/ 'newGetBlobResponse' smart constructor.
data GetBlobResponse = GetBlobResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The content of the blob, usually a file.
    content :: Core.Base64
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetBlobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getBlobResponse_httpStatus' - The response's http status code.
--
-- 'content', 'getBlobResponse_content' - The content of the blob, usually a file.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newGetBlobResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'content'
  Core.ByteString ->
  GetBlobResponse
newGetBlobResponse pHttpStatus_ pContent_ =
  GetBlobResponse'
    { httpStatus = pHttpStatus_,
      content = Core._Base64 Lens.# pContent_
    }

-- | The response's http status code.
getBlobResponse_httpStatus :: Lens.Lens' GetBlobResponse Core.Int
getBlobResponse_httpStatus = Lens.lens (\GetBlobResponse' {httpStatus} -> httpStatus) (\s@GetBlobResponse' {} a -> s {httpStatus = a} :: GetBlobResponse)

-- | The content of the blob, usually a file.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
getBlobResponse_content :: Lens.Lens' GetBlobResponse Core.ByteString
getBlobResponse_content = Lens.lens (\GetBlobResponse' {content} -> content) (\s@GetBlobResponse' {} a -> s {content = a} :: GetBlobResponse) Core.. Core._Base64

instance Core.NFData GetBlobResponse
