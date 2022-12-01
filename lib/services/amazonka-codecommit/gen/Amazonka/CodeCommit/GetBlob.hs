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
-- Module      : Amazonka.CodeCommit.GetBlob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the base-64 encoded content of an individual blob in a
-- repository.
module Amazonka.CodeCommit.GetBlob
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

import Amazonka.CodeCommit.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a get blob operation.
--
-- /See:/ 'newGetBlob' smart constructor.
data GetBlob = GetBlob'
  { -- | The name of the repository that contains the blob.
    repositoryName :: Prelude.Text,
    -- | The ID of the blob, which is its SHA-1 pointer.
    blobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'blobId'
  Prelude.Text ->
  GetBlob
newGetBlob pRepositoryName_ pBlobId_ =
  GetBlob'
    { repositoryName = pRepositoryName_,
      blobId = pBlobId_
    }

-- | The name of the repository that contains the blob.
getBlob_repositoryName :: Lens.Lens' GetBlob Prelude.Text
getBlob_repositoryName = Lens.lens (\GetBlob' {repositoryName} -> repositoryName) (\s@GetBlob' {} a -> s {repositoryName = a} :: GetBlob)

-- | The ID of the blob, which is its SHA-1 pointer.
getBlob_blobId :: Lens.Lens' GetBlob Prelude.Text
getBlob_blobId = Lens.lens (\GetBlob' {blobId} -> blobId) (\s@GetBlob' {} a -> s {blobId = a} :: GetBlob)

instance Core.AWSRequest GetBlob where
  type AWSResponse GetBlob = GetBlobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBlobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "content")
      )

instance Prelude.Hashable GetBlob where
  hashWithSalt _salt GetBlob' {..} =
    _salt `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` blobId

instance Prelude.NFData GetBlob where
  rnf GetBlob' {..} =
    Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf blobId

instance Core.ToHeaders GetBlob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeCommit_20150413.GetBlob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetBlob where
  toJSON GetBlob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("repositoryName" Core..= repositoryName),
            Prelude.Just ("blobId" Core..= blobId)
          ]
      )

instance Core.ToPath GetBlob where
  toPath = Prelude.const "/"

instance Core.ToQuery GetBlob where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a get blob operation.
--
-- /See:/ 'newGetBlobResponse' smart constructor.
data GetBlobResponse = GetBlobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The content of the blob, usually a file.
    content :: Core.Base64
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'content'
  Prelude.ByteString ->
  GetBlobResponse
newGetBlobResponse pHttpStatus_ pContent_ =
  GetBlobResponse'
    { httpStatus = pHttpStatus_,
      content = Core._Base64 Lens.# pContent_
    }

-- | The response's http status code.
getBlobResponse_httpStatus :: Lens.Lens' GetBlobResponse Prelude.Int
getBlobResponse_httpStatus = Lens.lens (\GetBlobResponse' {httpStatus} -> httpStatus) (\s@GetBlobResponse' {} a -> s {httpStatus = a} :: GetBlobResponse)

-- | The content of the blob, usually a file.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
getBlobResponse_content :: Lens.Lens' GetBlobResponse Prelude.ByteString
getBlobResponse_content = Lens.lens (\GetBlobResponse' {content} -> content) (\s@GetBlobResponse' {} a -> s {content = a} :: GetBlobResponse) Prelude.. Core._Base64

instance Prelude.NFData GetBlobResponse where
  rnf GetBlobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf content
