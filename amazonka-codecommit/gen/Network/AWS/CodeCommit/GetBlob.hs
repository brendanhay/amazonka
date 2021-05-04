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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a get blob operation.
--
-- /See:/ 'newGetBlob' smart constructor.
data GetBlob = GetBlob'
  { -- | The name of the repository that contains the blob.
    repositoryName :: Prelude.Text,
    -- | The ID of the blob, which is its SHA-1 pointer.
    blobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest GetBlob where
  type Rs GetBlob = GetBlobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBlobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "content")
      )

instance Prelude.Hashable GetBlob

instance Prelude.NFData GetBlob

instance Prelude.ToHeaders GetBlob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeCommit_20150413.GetBlob" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetBlob where
  toJSON GetBlob' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("repositoryName" Prelude..= repositoryName),
            Prelude.Just ("blobId" Prelude..= blobId)
          ]
      )

instance Prelude.ToPath GetBlob where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetBlob where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a get blob operation.
--
-- /See:/ 'newGetBlobResponse' smart constructor.
data GetBlobResponse = GetBlobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The content of the blob, usually a file.
    content :: Prelude.Base64
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
      content = Prelude._Base64 Lens.# pContent_
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
getBlobResponse_content = Lens.lens (\GetBlobResponse' {content} -> content) (\s@GetBlobResponse' {} a -> s {content = a} :: GetBlobResponse) Prelude.. Prelude._Base64

instance Prelude.NFData GetBlobResponse
