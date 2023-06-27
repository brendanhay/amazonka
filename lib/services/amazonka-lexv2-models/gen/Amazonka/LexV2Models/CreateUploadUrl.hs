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
-- Module      : Amazonka.LexV2Models.CreateUploadUrl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a pre-signed S3 write URL that you use to upload the zip archive
-- when importing a bot or a bot locale.
module Amazonka.LexV2Models.CreateUploadUrl
  ( -- * Creating a Request
    CreateUploadUrl (..),
    newCreateUploadUrl,

    -- * Destructuring the Response
    CreateUploadUrlResponse (..),
    newCreateUploadUrlResponse,

    -- * Response Lenses
    createUploadUrlResponse_importId,
    createUploadUrlResponse_uploadUrl,
    createUploadUrlResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateUploadUrl' smart constructor.
data CreateUploadUrl = CreateUploadUrl'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateUploadUrl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateUploadUrl ::
  CreateUploadUrl
newCreateUploadUrl = CreateUploadUrl'

instance Core.AWSRequest CreateUploadUrl where
  type
    AWSResponse CreateUploadUrl =
      CreateUploadUrlResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUploadUrlResponse'
            Prelude.<$> (x Data..?> "importId")
            Prelude.<*> (x Data..?> "uploadUrl")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateUploadUrl where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData CreateUploadUrl where
  rnf _ = ()

instance Data.ToHeaders CreateUploadUrl where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateUploadUrl where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath CreateUploadUrl where
  toPath = Prelude.const "/createuploadurl/"

instance Data.ToQuery CreateUploadUrl where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateUploadUrlResponse' smart constructor.
data CreateUploadUrlResponse = CreateUploadUrlResponse'
  { -- | An identifier for a unique import job. Use it when you call the
    -- <https://docs.aws.amazon.com/lexv2/latest/APIReference/API_StartImport.html StartImport>
    -- operation.
    importId :: Prelude.Maybe Prelude.Text,
    -- | A pre-signed S3 write URL. Upload the zip archive file that contains the
    -- definition of your bot or bot locale.
    uploadUrl :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateUploadUrlResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'importId', 'createUploadUrlResponse_importId' - An identifier for a unique import job. Use it when you call the
-- <https://docs.aws.amazon.com/lexv2/latest/APIReference/API_StartImport.html StartImport>
-- operation.
--
-- 'uploadUrl', 'createUploadUrlResponse_uploadUrl' - A pre-signed S3 write URL. Upload the zip archive file that contains the
-- definition of your bot or bot locale.
--
-- 'httpStatus', 'createUploadUrlResponse_httpStatus' - The response's http status code.
newCreateUploadUrlResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateUploadUrlResponse
newCreateUploadUrlResponse pHttpStatus_ =
  CreateUploadUrlResponse'
    { importId =
        Prelude.Nothing,
      uploadUrl = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier for a unique import job. Use it when you call the
-- <https://docs.aws.amazon.com/lexv2/latest/APIReference/API_StartImport.html StartImport>
-- operation.
createUploadUrlResponse_importId :: Lens.Lens' CreateUploadUrlResponse (Prelude.Maybe Prelude.Text)
createUploadUrlResponse_importId = Lens.lens (\CreateUploadUrlResponse' {importId} -> importId) (\s@CreateUploadUrlResponse' {} a -> s {importId = a} :: CreateUploadUrlResponse)

-- | A pre-signed S3 write URL. Upload the zip archive file that contains the
-- definition of your bot or bot locale.
createUploadUrlResponse_uploadUrl :: Lens.Lens' CreateUploadUrlResponse (Prelude.Maybe Prelude.Text)
createUploadUrlResponse_uploadUrl = Lens.lens (\CreateUploadUrlResponse' {uploadUrl} -> uploadUrl) (\s@CreateUploadUrlResponse' {} a -> s {uploadUrl = a} :: CreateUploadUrlResponse)

-- | The response's http status code.
createUploadUrlResponse_httpStatus :: Lens.Lens' CreateUploadUrlResponse Prelude.Int
createUploadUrlResponse_httpStatus = Lens.lens (\CreateUploadUrlResponse' {httpStatus} -> httpStatus) (\s@CreateUploadUrlResponse' {} a -> s {httpStatus = a} :: CreateUploadUrlResponse)

instance Prelude.NFData CreateUploadUrlResponse where
  rnf CreateUploadUrlResponse' {..} =
    Prelude.rnf importId
      `Prelude.seq` Prelude.rnf uploadUrl
      `Prelude.seq` Prelude.rnf httpStatus
