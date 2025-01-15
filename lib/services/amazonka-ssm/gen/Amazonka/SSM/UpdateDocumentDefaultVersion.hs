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
-- Module      : Amazonka.SSM.UpdateDocumentDefaultVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Set the default version of a document.
--
-- If you change a document version for a State Manager association,
-- Systems Manager immediately runs the association unless you previously
-- specifed the @apply-only-at-cron-interval@ parameter.
module Amazonka.SSM.UpdateDocumentDefaultVersion
  ( -- * Creating a Request
    UpdateDocumentDefaultVersion (..),
    newUpdateDocumentDefaultVersion,

    -- * Request Lenses
    updateDocumentDefaultVersion_name,
    updateDocumentDefaultVersion_documentVersion,

    -- * Destructuring the Response
    UpdateDocumentDefaultVersionResponse (..),
    newUpdateDocumentDefaultVersionResponse,

    -- * Response Lenses
    updateDocumentDefaultVersionResponse_description,
    updateDocumentDefaultVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newUpdateDocumentDefaultVersion' smart constructor.
data UpdateDocumentDefaultVersion = UpdateDocumentDefaultVersion'
  { -- | The name of a custom document that you want to set as the default
    -- version.
    name :: Prelude.Text,
    -- | The version of a custom document that you want to set as the default
    -- version.
    documentVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDocumentDefaultVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateDocumentDefaultVersion_name' - The name of a custom document that you want to set as the default
-- version.
--
-- 'documentVersion', 'updateDocumentDefaultVersion_documentVersion' - The version of a custom document that you want to set as the default
-- version.
newUpdateDocumentDefaultVersion ::
  -- | 'name'
  Prelude.Text ->
  -- | 'documentVersion'
  Prelude.Text ->
  UpdateDocumentDefaultVersion
newUpdateDocumentDefaultVersion
  pName_
  pDocumentVersion_ =
    UpdateDocumentDefaultVersion'
      { name = pName_,
        documentVersion = pDocumentVersion_
      }

-- | The name of a custom document that you want to set as the default
-- version.
updateDocumentDefaultVersion_name :: Lens.Lens' UpdateDocumentDefaultVersion Prelude.Text
updateDocumentDefaultVersion_name = Lens.lens (\UpdateDocumentDefaultVersion' {name} -> name) (\s@UpdateDocumentDefaultVersion' {} a -> s {name = a} :: UpdateDocumentDefaultVersion)

-- | The version of a custom document that you want to set as the default
-- version.
updateDocumentDefaultVersion_documentVersion :: Lens.Lens' UpdateDocumentDefaultVersion Prelude.Text
updateDocumentDefaultVersion_documentVersion = Lens.lens (\UpdateDocumentDefaultVersion' {documentVersion} -> documentVersion) (\s@UpdateDocumentDefaultVersion' {} a -> s {documentVersion = a} :: UpdateDocumentDefaultVersion)

instance Core.AWSRequest UpdateDocumentDefaultVersion where
  type
    AWSResponse UpdateDocumentDefaultVersion =
      UpdateDocumentDefaultVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDocumentDefaultVersionResponse'
            Prelude.<$> (x Data..?> "Description")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateDocumentDefaultVersion
  where
  hashWithSalt _salt UpdateDocumentDefaultVersion' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` documentVersion

instance Prelude.NFData UpdateDocumentDefaultVersion where
  rnf UpdateDocumentDefaultVersion' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf documentVersion

instance Data.ToHeaders UpdateDocumentDefaultVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.UpdateDocumentDefaultVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDocumentDefaultVersion where
  toJSON UpdateDocumentDefaultVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("DocumentVersion" Data..= documentVersion)
          ]
      )

instance Data.ToPath UpdateDocumentDefaultVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateDocumentDefaultVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDocumentDefaultVersionResponse' smart constructor.
data UpdateDocumentDefaultVersionResponse = UpdateDocumentDefaultVersionResponse'
  { -- | The description of a custom document that you want to set as the default
    -- version.
    description :: Prelude.Maybe DocumentDefaultVersionDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDocumentDefaultVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateDocumentDefaultVersionResponse_description' - The description of a custom document that you want to set as the default
-- version.
--
-- 'httpStatus', 'updateDocumentDefaultVersionResponse_httpStatus' - The response's http status code.
newUpdateDocumentDefaultVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDocumentDefaultVersionResponse
newUpdateDocumentDefaultVersionResponse pHttpStatus_ =
  UpdateDocumentDefaultVersionResponse'
    { description =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The description of a custom document that you want to set as the default
-- version.
updateDocumentDefaultVersionResponse_description :: Lens.Lens' UpdateDocumentDefaultVersionResponse (Prelude.Maybe DocumentDefaultVersionDescription)
updateDocumentDefaultVersionResponse_description = Lens.lens (\UpdateDocumentDefaultVersionResponse' {description} -> description) (\s@UpdateDocumentDefaultVersionResponse' {} a -> s {description = a} :: UpdateDocumentDefaultVersionResponse)

-- | The response's http status code.
updateDocumentDefaultVersionResponse_httpStatus :: Lens.Lens' UpdateDocumentDefaultVersionResponse Prelude.Int
updateDocumentDefaultVersionResponse_httpStatus = Lens.lens (\UpdateDocumentDefaultVersionResponse' {httpStatus} -> httpStatus) (\s@UpdateDocumentDefaultVersionResponse' {} a -> s {httpStatus = a} :: UpdateDocumentDefaultVersionResponse)

instance
  Prelude.NFData
    UpdateDocumentDefaultVersionResponse
  where
  rnf UpdateDocumentDefaultVersionResponse' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf httpStatus
