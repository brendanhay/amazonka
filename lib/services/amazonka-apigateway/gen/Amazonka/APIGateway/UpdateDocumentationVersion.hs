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
-- Module      : Amazonka.APIGateway.UpdateDocumentationVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a documentation version.
module Amazonka.APIGateway.UpdateDocumentationVersion
  ( -- * Creating a Request
    UpdateDocumentationVersion (..),
    newUpdateDocumentationVersion,

    -- * Request Lenses
    updateDocumentationVersion_patchOperations,
    updateDocumentationVersion_restApiId,
    updateDocumentationVersion_documentationVersion,

    -- * Destructuring the Response
    DocumentationVersion (..),
    newDocumentationVersion,

    -- * Response Lenses
    documentationVersion_createdDate,
    documentationVersion_description,
    documentationVersion_version,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Updates an existing documentation version of an API.
--
-- /See:/ 'newUpdateDocumentationVersion' smart constructor.
data UpdateDocumentationVersion = UpdateDocumentationVersion'
  { -- | For more information about supported patch operations, see
    -- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
    patchOperations :: Prelude.Maybe [PatchOperation],
    -- | The string identifier of the associated RestApi..
    restApiId :: Prelude.Text,
    -- | The version identifier of the to-be-updated documentation version.
    documentationVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDocumentationVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'patchOperations', 'updateDocumentationVersion_patchOperations' - For more information about supported patch operations, see
-- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
--
-- 'restApiId', 'updateDocumentationVersion_restApiId' - The string identifier of the associated RestApi..
--
-- 'documentationVersion', 'updateDocumentationVersion_documentationVersion' - The version identifier of the to-be-updated documentation version.
newUpdateDocumentationVersion ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'documentationVersion'
  Prelude.Text ->
  UpdateDocumentationVersion
newUpdateDocumentationVersion
  pRestApiId_
  pDocumentationVersion_ =
    UpdateDocumentationVersion'
      { patchOperations =
          Prelude.Nothing,
        restApiId = pRestApiId_,
        documentationVersion = pDocumentationVersion_
      }

-- | For more information about supported patch operations, see
-- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
updateDocumentationVersion_patchOperations :: Lens.Lens' UpdateDocumentationVersion (Prelude.Maybe [PatchOperation])
updateDocumentationVersion_patchOperations = Lens.lens (\UpdateDocumentationVersion' {patchOperations} -> patchOperations) (\s@UpdateDocumentationVersion' {} a -> s {patchOperations = a} :: UpdateDocumentationVersion) Prelude.. Lens.mapping Lens.coerced

-- | The string identifier of the associated RestApi..
updateDocumentationVersion_restApiId :: Lens.Lens' UpdateDocumentationVersion Prelude.Text
updateDocumentationVersion_restApiId = Lens.lens (\UpdateDocumentationVersion' {restApiId} -> restApiId) (\s@UpdateDocumentationVersion' {} a -> s {restApiId = a} :: UpdateDocumentationVersion)

-- | The version identifier of the to-be-updated documentation version.
updateDocumentationVersion_documentationVersion :: Lens.Lens' UpdateDocumentationVersion Prelude.Text
updateDocumentationVersion_documentationVersion = Lens.lens (\UpdateDocumentationVersion' {documentationVersion} -> documentationVersion) (\s@UpdateDocumentationVersion' {} a -> s {documentationVersion = a} :: UpdateDocumentationVersion)

instance Core.AWSRequest UpdateDocumentationVersion where
  type
    AWSResponse UpdateDocumentationVersion =
      DocumentationVersion
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable UpdateDocumentationVersion where
  hashWithSalt _salt UpdateDocumentationVersion' {..} =
    _salt
      `Prelude.hashWithSalt` patchOperations
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` documentationVersion

instance Prelude.NFData UpdateDocumentationVersion where
  rnf UpdateDocumentationVersion' {..} =
    Prelude.rnf patchOperations
      `Prelude.seq` Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf documentationVersion

instance Data.ToHeaders UpdateDocumentationVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToJSON UpdateDocumentationVersion where
  toJSON UpdateDocumentationVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("patchOperations" Data..=)
              Prelude.<$> patchOperations
          ]
      )

instance Data.ToPath UpdateDocumentationVersion where
  toPath UpdateDocumentationVersion' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/documentation/versions/",
        Data.toBS documentationVersion
      ]

instance Data.ToQuery UpdateDocumentationVersion where
  toQuery = Prelude.const Prelude.mempty
