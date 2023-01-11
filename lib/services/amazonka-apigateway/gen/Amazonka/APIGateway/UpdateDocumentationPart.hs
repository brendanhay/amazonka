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
-- Module      : Amazonka.APIGateway.UpdateDocumentationPart
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a documentation part.
module Amazonka.APIGateway.UpdateDocumentationPart
  ( -- * Creating a Request
    UpdateDocumentationPart (..),
    newUpdateDocumentationPart,

    -- * Request Lenses
    updateDocumentationPart_patchOperations,
    updateDocumentationPart_restApiId,
    updateDocumentationPart_documentationPartId,

    -- * Destructuring the Response
    DocumentationPart (..),
    newDocumentationPart,

    -- * Response Lenses
    documentationPart_id,
    documentationPart_location,
    documentationPart_properties,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Updates an existing documentation part of a given API.
--
-- /See:/ 'newUpdateDocumentationPart' smart constructor.
data UpdateDocumentationPart = UpdateDocumentationPart'
  { -- | For more information about supported patch operations, see
    -- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
    patchOperations :: Prelude.Maybe [PatchOperation],
    -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The identifier of the to-be-updated documentation part.
    documentationPartId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDocumentationPart' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'patchOperations', 'updateDocumentationPart_patchOperations' - For more information about supported patch operations, see
-- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
--
-- 'restApiId', 'updateDocumentationPart_restApiId' - The string identifier of the associated RestApi.
--
-- 'documentationPartId', 'updateDocumentationPart_documentationPartId' - The identifier of the to-be-updated documentation part.
newUpdateDocumentationPart ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'documentationPartId'
  Prelude.Text ->
  UpdateDocumentationPart
newUpdateDocumentationPart
  pRestApiId_
  pDocumentationPartId_ =
    UpdateDocumentationPart'
      { patchOperations =
          Prelude.Nothing,
        restApiId = pRestApiId_,
        documentationPartId = pDocumentationPartId_
      }

-- | For more information about supported patch operations, see
-- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
updateDocumentationPart_patchOperations :: Lens.Lens' UpdateDocumentationPart (Prelude.Maybe [PatchOperation])
updateDocumentationPart_patchOperations = Lens.lens (\UpdateDocumentationPart' {patchOperations} -> patchOperations) (\s@UpdateDocumentationPart' {} a -> s {patchOperations = a} :: UpdateDocumentationPart) Prelude.. Lens.mapping Lens.coerced

-- | The string identifier of the associated RestApi.
updateDocumentationPart_restApiId :: Lens.Lens' UpdateDocumentationPart Prelude.Text
updateDocumentationPart_restApiId = Lens.lens (\UpdateDocumentationPart' {restApiId} -> restApiId) (\s@UpdateDocumentationPart' {} a -> s {restApiId = a} :: UpdateDocumentationPart)

-- | The identifier of the to-be-updated documentation part.
updateDocumentationPart_documentationPartId :: Lens.Lens' UpdateDocumentationPart Prelude.Text
updateDocumentationPart_documentationPartId = Lens.lens (\UpdateDocumentationPart' {documentationPartId} -> documentationPartId) (\s@UpdateDocumentationPart' {} a -> s {documentationPartId = a} :: UpdateDocumentationPart)

instance Core.AWSRequest UpdateDocumentationPart where
  type
    AWSResponse UpdateDocumentationPart =
      DocumentationPart
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable UpdateDocumentationPart where
  hashWithSalt _salt UpdateDocumentationPart' {..} =
    _salt `Prelude.hashWithSalt` patchOperations
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` documentationPartId

instance Prelude.NFData UpdateDocumentationPart where
  rnf UpdateDocumentationPart' {..} =
    Prelude.rnf patchOperations
      `Prelude.seq` Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf documentationPartId

instance Data.ToHeaders UpdateDocumentationPart where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToJSON UpdateDocumentationPart where
  toJSON UpdateDocumentationPart' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("patchOperations" Data..=)
              Prelude.<$> patchOperations
          ]
      )

instance Data.ToPath UpdateDocumentationPart where
  toPath UpdateDocumentationPart' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/documentation/parts/",
        Data.toBS documentationPartId
      ]

instance Data.ToQuery UpdateDocumentationPart where
  toQuery = Prelude.const Prelude.mempty
