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
-- Module      : Network.AWS.APIGateway.UpdateDocumentationVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Network.AWS.APIGateway.UpdateDocumentationVersion
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
    documentationVersion_version,
    documentationVersion_description,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Updates an existing documentation version of an API.
--
-- /See:/ 'newUpdateDocumentationVersion' smart constructor.
data UpdateDocumentationVersion = UpdateDocumentationVersion'
  { -- | A list of update operations to be applied to the specified resource and
    -- in the order specified in this list.
    patchOperations :: Prelude.Maybe [PatchOperation],
    -- | [Required] The string identifier of the associated RestApi..
    restApiId :: Prelude.Text,
    -- | [Required] The version identifier of the to-be-updated documentation
    -- version.
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
-- 'patchOperations', 'updateDocumentationVersion_patchOperations' - A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
--
-- 'restApiId', 'updateDocumentationVersion_restApiId' - [Required] The string identifier of the associated RestApi..
--
-- 'documentationVersion', 'updateDocumentationVersion_documentationVersion' - [Required] The version identifier of the to-be-updated documentation
-- version.
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

-- | A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
updateDocumentationVersion_patchOperations :: Lens.Lens' UpdateDocumentationVersion (Prelude.Maybe [PatchOperation])
updateDocumentationVersion_patchOperations = Lens.lens (\UpdateDocumentationVersion' {patchOperations} -> patchOperations) (\s@UpdateDocumentationVersion' {} a -> s {patchOperations = a} :: UpdateDocumentationVersion) Prelude.. Lens.mapping Lens._Coerce

-- | [Required] The string identifier of the associated RestApi..
updateDocumentationVersion_restApiId :: Lens.Lens' UpdateDocumentationVersion Prelude.Text
updateDocumentationVersion_restApiId = Lens.lens (\UpdateDocumentationVersion' {restApiId} -> restApiId) (\s@UpdateDocumentationVersion' {} a -> s {restApiId = a} :: UpdateDocumentationVersion)

-- | [Required] The version identifier of the to-be-updated documentation
-- version.
updateDocumentationVersion_documentationVersion :: Lens.Lens' UpdateDocumentationVersion Prelude.Text
updateDocumentationVersion_documentationVersion = Lens.lens (\UpdateDocumentationVersion' {documentationVersion} -> documentationVersion) (\s@UpdateDocumentationVersion' {} a -> s {documentationVersion = a} :: UpdateDocumentationVersion)

instance Core.AWSRequest UpdateDocumentationVersion where
  type
    AWSResponse UpdateDocumentationVersion =
      DocumentationVersion
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable UpdateDocumentationVersion

instance Prelude.NFData UpdateDocumentationVersion

instance Core.ToHeaders UpdateDocumentationVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToJSON UpdateDocumentationVersion where
  toJSON UpdateDocumentationVersion' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("patchOperations" Core..=)
              Prelude.<$> patchOperations
          ]
      )

instance Core.ToPath UpdateDocumentationVersion where
  toPath UpdateDocumentationVersion' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/documentation/versions/",
        Core.toBS documentationVersion
      ]

instance Core.ToQuery UpdateDocumentationVersion where
  toQuery = Prelude.const Prelude.mempty
