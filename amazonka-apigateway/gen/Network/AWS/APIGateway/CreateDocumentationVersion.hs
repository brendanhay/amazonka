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
-- Module      : Network.AWS.APIGateway.CreateDocumentationVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Network.AWS.APIGateway.CreateDocumentationVersion
  ( -- * Creating a Request
    CreateDocumentationVersion (..),
    newCreateDocumentationVersion,

    -- * Request Lenses
    createDocumentationVersion_stageName,
    createDocumentationVersion_description,
    createDocumentationVersion_restApiId,
    createDocumentationVersion_documentationVersion,

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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Creates a new documentation version of a given API.
--
-- /See:/ 'newCreateDocumentationVersion' smart constructor.
data CreateDocumentationVersion = CreateDocumentationVersion'
  { -- | The stage name to be associated with the new documentation snapshot.
    stageName :: Core.Maybe Core.Text,
    -- | A description about the new documentation snapshot.
    description :: Core.Maybe Core.Text,
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Core.Text,
    -- | [Required] The version identifier of the new snapshot.
    documentationVersion :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDocumentationVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stageName', 'createDocumentationVersion_stageName' - The stage name to be associated with the new documentation snapshot.
--
-- 'description', 'createDocumentationVersion_description' - A description about the new documentation snapshot.
--
-- 'restApiId', 'createDocumentationVersion_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'documentationVersion', 'createDocumentationVersion_documentationVersion' - [Required] The version identifier of the new snapshot.
newCreateDocumentationVersion ::
  -- | 'restApiId'
  Core.Text ->
  -- | 'documentationVersion'
  Core.Text ->
  CreateDocumentationVersion
newCreateDocumentationVersion
  pRestApiId_
  pDocumentationVersion_ =
    CreateDocumentationVersion'
      { stageName =
          Core.Nothing,
        description = Core.Nothing,
        restApiId = pRestApiId_,
        documentationVersion = pDocumentationVersion_
      }

-- | The stage name to be associated with the new documentation snapshot.
createDocumentationVersion_stageName :: Lens.Lens' CreateDocumentationVersion (Core.Maybe Core.Text)
createDocumentationVersion_stageName = Lens.lens (\CreateDocumentationVersion' {stageName} -> stageName) (\s@CreateDocumentationVersion' {} a -> s {stageName = a} :: CreateDocumentationVersion)

-- | A description about the new documentation snapshot.
createDocumentationVersion_description :: Lens.Lens' CreateDocumentationVersion (Core.Maybe Core.Text)
createDocumentationVersion_description = Lens.lens (\CreateDocumentationVersion' {description} -> description) (\s@CreateDocumentationVersion' {} a -> s {description = a} :: CreateDocumentationVersion)

-- | [Required] The string identifier of the associated RestApi.
createDocumentationVersion_restApiId :: Lens.Lens' CreateDocumentationVersion Core.Text
createDocumentationVersion_restApiId = Lens.lens (\CreateDocumentationVersion' {restApiId} -> restApiId) (\s@CreateDocumentationVersion' {} a -> s {restApiId = a} :: CreateDocumentationVersion)

-- | [Required] The version identifier of the new snapshot.
createDocumentationVersion_documentationVersion :: Lens.Lens' CreateDocumentationVersion Core.Text
createDocumentationVersion_documentationVersion = Lens.lens (\CreateDocumentationVersion' {documentationVersion} -> documentationVersion) (\s@CreateDocumentationVersion' {} a -> s {documentationVersion = a} :: CreateDocumentationVersion)

instance Core.AWSRequest CreateDocumentationVersion where
  type
    AWSResponse CreateDocumentationVersion =
      DocumentationVersion
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable CreateDocumentationVersion

instance Core.NFData CreateDocumentationVersion

instance Core.ToHeaders CreateDocumentationVersion where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateDocumentationVersion where
  toJSON CreateDocumentationVersion' {..} =
    Core.object
      ( Core.catMaybes
          [ ("stageName" Core..=) Core.<$> stageName,
            ("description" Core..=) Core.<$> description,
            Core.Just
              ( "documentationVersion"
                  Core..= documentationVersion
              )
          ]
      )

instance Core.ToPath CreateDocumentationVersion where
  toPath CreateDocumentationVersion' {..} =
    Core.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/documentation/versions"
      ]

instance Core.ToQuery CreateDocumentationVersion where
  toQuery = Core.const Core.mempty
