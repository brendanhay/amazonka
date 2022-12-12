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
-- Module      : Amazonka.APIGateway.CreateDocumentationVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a documentation version
module Amazonka.APIGateway.CreateDocumentationVersion
  ( -- * Creating a Request
    CreateDocumentationVersion (..),
    newCreateDocumentationVersion,

    -- * Request Lenses
    createDocumentationVersion_description,
    createDocumentationVersion_stageName,
    createDocumentationVersion_restApiId,
    createDocumentationVersion_documentationVersion,

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

-- | Creates a new documentation version of a given API.
--
-- /See:/ 'newCreateDocumentationVersion' smart constructor.
data CreateDocumentationVersion = CreateDocumentationVersion'
  { -- | A description about the new documentation snapshot.
    description :: Prelude.Maybe Prelude.Text,
    -- | The stage name to be associated with the new documentation snapshot.
    stageName :: Prelude.Maybe Prelude.Text,
    -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The version identifier of the new snapshot.
    documentationVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDocumentationVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createDocumentationVersion_description' - A description about the new documentation snapshot.
--
-- 'stageName', 'createDocumentationVersion_stageName' - The stage name to be associated with the new documentation snapshot.
--
-- 'restApiId', 'createDocumentationVersion_restApiId' - The string identifier of the associated RestApi.
--
-- 'documentationVersion', 'createDocumentationVersion_documentationVersion' - The version identifier of the new snapshot.
newCreateDocumentationVersion ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'documentationVersion'
  Prelude.Text ->
  CreateDocumentationVersion
newCreateDocumentationVersion
  pRestApiId_
  pDocumentationVersion_ =
    CreateDocumentationVersion'
      { description =
          Prelude.Nothing,
        stageName = Prelude.Nothing,
        restApiId = pRestApiId_,
        documentationVersion = pDocumentationVersion_
      }

-- | A description about the new documentation snapshot.
createDocumentationVersion_description :: Lens.Lens' CreateDocumentationVersion (Prelude.Maybe Prelude.Text)
createDocumentationVersion_description = Lens.lens (\CreateDocumentationVersion' {description} -> description) (\s@CreateDocumentationVersion' {} a -> s {description = a} :: CreateDocumentationVersion)

-- | The stage name to be associated with the new documentation snapshot.
createDocumentationVersion_stageName :: Lens.Lens' CreateDocumentationVersion (Prelude.Maybe Prelude.Text)
createDocumentationVersion_stageName = Lens.lens (\CreateDocumentationVersion' {stageName} -> stageName) (\s@CreateDocumentationVersion' {} a -> s {stageName = a} :: CreateDocumentationVersion)

-- | The string identifier of the associated RestApi.
createDocumentationVersion_restApiId :: Lens.Lens' CreateDocumentationVersion Prelude.Text
createDocumentationVersion_restApiId = Lens.lens (\CreateDocumentationVersion' {restApiId} -> restApiId) (\s@CreateDocumentationVersion' {} a -> s {restApiId = a} :: CreateDocumentationVersion)

-- | The version identifier of the new snapshot.
createDocumentationVersion_documentationVersion :: Lens.Lens' CreateDocumentationVersion Prelude.Text
createDocumentationVersion_documentationVersion = Lens.lens (\CreateDocumentationVersion' {documentationVersion} -> documentationVersion) (\s@CreateDocumentationVersion' {} a -> s {documentationVersion = a} :: CreateDocumentationVersion)

instance Core.AWSRequest CreateDocumentationVersion where
  type
    AWSResponse CreateDocumentationVersion =
      DocumentationVersion
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable CreateDocumentationVersion where
  hashWithSalt _salt CreateDocumentationVersion' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` stageName
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` documentationVersion

instance Prelude.NFData CreateDocumentationVersion where
  rnf CreateDocumentationVersion' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf stageName
      `Prelude.seq` Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf documentationVersion

instance Data.ToHeaders CreateDocumentationVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToJSON CreateDocumentationVersion where
  toJSON CreateDocumentationVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("stageName" Data..=) Prelude.<$> stageName,
            Prelude.Just
              ( "documentationVersion"
                  Data..= documentationVersion
              )
          ]
      )

instance Data.ToPath CreateDocumentationVersion where
  toPath CreateDocumentationVersion' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/documentation/versions"
      ]

instance Data.ToQuery CreateDocumentationVersion where
  toQuery = Prelude.const Prelude.mempty
