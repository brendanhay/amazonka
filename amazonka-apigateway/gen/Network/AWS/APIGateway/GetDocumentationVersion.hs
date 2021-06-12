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
-- Module      : Network.AWS.APIGateway.GetDocumentationVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Network.AWS.APIGateway.GetDocumentationVersion
  ( -- * Creating a Request
    GetDocumentationVersion (..),
    newGetDocumentationVersion,

    -- * Request Lenses
    getDocumentationVersion_restApiId,
    getDocumentationVersion_documentationVersion,

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

-- | Gets a documentation snapshot of an API.
--
-- /See:/ 'newGetDocumentationVersion' smart constructor.
data GetDocumentationVersion = GetDocumentationVersion'
  { -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Core.Text,
    -- | [Required] The version identifier of the to-be-retrieved documentation
    -- snapshot.
    documentationVersion :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDocumentationVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'getDocumentationVersion_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'documentationVersion', 'getDocumentationVersion_documentationVersion' - [Required] The version identifier of the to-be-retrieved documentation
-- snapshot.
newGetDocumentationVersion ::
  -- | 'restApiId'
  Core.Text ->
  -- | 'documentationVersion'
  Core.Text ->
  GetDocumentationVersion
newGetDocumentationVersion
  pRestApiId_
  pDocumentationVersion_ =
    GetDocumentationVersion'
      { restApiId = pRestApiId_,
        documentationVersion = pDocumentationVersion_
      }

-- | [Required] The string identifier of the associated RestApi.
getDocumentationVersion_restApiId :: Lens.Lens' GetDocumentationVersion Core.Text
getDocumentationVersion_restApiId = Lens.lens (\GetDocumentationVersion' {restApiId} -> restApiId) (\s@GetDocumentationVersion' {} a -> s {restApiId = a} :: GetDocumentationVersion)

-- | [Required] The version identifier of the to-be-retrieved documentation
-- snapshot.
getDocumentationVersion_documentationVersion :: Lens.Lens' GetDocumentationVersion Core.Text
getDocumentationVersion_documentationVersion = Lens.lens (\GetDocumentationVersion' {documentationVersion} -> documentationVersion) (\s@GetDocumentationVersion' {} a -> s {documentationVersion = a} :: GetDocumentationVersion)

instance Core.AWSRequest GetDocumentationVersion where
  type
    AWSResponse GetDocumentationVersion =
      DocumentationVersion
  request = Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable GetDocumentationVersion

instance Core.NFData GetDocumentationVersion

instance Core.ToHeaders GetDocumentationVersion where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetDocumentationVersion where
  toPath GetDocumentationVersion' {..} =
    Core.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/documentation/versions/",
        Core.toBS documentationVersion
      ]

instance Core.ToQuery GetDocumentationVersion where
  toQuery = Core.const Core.mempty
