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
-- Module      : Network.AWS.APIGateway.GetDocumentationPart
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Network.AWS.APIGateway.GetDocumentationPart
  ( -- * Creating a Request
    GetDocumentationPart (..),
    newGetDocumentationPart,

    -- * Request Lenses
    getDocumentationPart_restApiId,
    getDocumentationPart_documentationPartId,

    -- * Destructuring the Response
    DocumentationPart (..),
    newDocumentationPart,

    -- * Response Lenses
    documentationPart_id,
    documentationPart_properties,
    documentationPart_location,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Gets a specified documentation part of a given API.
--
-- /See:/ 'newGetDocumentationPart' smart constructor.
data GetDocumentationPart = GetDocumentationPart'
  { -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | [Required] The string identifier of the associated RestApi.
    documentationPartId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDocumentationPart' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'getDocumentationPart_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'documentationPartId', 'getDocumentationPart_documentationPartId' - [Required] The string identifier of the associated RestApi.
newGetDocumentationPart ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'documentationPartId'
  Prelude.Text ->
  GetDocumentationPart
newGetDocumentationPart
  pRestApiId_
  pDocumentationPartId_ =
    GetDocumentationPart'
      { restApiId = pRestApiId_,
        documentationPartId = pDocumentationPartId_
      }

-- | [Required] The string identifier of the associated RestApi.
getDocumentationPart_restApiId :: Lens.Lens' GetDocumentationPart Prelude.Text
getDocumentationPart_restApiId = Lens.lens (\GetDocumentationPart' {restApiId} -> restApiId) (\s@GetDocumentationPart' {} a -> s {restApiId = a} :: GetDocumentationPart)

-- | [Required] The string identifier of the associated RestApi.
getDocumentationPart_documentationPartId :: Lens.Lens' GetDocumentationPart Prelude.Text
getDocumentationPart_documentationPartId = Lens.lens (\GetDocumentationPart' {documentationPartId} -> documentationPartId) (\s@GetDocumentationPart' {} a -> s {documentationPartId = a} :: GetDocumentationPart)

instance Core.AWSRequest GetDocumentationPart where
  type
    AWSResponse GetDocumentationPart =
      DocumentationPart
  request = Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable GetDocumentationPart

instance Prelude.NFData GetDocumentationPart

instance Core.ToHeaders GetDocumentationPart where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToPath GetDocumentationPart where
  toPath GetDocumentationPart' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/documentation/parts/",
        Core.toBS documentationPartId
      ]

instance Core.ToQuery GetDocumentationPart where
  toQuery = Prelude.const Prelude.mempty
