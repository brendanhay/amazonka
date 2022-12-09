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
-- Module      : Amazonka.APIGateway.GetDocumentationPart
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a documentation part.
module Amazonka.APIGateway.GetDocumentationPart
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

-- | Gets a specified documentation part of a given API.
--
-- /See:/ 'newGetDocumentationPart' smart constructor.
data GetDocumentationPart = GetDocumentationPart'
  { -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The string identifier of the associated RestApi.
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
-- 'restApiId', 'getDocumentationPart_restApiId' - The string identifier of the associated RestApi.
--
-- 'documentationPartId', 'getDocumentationPart_documentationPartId' - The string identifier of the associated RestApi.
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

-- | The string identifier of the associated RestApi.
getDocumentationPart_restApiId :: Lens.Lens' GetDocumentationPart Prelude.Text
getDocumentationPart_restApiId = Lens.lens (\GetDocumentationPart' {restApiId} -> restApiId) (\s@GetDocumentationPart' {} a -> s {restApiId = a} :: GetDocumentationPart)

-- | The string identifier of the associated RestApi.
getDocumentationPart_documentationPartId :: Lens.Lens' GetDocumentationPart Prelude.Text
getDocumentationPart_documentationPartId = Lens.lens (\GetDocumentationPart' {documentationPartId} -> documentationPartId) (\s@GetDocumentationPart' {} a -> s {documentationPartId = a} :: GetDocumentationPart)

instance Core.AWSRequest GetDocumentationPart where
  type
    AWSResponse GetDocumentationPart =
      DocumentationPart
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable GetDocumentationPart where
  hashWithSalt _salt GetDocumentationPart' {..} =
    _salt `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` documentationPartId

instance Prelude.NFData GetDocumentationPart where
  rnf GetDocumentationPart' {..} =
    Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf documentationPartId

instance Data.ToHeaders GetDocumentationPart where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath GetDocumentationPart where
  toPath GetDocumentationPart' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/documentation/parts/",
        Data.toBS documentationPartId
      ]

instance Data.ToQuery GetDocumentationPart where
  toQuery = Prelude.const Prelude.mempty
