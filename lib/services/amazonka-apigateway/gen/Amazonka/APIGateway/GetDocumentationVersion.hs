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
-- Module      : Amazonka.APIGateway.GetDocumentationVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a documentation version.
module Amazonka.APIGateway.GetDocumentationVersion
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

-- | Gets a documentation snapshot of an API.
--
-- /See:/ 'newGetDocumentationVersion' smart constructor.
data GetDocumentationVersion = GetDocumentationVersion'
  { -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The version identifier of the to-be-retrieved documentation snapshot.
    documentationVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDocumentationVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'getDocumentationVersion_restApiId' - The string identifier of the associated RestApi.
--
-- 'documentationVersion', 'getDocumentationVersion_documentationVersion' - The version identifier of the to-be-retrieved documentation snapshot.
newGetDocumentationVersion ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'documentationVersion'
  Prelude.Text ->
  GetDocumentationVersion
newGetDocumentationVersion
  pRestApiId_
  pDocumentationVersion_ =
    GetDocumentationVersion'
      { restApiId = pRestApiId_,
        documentationVersion = pDocumentationVersion_
      }

-- | The string identifier of the associated RestApi.
getDocumentationVersion_restApiId :: Lens.Lens' GetDocumentationVersion Prelude.Text
getDocumentationVersion_restApiId = Lens.lens (\GetDocumentationVersion' {restApiId} -> restApiId) (\s@GetDocumentationVersion' {} a -> s {restApiId = a} :: GetDocumentationVersion)

-- | The version identifier of the to-be-retrieved documentation snapshot.
getDocumentationVersion_documentationVersion :: Lens.Lens' GetDocumentationVersion Prelude.Text
getDocumentationVersion_documentationVersion = Lens.lens (\GetDocumentationVersion' {documentationVersion} -> documentationVersion) (\s@GetDocumentationVersion' {} a -> s {documentationVersion = a} :: GetDocumentationVersion)

instance Core.AWSRequest GetDocumentationVersion where
  type
    AWSResponse GetDocumentationVersion =
      DocumentationVersion
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable GetDocumentationVersion where
  hashWithSalt _salt GetDocumentationVersion' {..} =
    _salt `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` documentationVersion

instance Prelude.NFData GetDocumentationVersion where
  rnf GetDocumentationVersion' {..} =
    Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf documentationVersion

instance Data.ToHeaders GetDocumentationVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath GetDocumentationVersion where
  toPath GetDocumentationVersion' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/documentation/versions/",
        Data.toBS documentationVersion
      ]

instance Data.ToQuery GetDocumentationVersion where
  toQuery = Prelude.const Prelude.mempty
