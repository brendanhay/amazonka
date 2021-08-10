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
-- Module      : Network.AWS.APIGateway.ImportDocumentationParts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Network.AWS.APIGateway.ImportDocumentationParts
  ( -- * Creating a Request
    ImportDocumentationParts (..),
    newImportDocumentationParts,

    -- * Request Lenses
    importDocumentationParts_mode,
    importDocumentationParts_failOnWarnings,
    importDocumentationParts_restApiId,
    importDocumentationParts_body,

    -- * Destructuring the Response
    ImportDocumentationPartsResponse (..),
    newImportDocumentationPartsResponse,

    -- * Response Lenses
    importDocumentationPartsResponse_warnings,
    importDocumentationPartsResponse_ids,
    importDocumentationPartsResponse_httpStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Import documentation parts from an external (e.g., OpenAPI) definition
-- file.
--
-- /See:/ 'newImportDocumentationParts' smart constructor.
data ImportDocumentationParts = ImportDocumentationParts'
  { -- | A query parameter to indicate whether to overwrite (@OVERWRITE@) any
    -- existing DocumentationParts definition or to merge (@MERGE@) the new
    -- definition into the existing one. The default value is @MERGE@.
    mode :: Prelude.Maybe PutMode,
    -- | A query parameter to specify whether to rollback the documentation
    -- importation (@true@) or not (@false@) when a warning is encountered. The
    -- default value is @false@.
    failOnWarnings :: Prelude.Maybe Prelude.Bool,
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | [Required] Raw byte array representing the to-be-imported documentation
    -- parts. To import from an OpenAPI file, this is a JSON object.
    body :: Prelude.ByteString
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportDocumentationParts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mode', 'importDocumentationParts_mode' - A query parameter to indicate whether to overwrite (@OVERWRITE@) any
-- existing DocumentationParts definition or to merge (@MERGE@) the new
-- definition into the existing one. The default value is @MERGE@.
--
-- 'failOnWarnings', 'importDocumentationParts_failOnWarnings' - A query parameter to specify whether to rollback the documentation
-- importation (@true@) or not (@false@) when a warning is encountered. The
-- default value is @false@.
--
-- 'restApiId', 'importDocumentationParts_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'body', 'importDocumentationParts_body' - [Required] Raw byte array representing the to-be-imported documentation
-- parts. To import from an OpenAPI file, this is a JSON object.
newImportDocumentationParts ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'body'
  Prelude.ByteString ->
  ImportDocumentationParts
newImportDocumentationParts pRestApiId_ pBody_ =
  ImportDocumentationParts'
    { mode = Prelude.Nothing,
      failOnWarnings = Prelude.Nothing,
      restApiId = pRestApiId_,
      body = pBody_
    }

-- | A query parameter to indicate whether to overwrite (@OVERWRITE@) any
-- existing DocumentationParts definition or to merge (@MERGE@) the new
-- definition into the existing one. The default value is @MERGE@.
importDocumentationParts_mode :: Lens.Lens' ImportDocumentationParts (Prelude.Maybe PutMode)
importDocumentationParts_mode = Lens.lens (\ImportDocumentationParts' {mode} -> mode) (\s@ImportDocumentationParts' {} a -> s {mode = a} :: ImportDocumentationParts)

-- | A query parameter to specify whether to rollback the documentation
-- importation (@true@) or not (@false@) when a warning is encountered. The
-- default value is @false@.
importDocumentationParts_failOnWarnings :: Lens.Lens' ImportDocumentationParts (Prelude.Maybe Prelude.Bool)
importDocumentationParts_failOnWarnings = Lens.lens (\ImportDocumentationParts' {failOnWarnings} -> failOnWarnings) (\s@ImportDocumentationParts' {} a -> s {failOnWarnings = a} :: ImportDocumentationParts)

-- | [Required] The string identifier of the associated RestApi.
importDocumentationParts_restApiId :: Lens.Lens' ImportDocumentationParts Prelude.Text
importDocumentationParts_restApiId = Lens.lens (\ImportDocumentationParts' {restApiId} -> restApiId) (\s@ImportDocumentationParts' {} a -> s {restApiId = a} :: ImportDocumentationParts)

-- | [Required] Raw byte array representing the to-be-imported documentation
-- parts. To import from an OpenAPI file, this is a JSON object.
importDocumentationParts_body :: Lens.Lens' ImportDocumentationParts Prelude.ByteString
importDocumentationParts_body = Lens.lens (\ImportDocumentationParts' {body} -> body) (\s@ImportDocumentationParts' {} a -> s {body = a} :: ImportDocumentationParts)

instance Core.AWSRequest ImportDocumentationParts where
  type
    AWSResponse ImportDocumentationParts =
      ImportDocumentationPartsResponse
  request = Request.putBody defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportDocumentationPartsResponse'
            Prelude.<$> (x Core..?> "warnings" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "ids" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportDocumentationParts

instance Prelude.NFData ImportDocumentationParts

instance Core.ToBody ImportDocumentationParts where
  toBody ImportDocumentationParts' {..} =
    Core.toBody body

instance Core.ToHeaders ImportDocumentationParts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToPath ImportDocumentationParts where
  toPath ImportDocumentationParts' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/documentation/parts"
      ]

instance Core.ToQuery ImportDocumentationParts where
  toQuery ImportDocumentationParts' {..} =
    Prelude.mconcat
      [ "mode" Core.=: mode,
        "failonwarnings" Core.=: failOnWarnings
      ]

-- | A collection of the imported DocumentationPart identifiers.
--
-- This is used to return the result when documentation parts in an
-- external (e.g., OpenAPI) file are imported into API Gateway
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-documenting-api.html Documenting an API>,
-- <https://docs.aws.amazon.com/apigateway/api-reference/link-relation/documentationpart-import/ documentationpart:import>,
-- DocumentationPart
--
-- /See:/ 'newImportDocumentationPartsResponse' smart constructor.
data ImportDocumentationPartsResponse = ImportDocumentationPartsResponse'
  { -- | A list of warning messages reported during import of documentation
    -- parts.
    warnings :: Prelude.Maybe [Prelude.Text],
    -- | A list of the returned documentation part identifiers.
    ids :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportDocumentationPartsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'warnings', 'importDocumentationPartsResponse_warnings' - A list of warning messages reported during import of documentation
-- parts.
--
-- 'ids', 'importDocumentationPartsResponse_ids' - A list of the returned documentation part identifiers.
--
-- 'httpStatus', 'importDocumentationPartsResponse_httpStatus' - The response's http status code.
newImportDocumentationPartsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ImportDocumentationPartsResponse
newImportDocumentationPartsResponse pHttpStatus_ =
  ImportDocumentationPartsResponse'
    { warnings =
        Prelude.Nothing,
      ids = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of warning messages reported during import of documentation
-- parts.
importDocumentationPartsResponse_warnings :: Lens.Lens' ImportDocumentationPartsResponse (Prelude.Maybe [Prelude.Text])
importDocumentationPartsResponse_warnings = Lens.lens (\ImportDocumentationPartsResponse' {warnings} -> warnings) (\s@ImportDocumentationPartsResponse' {} a -> s {warnings = a} :: ImportDocumentationPartsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A list of the returned documentation part identifiers.
importDocumentationPartsResponse_ids :: Lens.Lens' ImportDocumentationPartsResponse (Prelude.Maybe [Prelude.Text])
importDocumentationPartsResponse_ids = Lens.lens (\ImportDocumentationPartsResponse' {ids} -> ids) (\s@ImportDocumentationPartsResponse' {} a -> s {ids = a} :: ImportDocumentationPartsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
importDocumentationPartsResponse_httpStatus :: Lens.Lens' ImportDocumentationPartsResponse Prelude.Int
importDocumentationPartsResponse_httpStatus = Lens.lens (\ImportDocumentationPartsResponse' {httpStatus} -> httpStatus) (\s@ImportDocumentationPartsResponse' {} a -> s {httpStatus = a} :: ImportDocumentationPartsResponse)

instance
  Prelude.NFData
    ImportDocumentationPartsResponse
