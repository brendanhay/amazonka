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
-- Module      : Amazonka.OsIs.GetPipelineBlueprint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a specific blueprint for OpenSearch
-- Ingestion. Blueprints are templates for the configuration needed for a
-- @CreatePipeline@ request. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/creating-pipeline.html#pipeline-blueprint Using blueprints to create a pipeline>.
module Amazonka.OsIs.GetPipelineBlueprint
  ( -- * Creating a Request
    GetPipelineBlueprint (..),
    newGetPipelineBlueprint,

    -- * Request Lenses
    getPipelineBlueprint_blueprintName,

    -- * Destructuring the Response
    GetPipelineBlueprintResponse (..),
    newGetPipelineBlueprintResponse,

    -- * Response Lenses
    getPipelineBlueprintResponse_blueprint,
    getPipelineBlueprintResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OsIs.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPipelineBlueprint' smart constructor.
data GetPipelineBlueprint = GetPipelineBlueprint'
  { -- | The name of the blueprint to retrieve.
    blueprintName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPipelineBlueprint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blueprintName', 'getPipelineBlueprint_blueprintName' - The name of the blueprint to retrieve.
newGetPipelineBlueprint ::
  -- | 'blueprintName'
  Prelude.Text ->
  GetPipelineBlueprint
newGetPipelineBlueprint pBlueprintName_ =
  GetPipelineBlueprint'
    { blueprintName =
        pBlueprintName_
    }

-- | The name of the blueprint to retrieve.
getPipelineBlueprint_blueprintName :: Lens.Lens' GetPipelineBlueprint Prelude.Text
getPipelineBlueprint_blueprintName = Lens.lens (\GetPipelineBlueprint' {blueprintName} -> blueprintName) (\s@GetPipelineBlueprint' {} a -> s {blueprintName = a} :: GetPipelineBlueprint)

instance Core.AWSRequest GetPipelineBlueprint where
  type
    AWSResponse GetPipelineBlueprint =
      GetPipelineBlueprintResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPipelineBlueprintResponse'
            Prelude.<$> (x Data..?> "Blueprint")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPipelineBlueprint where
  hashWithSalt _salt GetPipelineBlueprint' {..} =
    _salt `Prelude.hashWithSalt` blueprintName

instance Prelude.NFData GetPipelineBlueprint where
  rnf GetPipelineBlueprint' {..} =
    Prelude.rnf blueprintName

instance Data.ToHeaders GetPipelineBlueprint where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetPipelineBlueprint where
  toPath GetPipelineBlueprint' {..} =
    Prelude.mconcat
      [ "/2022-01-01/osis/getPipelineBlueprint/",
        Data.toBS blueprintName
      ]

instance Data.ToQuery GetPipelineBlueprint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPipelineBlueprintResponse' smart constructor.
data GetPipelineBlueprintResponse = GetPipelineBlueprintResponse'
  { -- | The requested blueprint in YAML format.
    blueprint :: Prelude.Maybe PipelineBlueprint,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPipelineBlueprintResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blueprint', 'getPipelineBlueprintResponse_blueprint' - The requested blueprint in YAML format.
--
-- 'httpStatus', 'getPipelineBlueprintResponse_httpStatus' - The response's http status code.
newGetPipelineBlueprintResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPipelineBlueprintResponse
newGetPipelineBlueprintResponse pHttpStatus_ =
  GetPipelineBlueprintResponse'
    { blueprint =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The requested blueprint in YAML format.
getPipelineBlueprintResponse_blueprint :: Lens.Lens' GetPipelineBlueprintResponse (Prelude.Maybe PipelineBlueprint)
getPipelineBlueprintResponse_blueprint = Lens.lens (\GetPipelineBlueprintResponse' {blueprint} -> blueprint) (\s@GetPipelineBlueprintResponse' {} a -> s {blueprint = a} :: GetPipelineBlueprintResponse)

-- | The response's http status code.
getPipelineBlueprintResponse_httpStatus :: Lens.Lens' GetPipelineBlueprintResponse Prelude.Int
getPipelineBlueprintResponse_httpStatus = Lens.lens (\GetPipelineBlueprintResponse' {httpStatus} -> httpStatus) (\s@GetPipelineBlueprintResponse' {} a -> s {httpStatus = a} :: GetPipelineBlueprintResponse)

instance Prelude.NFData GetPipelineBlueprintResponse where
  rnf GetPipelineBlueprintResponse' {..} =
    Prelude.rnf blueprint
      `Prelude.seq` Prelude.rnf httpStatus
