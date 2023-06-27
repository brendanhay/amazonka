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
-- Module      : Amazonka.OsIs.ListPipelineBlueprints
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of all available blueprints for Data Prepper. For more
-- information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/creating-pipeline.html#pipeline-blueprint Using blueprints to create a pipeline>.
module Amazonka.OsIs.ListPipelineBlueprints
  ( -- * Creating a Request
    ListPipelineBlueprints (..),
    newListPipelineBlueprints,

    -- * Destructuring the Response
    ListPipelineBlueprintsResponse (..),
    newListPipelineBlueprintsResponse,

    -- * Response Lenses
    listPipelineBlueprintsResponse_blueprints,
    listPipelineBlueprintsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OsIs.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPipelineBlueprints' smart constructor.
data ListPipelineBlueprints = ListPipelineBlueprints'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPipelineBlueprints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newListPipelineBlueprints ::
  ListPipelineBlueprints
newListPipelineBlueprints = ListPipelineBlueprints'

instance Core.AWSRequest ListPipelineBlueprints where
  type
    AWSResponse ListPipelineBlueprints =
      ListPipelineBlueprintsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPipelineBlueprintsResponse'
            Prelude.<$> (x Data..?> "Blueprints" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPipelineBlueprints where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData ListPipelineBlueprints where
  rnf _ = ()

instance Data.ToHeaders ListPipelineBlueprints where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON ListPipelineBlueprints where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath ListPipelineBlueprints where
  toPath =
    Prelude.const
      "/2022-01-01/osis/listPipelineBlueprints"

instance Data.ToQuery ListPipelineBlueprints where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPipelineBlueprintsResponse' smart constructor.
data ListPipelineBlueprintsResponse = ListPipelineBlueprintsResponse'
  { -- | A list of available blueprints for Data Prepper.
    blueprints :: Prelude.Maybe [PipelineBlueprintSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPipelineBlueprintsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blueprints', 'listPipelineBlueprintsResponse_blueprints' - A list of available blueprints for Data Prepper.
--
-- 'httpStatus', 'listPipelineBlueprintsResponse_httpStatus' - The response's http status code.
newListPipelineBlueprintsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPipelineBlueprintsResponse
newListPipelineBlueprintsResponse pHttpStatus_ =
  ListPipelineBlueprintsResponse'
    { blueprints =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of available blueprints for Data Prepper.
listPipelineBlueprintsResponse_blueprints :: Lens.Lens' ListPipelineBlueprintsResponse (Prelude.Maybe [PipelineBlueprintSummary])
listPipelineBlueprintsResponse_blueprints = Lens.lens (\ListPipelineBlueprintsResponse' {blueprints} -> blueprints) (\s@ListPipelineBlueprintsResponse' {} a -> s {blueprints = a} :: ListPipelineBlueprintsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPipelineBlueprintsResponse_httpStatus :: Lens.Lens' ListPipelineBlueprintsResponse Prelude.Int
listPipelineBlueprintsResponse_httpStatus = Lens.lens (\ListPipelineBlueprintsResponse' {httpStatus} -> httpStatus) (\s@ListPipelineBlueprintsResponse' {} a -> s {httpStatus = a} :: ListPipelineBlueprintsResponse)

instance
  Prelude.NFData
    ListPipelineBlueprintsResponse
  where
  rnf ListPipelineBlueprintsResponse' {..} =
    Prelude.rnf blueprints
      `Prelude.seq` Prelude.rnf httpStatus
