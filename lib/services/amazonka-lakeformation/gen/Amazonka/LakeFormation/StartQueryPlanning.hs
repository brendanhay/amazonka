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
-- Module      : Amazonka.LakeFormation.StartQueryPlanning
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Submits a request to process a query statement.
--
-- This operation generates work units that can be retrieved with the
-- @GetWorkUnits@ operation as soon as the query state is
-- WORKUNITS_AVAILABLE or FINISHED.
module Amazonka.LakeFormation.StartQueryPlanning
  ( -- * Creating a Request
    StartQueryPlanning (..),
    newStartQueryPlanning,

    -- * Request Lenses
    startQueryPlanning_queryPlanningContext,
    startQueryPlanning_queryString,

    -- * Destructuring the Response
    StartQueryPlanningResponse (..),
    newStartQueryPlanningResponse,

    -- * Response Lenses
    startQueryPlanningResponse_httpStatus,
    startQueryPlanningResponse_queryId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartQueryPlanning' smart constructor.
data StartQueryPlanning = StartQueryPlanning'
  { -- | A structure containing information about the query plan.
    queryPlanningContext :: QueryPlanningContext,
    -- | A PartiQL query statement used as an input to the planner service.
    queryString :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartQueryPlanning' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryPlanningContext', 'startQueryPlanning_queryPlanningContext' - A structure containing information about the query plan.
--
-- 'queryString', 'startQueryPlanning_queryString' - A PartiQL query statement used as an input to the planner service.
newStartQueryPlanning ::
  -- | 'queryPlanningContext'
  QueryPlanningContext ->
  -- | 'queryString'
  Prelude.Text ->
  StartQueryPlanning
newStartQueryPlanning
  pQueryPlanningContext_
  pQueryString_ =
    StartQueryPlanning'
      { queryPlanningContext =
          pQueryPlanningContext_,
        queryString = Core._Sensitive Lens.# pQueryString_
      }

-- | A structure containing information about the query plan.
startQueryPlanning_queryPlanningContext :: Lens.Lens' StartQueryPlanning QueryPlanningContext
startQueryPlanning_queryPlanningContext = Lens.lens (\StartQueryPlanning' {queryPlanningContext} -> queryPlanningContext) (\s@StartQueryPlanning' {} a -> s {queryPlanningContext = a} :: StartQueryPlanning)

-- | A PartiQL query statement used as an input to the planner service.
startQueryPlanning_queryString :: Lens.Lens' StartQueryPlanning Prelude.Text
startQueryPlanning_queryString = Lens.lens (\StartQueryPlanning' {queryString} -> queryString) (\s@StartQueryPlanning' {} a -> s {queryString = a} :: StartQueryPlanning) Prelude.. Core._Sensitive

instance Core.AWSRequest StartQueryPlanning where
  type
    AWSResponse StartQueryPlanning =
      StartQueryPlanningResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartQueryPlanningResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "QueryId")
      )

instance Prelude.Hashable StartQueryPlanning where
  hashWithSalt _salt StartQueryPlanning' {..} =
    _salt `Prelude.hashWithSalt` queryPlanningContext
      `Prelude.hashWithSalt` queryString

instance Prelude.NFData StartQueryPlanning where
  rnf StartQueryPlanning' {..} =
    Prelude.rnf queryPlanningContext
      `Prelude.seq` Prelude.rnf queryString

instance Core.ToHeaders StartQueryPlanning where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartQueryPlanning where
  toJSON StartQueryPlanning' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "QueryPlanningContext"
                  Core..= queryPlanningContext
              ),
            Prelude.Just ("QueryString" Core..= queryString)
          ]
      )

instance Core.ToPath StartQueryPlanning where
  toPath = Prelude.const "/StartQueryPlanning"

instance Core.ToQuery StartQueryPlanning where
  toQuery = Prelude.const Prelude.mempty

-- | A structure for the output.
--
-- /See:/ 'newStartQueryPlanningResponse' smart constructor.
data StartQueryPlanningResponse = StartQueryPlanningResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the plan query operation can be used to fetch the actual work
    -- unit descriptors that are produced as the result of the operation. The
    -- ID is also used to get the query state and as an input to the @Execute@
    -- operation.
    queryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartQueryPlanningResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startQueryPlanningResponse_httpStatus' - The response's http status code.
--
-- 'queryId', 'startQueryPlanningResponse_queryId' - The ID of the plan query operation can be used to fetch the actual work
-- unit descriptors that are produced as the result of the operation. The
-- ID is also used to get the query state and as an input to the @Execute@
-- operation.
newStartQueryPlanningResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'queryId'
  Prelude.Text ->
  StartQueryPlanningResponse
newStartQueryPlanningResponse pHttpStatus_ pQueryId_ =
  StartQueryPlanningResponse'
    { httpStatus =
        pHttpStatus_,
      queryId = pQueryId_
    }

-- | The response's http status code.
startQueryPlanningResponse_httpStatus :: Lens.Lens' StartQueryPlanningResponse Prelude.Int
startQueryPlanningResponse_httpStatus = Lens.lens (\StartQueryPlanningResponse' {httpStatus} -> httpStatus) (\s@StartQueryPlanningResponse' {} a -> s {httpStatus = a} :: StartQueryPlanningResponse)

-- | The ID of the plan query operation can be used to fetch the actual work
-- unit descriptors that are produced as the result of the operation. The
-- ID is also used to get the query state and as an input to the @Execute@
-- operation.
startQueryPlanningResponse_queryId :: Lens.Lens' StartQueryPlanningResponse Prelude.Text
startQueryPlanningResponse_queryId = Lens.lens (\StartQueryPlanningResponse' {queryId} -> queryId) (\s@StartQueryPlanningResponse' {} a -> s {queryId = a} :: StartQueryPlanningResponse)

instance Prelude.NFData StartQueryPlanningResponse where
  rnf StartQueryPlanningResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf queryId
