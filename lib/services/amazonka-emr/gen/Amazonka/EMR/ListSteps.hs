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
-- Module      : Amazonka.EMR.ListSteps
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of steps for the cluster in reverse order unless you
-- specify @stepIds@ with the request or filter by @StepStates@. You can
-- specify a maximum of 10 @stepIDs@. The CLI automatically paginates
-- results to return a list greater than 50 steps. To return more than 50
-- steps using the CLI, specify a @Marker@, which is a pagination token
-- that indicates the next set of steps to retrieve.
--
-- This operation returns paginated results.
module Amazonka.EMR.ListSteps
  ( -- * Creating a Request
    ListSteps (..),
    newListSteps,

    -- * Request Lenses
    listSteps_marker,
    listSteps_stepIds,
    listSteps_stepStates,
    listSteps_clusterId,

    -- * Destructuring the Response
    ListStepsResponse (..),
    newListStepsResponse,

    -- * Response Lenses
    listStepsResponse_marker,
    listStepsResponse_steps,
    listStepsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | This input determines which steps to list.
--
-- /See:/ 'newListSteps' smart constructor.
data ListSteps = ListSteps'
  { -- | The maximum number of steps that a single @ListSteps@ action returns is
    -- 50. To return a longer list of steps, use multiple @ListSteps@ actions
    -- along with the @Marker@ parameter, which is a pagination token that
    -- indicates the next set of results to retrieve.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The filter to limit the step list based on the identifier of the steps.
    -- You can specify a maximum of ten Step IDs. The character constraint
    -- applies to the overall length of the array.
    stepIds :: Prelude.Maybe [Prelude.Text],
    -- | The filter to limit the step list based on certain states.
    stepStates :: Prelude.Maybe [StepState],
    -- | The identifier of the cluster for which to list the steps.
    clusterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSteps' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listSteps_marker' - The maximum number of steps that a single @ListSteps@ action returns is
-- 50. To return a longer list of steps, use multiple @ListSteps@ actions
-- along with the @Marker@ parameter, which is a pagination token that
-- indicates the next set of results to retrieve.
--
-- 'stepIds', 'listSteps_stepIds' - The filter to limit the step list based on the identifier of the steps.
-- You can specify a maximum of ten Step IDs. The character constraint
-- applies to the overall length of the array.
--
-- 'stepStates', 'listSteps_stepStates' - The filter to limit the step list based on certain states.
--
-- 'clusterId', 'listSteps_clusterId' - The identifier of the cluster for which to list the steps.
newListSteps ::
  -- | 'clusterId'
  Prelude.Text ->
  ListSteps
newListSteps pClusterId_ =
  ListSteps'
    { marker = Prelude.Nothing,
      stepIds = Prelude.Nothing,
      stepStates = Prelude.Nothing,
      clusterId = pClusterId_
    }

-- | The maximum number of steps that a single @ListSteps@ action returns is
-- 50. To return a longer list of steps, use multiple @ListSteps@ actions
-- along with the @Marker@ parameter, which is a pagination token that
-- indicates the next set of results to retrieve.
listSteps_marker :: Lens.Lens' ListSteps (Prelude.Maybe Prelude.Text)
listSteps_marker = Lens.lens (\ListSteps' {marker} -> marker) (\s@ListSteps' {} a -> s {marker = a} :: ListSteps)

-- | The filter to limit the step list based on the identifier of the steps.
-- You can specify a maximum of ten Step IDs. The character constraint
-- applies to the overall length of the array.
listSteps_stepIds :: Lens.Lens' ListSteps (Prelude.Maybe [Prelude.Text])
listSteps_stepIds = Lens.lens (\ListSteps' {stepIds} -> stepIds) (\s@ListSteps' {} a -> s {stepIds = a} :: ListSteps) Prelude.. Lens.mapping Lens.coerced

-- | The filter to limit the step list based on certain states.
listSteps_stepStates :: Lens.Lens' ListSteps (Prelude.Maybe [StepState])
listSteps_stepStates = Lens.lens (\ListSteps' {stepStates} -> stepStates) (\s@ListSteps' {} a -> s {stepStates = a} :: ListSteps) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the cluster for which to list the steps.
listSteps_clusterId :: Lens.Lens' ListSteps Prelude.Text
listSteps_clusterId = Lens.lens (\ListSteps' {clusterId} -> clusterId) (\s@ListSteps' {} a -> s {clusterId = a} :: ListSteps)

instance Core.AWSPager ListSteps where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listStepsResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listStepsResponse_steps
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listSteps_marker
          Lens..~ rs
          Lens.^? listStepsResponse_marker
          Prelude.. Lens._Just

instance Core.AWSRequest ListSteps where
  type AWSResponse ListSteps = ListStepsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStepsResponse'
            Prelude.<$> (x Data..?> "Marker")
            Prelude.<*> (x Data..?> "Steps" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSteps where
  hashWithSalt _salt ListSteps' {..} =
    _salt
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` stepIds
      `Prelude.hashWithSalt` stepStates
      `Prelude.hashWithSalt` clusterId

instance Prelude.NFData ListSteps where
  rnf ListSteps' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf stepIds
      `Prelude.seq` Prelude.rnf stepStates
      `Prelude.seq` Prelude.rnf clusterId

instance Data.ToHeaders ListSteps where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("ElasticMapReduce.ListSteps" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListSteps where
  toJSON ListSteps' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Marker" Data..=) Prelude.<$> marker,
            ("StepIds" Data..=) Prelude.<$> stepIds,
            ("StepStates" Data..=) Prelude.<$> stepStates,
            Prelude.Just ("ClusterId" Data..= clusterId)
          ]
      )

instance Data.ToPath ListSteps where
  toPath = Prelude.const "/"

instance Data.ToQuery ListSteps where
  toQuery = Prelude.const Prelude.mempty

-- | This output contains the list of steps returned in reverse order. This
-- means that the last step is the first element in the list.
--
-- /See:/ 'newListStepsResponse' smart constructor.
data ListStepsResponse = ListStepsResponse'
  { -- | The maximum number of steps that a single @ListSteps@ action returns is
    -- 50. To return a longer list of steps, use multiple @ListSteps@ actions
    -- along with the @Marker@ parameter, which is a pagination token that
    -- indicates the next set of results to retrieve.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The filtered list of steps for the cluster.
    steps :: Prelude.Maybe [StepSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStepsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listStepsResponse_marker' - The maximum number of steps that a single @ListSteps@ action returns is
-- 50. To return a longer list of steps, use multiple @ListSteps@ actions
-- along with the @Marker@ parameter, which is a pagination token that
-- indicates the next set of results to retrieve.
--
-- 'steps', 'listStepsResponse_steps' - The filtered list of steps for the cluster.
--
-- 'httpStatus', 'listStepsResponse_httpStatus' - The response's http status code.
newListStepsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListStepsResponse
newListStepsResponse pHttpStatus_ =
  ListStepsResponse'
    { marker = Prelude.Nothing,
      steps = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The maximum number of steps that a single @ListSteps@ action returns is
-- 50. To return a longer list of steps, use multiple @ListSteps@ actions
-- along with the @Marker@ parameter, which is a pagination token that
-- indicates the next set of results to retrieve.
listStepsResponse_marker :: Lens.Lens' ListStepsResponse (Prelude.Maybe Prelude.Text)
listStepsResponse_marker = Lens.lens (\ListStepsResponse' {marker} -> marker) (\s@ListStepsResponse' {} a -> s {marker = a} :: ListStepsResponse)

-- | The filtered list of steps for the cluster.
listStepsResponse_steps :: Lens.Lens' ListStepsResponse (Prelude.Maybe [StepSummary])
listStepsResponse_steps = Lens.lens (\ListStepsResponse' {steps} -> steps) (\s@ListStepsResponse' {} a -> s {steps = a} :: ListStepsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listStepsResponse_httpStatus :: Lens.Lens' ListStepsResponse Prelude.Int
listStepsResponse_httpStatus = Lens.lens (\ListStepsResponse' {httpStatus} -> httpStatus) (\s@ListStepsResponse' {} a -> s {httpStatus = a} :: ListStepsResponse)

instance Prelude.NFData ListStepsResponse where
  rnf ListStepsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf steps
      `Prelude.seq` Prelude.rnf httpStatus
