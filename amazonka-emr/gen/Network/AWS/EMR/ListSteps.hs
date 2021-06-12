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
-- Module      : Network.AWS.EMR.ListSteps
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of steps for the cluster in reverse order unless you
-- specify @stepIds@ with the request of filter by @StepStates@. You can
-- specify a maximum of 10 @stepIDs@.
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListSteps
  ( -- * Creating a Request
    ListSteps (..),
    newListSteps,

    -- * Request Lenses
    listSteps_stepIds,
    listSteps_stepStates,
    listSteps_marker,
    listSteps_clusterId,

    -- * Destructuring the Response
    ListStepsResponse (..),
    newListStepsResponse,

    -- * Response Lenses
    listStepsResponse_steps,
    listStepsResponse_marker,
    listStepsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | This input determines which steps to list.
--
-- /See:/ 'newListSteps' smart constructor.
data ListSteps = ListSteps'
  { -- | The filter to limit the step list based on the identifier of the steps.
    -- You can specify a maximum of ten Step IDs. The character constraint
    -- applies to the overall length of the array.
    stepIds :: Core.Maybe [Core.Text],
    -- | The filter to limit the step list based on certain states.
    stepStates :: Core.Maybe [StepState],
    -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Core.Maybe Core.Text,
    -- | The identifier of the cluster for which to list the steps.
    clusterId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListSteps' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stepIds', 'listSteps_stepIds' - The filter to limit the step list based on the identifier of the steps.
-- You can specify a maximum of ten Step IDs. The character constraint
-- applies to the overall length of the array.
--
-- 'stepStates', 'listSteps_stepStates' - The filter to limit the step list based on certain states.
--
-- 'marker', 'listSteps_marker' - The pagination token that indicates the next set of results to retrieve.
--
-- 'clusterId', 'listSteps_clusterId' - The identifier of the cluster for which to list the steps.
newListSteps ::
  -- | 'clusterId'
  Core.Text ->
  ListSteps
newListSteps pClusterId_ =
  ListSteps'
    { stepIds = Core.Nothing,
      stepStates = Core.Nothing,
      marker = Core.Nothing,
      clusterId = pClusterId_
    }

-- | The filter to limit the step list based on the identifier of the steps.
-- You can specify a maximum of ten Step IDs. The character constraint
-- applies to the overall length of the array.
listSteps_stepIds :: Lens.Lens' ListSteps (Core.Maybe [Core.Text])
listSteps_stepIds = Lens.lens (\ListSteps' {stepIds} -> stepIds) (\s@ListSteps' {} a -> s {stepIds = a} :: ListSteps) Core.. Lens.mapping Lens._Coerce

-- | The filter to limit the step list based on certain states.
listSteps_stepStates :: Lens.Lens' ListSteps (Core.Maybe [StepState])
listSteps_stepStates = Lens.lens (\ListSteps' {stepStates} -> stepStates) (\s@ListSteps' {} a -> s {stepStates = a} :: ListSteps) Core.. Lens.mapping Lens._Coerce

-- | The pagination token that indicates the next set of results to retrieve.
listSteps_marker :: Lens.Lens' ListSteps (Core.Maybe Core.Text)
listSteps_marker = Lens.lens (\ListSteps' {marker} -> marker) (\s@ListSteps' {} a -> s {marker = a} :: ListSteps)

-- | The identifier of the cluster for which to list the steps.
listSteps_clusterId :: Lens.Lens' ListSteps Core.Text
listSteps_clusterId = Lens.lens (\ListSteps' {clusterId} -> clusterId) (\s@ListSteps' {} a -> s {clusterId = a} :: ListSteps)

instance Core.AWSPager ListSteps where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listStepsResponse_marker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listStepsResponse_steps Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listSteps_marker
          Lens..~ rs Lens.^? listStepsResponse_marker Core.. Lens._Just

instance Core.AWSRequest ListSteps where
  type AWSResponse ListSteps = ListStepsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStepsResponse'
            Core.<$> (x Core..?> "Steps" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListSteps

instance Core.NFData ListSteps

instance Core.ToHeaders ListSteps where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("ElasticMapReduce.ListSteps" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListSteps where
  toJSON ListSteps' {..} =
    Core.object
      ( Core.catMaybes
          [ ("StepIds" Core..=) Core.<$> stepIds,
            ("StepStates" Core..=) Core.<$> stepStates,
            ("Marker" Core..=) Core.<$> marker,
            Core.Just ("ClusterId" Core..= clusterId)
          ]
      )

instance Core.ToPath ListSteps where
  toPath = Core.const "/"

instance Core.ToQuery ListSteps where
  toQuery = Core.const Core.mempty

-- | This output contains the list of steps returned in reverse order. This
-- means that the last step is the first element in the list.
--
-- /See:/ 'newListStepsResponse' smart constructor.
data ListStepsResponse = ListStepsResponse'
  { -- | The filtered list of steps for the cluster.
    steps :: Core.Maybe [StepSummary],
    -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListStepsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'steps', 'listStepsResponse_steps' - The filtered list of steps for the cluster.
--
-- 'marker', 'listStepsResponse_marker' - The pagination token that indicates the next set of results to retrieve.
--
-- 'httpStatus', 'listStepsResponse_httpStatus' - The response's http status code.
newListStepsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListStepsResponse
newListStepsResponse pHttpStatus_ =
  ListStepsResponse'
    { steps = Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The filtered list of steps for the cluster.
listStepsResponse_steps :: Lens.Lens' ListStepsResponse (Core.Maybe [StepSummary])
listStepsResponse_steps = Lens.lens (\ListStepsResponse' {steps} -> steps) (\s@ListStepsResponse' {} a -> s {steps = a} :: ListStepsResponse) Core.. Lens.mapping Lens._Coerce

-- | The pagination token that indicates the next set of results to retrieve.
listStepsResponse_marker :: Lens.Lens' ListStepsResponse (Core.Maybe Core.Text)
listStepsResponse_marker = Lens.lens (\ListStepsResponse' {marker} -> marker) (\s@ListStepsResponse' {} a -> s {marker = a} :: ListStepsResponse)

-- | The response's http status code.
listStepsResponse_httpStatus :: Lens.Lens' ListStepsResponse Core.Int
listStepsResponse_httpStatus = Lens.lens (\ListStepsResponse' {httpStatus} -> httpStatus) (\s@ListStepsResponse' {} a -> s {httpStatus = a} :: ListStepsResponse)

instance Core.NFData ListStepsResponse
