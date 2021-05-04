{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | This input determines which steps to list.
--
-- /See:/ 'newListSteps' smart constructor.
data ListSteps = ListSteps'
  { -- | The filter to limit the step list based on the identifier of the steps.
    -- You can specify a maximum of ten Step IDs. The character constraint
    -- applies to the overall length of the array.
    stepIds :: Prelude.Maybe [Prelude.Text],
    -- | The filter to limit the step list based on certain states.
    stepStates :: Prelude.Maybe [StepState],
    -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the cluster for which to list the steps.
    clusterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  ListSteps
newListSteps pClusterId_ =
  ListSteps'
    { stepIds = Prelude.Nothing,
      stepStates = Prelude.Nothing,
      marker = Prelude.Nothing,
      clusterId = pClusterId_
    }

-- | The filter to limit the step list based on the identifier of the steps.
-- You can specify a maximum of ten Step IDs. The character constraint
-- applies to the overall length of the array.
listSteps_stepIds :: Lens.Lens' ListSteps (Prelude.Maybe [Prelude.Text])
listSteps_stepIds = Lens.lens (\ListSteps' {stepIds} -> stepIds) (\s@ListSteps' {} a -> s {stepIds = a} :: ListSteps) Prelude.. Lens.mapping Prelude._Coerce

-- | The filter to limit the step list based on certain states.
listSteps_stepStates :: Lens.Lens' ListSteps (Prelude.Maybe [StepState])
listSteps_stepStates = Lens.lens (\ListSteps' {stepStates} -> stepStates) (\s@ListSteps' {} a -> s {stepStates = a} :: ListSteps) Prelude.. Lens.mapping Prelude._Coerce

-- | The pagination token that indicates the next set of results to retrieve.
listSteps_marker :: Lens.Lens' ListSteps (Prelude.Maybe Prelude.Text)
listSteps_marker = Lens.lens (\ListSteps' {marker} -> marker) (\s@ListSteps' {} a -> s {marker = a} :: ListSteps)

-- | The identifier of the cluster for which to list the steps.
listSteps_clusterId :: Lens.Lens' ListSteps Prelude.Text
listSteps_clusterId = Lens.lens (\ListSteps' {clusterId} -> clusterId) (\s@ListSteps' {} a -> s {clusterId = a} :: ListSteps)

instance Pager.AWSPager ListSteps where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listStepsResponse_marker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listStepsResponse_steps Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listSteps_marker
          Lens..~ rs
          Lens.^? listStepsResponse_marker Prelude.. Lens._Just

instance Prelude.AWSRequest ListSteps where
  type Rs ListSteps = ListStepsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStepsResponse'
            Prelude.<$> (x Prelude..?> "Steps" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..?> "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSteps

instance Prelude.NFData ListSteps

instance Prelude.ToHeaders ListSteps where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("ElasticMapReduce.ListSteps" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListSteps where
  toJSON ListSteps' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("StepIds" Prelude..=) Prelude.<$> stepIds,
            ("StepStates" Prelude..=) Prelude.<$> stepStates,
            ("Marker" Prelude..=) Prelude.<$> marker,
            Prelude.Just ("ClusterId" Prelude..= clusterId)
          ]
      )

instance Prelude.ToPath ListSteps where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListSteps where
  toQuery = Prelude.const Prelude.mempty

-- | This output contains the list of steps returned in reverse order. This
-- means that the last step is the first element in the list.
--
-- /See:/ 'newListStepsResponse' smart constructor.
data ListStepsResponse = ListStepsResponse'
  { -- | The filtered list of steps for the cluster.
    steps :: Prelude.Maybe [StepSummary],
    -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListStepsResponse
newListStepsResponse pHttpStatus_ =
  ListStepsResponse'
    { steps = Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The filtered list of steps for the cluster.
listStepsResponse_steps :: Lens.Lens' ListStepsResponse (Prelude.Maybe [StepSummary])
listStepsResponse_steps = Lens.lens (\ListStepsResponse' {steps} -> steps) (\s@ListStepsResponse' {} a -> s {steps = a} :: ListStepsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The pagination token that indicates the next set of results to retrieve.
listStepsResponse_marker :: Lens.Lens' ListStepsResponse (Prelude.Maybe Prelude.Text)
listStepsResponse_marker = Lens.lens (\ListStepsResponse' {marker} -> marker) (\s@ListStepsResponse' {} a -> s {marker = a} :: ListStepsResponse)

-- | The response's http status code.
listStepsResponse_httpStatus :: Lens.Lens' ListStepsResponse Prelude.Int
listStepsResponse_httpStatus = Lens.lens (\ListStepsResponse' {httpStatus} -> httpStatus) (\s@ListStepsResponse' {} a -> s {httpStatus = a} :: ListStepsResponse)

instance Prelude.NFData ListStepsResponse
