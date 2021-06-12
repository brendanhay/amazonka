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
-- Module      : Network.AWS.MediaLive.StopMultiplex
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running multiplex. If the multiplex isn\'t running, this action
-- has no effect.
module Network.AWS.MediaLive.StopMultiplex
  ( -- * Creating a Request
    StopMultiplex (..),
    newStopMultiplex,

    -- * Request Lenses
    stopMultiplex_multiplexId,

    -- * Destructuring the Response
    StopMultiplexResponse (..),
    newStopMultiplexResponse,

    -- * Response Lenses
    stopMultiplexResponse_availabilityZones,
    stopMultiplexResponse_arn,
    stopMultiplexResponse_id,
    stopMultiplexResponse_pipelinesRunningCount,
    stopMultiplexResponse_programCount,
    stopMultiplexResponse_destinations,
    stopMultiplexResponse_state,
    stopMultiplexResponse_name,
    stopMultiplexResponse_tags,
    stopMultiplexResponse_multiplexSettings,
    stopMultiplexResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for StopMultiplexRequest
--
-- /See:/ 'newStopMultiplex' smart constructor.
data StopMultiplex = StopMultiplex'
  { -- | The ID of the multiplex.
    multiplexId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopMultiplex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'multiplexId', 'stopMultiplex_multiplexId' - The ID of the multiplex.
newStopMultiplex ::
  -- | 'multiplexId'
  Core.Text ->
  StopMultiplex
newStopMultiplex pMultiplexId_ =
  StopMultiplex' {multiplexId = pMultiplexId_}

-- | The ID of the multiplex.
stopMultiplex_multiplexId :: Lens.Lens' StopMultiplex Core.Text
stopMultiplex_multiplexId = Lens.lens (\StopMultiplex' {multiplexId} -> multiplexId) (\s@StopMultiplex' {} a -> s {multiplexId = a} :: StopMultiplex)

instance Core.AWSRequest StopMultiplex where
  type
    AWSResponse StopMultiplex =
      StopMultiplexResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopMultiplexResponse'
            Core.<$> (x Core..?> "availabilityZones" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "arn")
            Core.<*> (x Core..?> "id")
            Core.<*> (x Core..?> "pipelinesRunningCount")
            Core.<*> (x Core..?> "programCount")
            Core.<*> (x Core..?> "destinations" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "state")
            Core.<*> (x Core..?> "name")
            Core.<*> (x Core..?> "tags" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "multiplexSettings")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StopMultiplex

instance Core.NFData StopMultiplex

instance Core.ToHeaders StopMultiplex where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopMultiplex where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath StopMultiplex where
  toPath StopMultiplex' {..} =
    Core.mconcat
      [ "/prod/multiplexes/",
        Core.toBS multiplexId,
        "/stop"
      ]

instance Core.ToQuery StopMultiplex where
  toQuery = Core.const Core.mempty

-- | Placeholder documentation for StopMultiplexResponse
--
-- /See:/ 'newStopMultiplexResponse' smart constructor.
data StopMultiplexResponse = StopMultiplexResponse'
  { -- | A list of availability zones for the multiplex.
    availabilityZones :: Core.Maybe [Core.Text],
    -- | The unique arn of the multiplex.
    arn :: Core.Maybe Core.Text,
    -- | The unique id of the multiplex.
    id :: Core.Maybe Core.Text,
    -- | The number of currently healthy pipelines.
    pipelinesRunningCount :: Core.Maybe Core.Int,
    -- | The number of programs in the multiplex.
    programCount :: Core.Maybe Core.Int,
    -- | A list of the multiplex output destinations.
    destinations :: Core.Maybe [MultiplexOutputDestination],
    -- | The current state of the multiplex.
    state :: Core.Maybe MultiplexState,
    -- | The name of the multiplex.
    name :: Core.Maybe Core.Text,
    -- | A collection of key-value pairs.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Configuration for a multiplex event.
    multiplexSettings :: Core.Maybe MultiplexSettings,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopMultiplexResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZones', 'stopMultiplexResponse_availabilityZones' - A list of availability zones for the multiplex.
--
-- 'arn', 'stopMultiplexResponse_arn' - The unique arn of the multiplex.
--
-- 'id', 'stopMultiplexResponse_id' - The unique id of the multiplex.
--
-- 'pipelinesRunningCount', 'stopMultiplexResponse_pipelinesRunningCount' - The number of currently healthy pipelines.
--
-- 'programCount', 'stopMultiplexResponse_programCount' - The number of programs in the multiplex.
--
-- 'destinations', 'stopMultiplexResponse_destinations' - A list of the multiplex output destinations.
--
-- 'state', 'stopMultiplexResponse_state' - The current state of the multiplex.
--
-- 'name', 'stopMultiplexResponse_name' - The name of the multiplex.
--
-- 'tags', 'stopMultiplexResponse_tags' - A collection of key-value pairs.
--
-- 'multiplexSettings', 'stopMultiplexResponse_multiplexSettings' - Configuration for a multiplex event.
--
-- 'httpStatus', 'stopMultiplexResponse_httpStatus' - The response's http status code.
newStopMultiplexResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StopMultiplexResponse
newStopMultiplexResponse pHttpStatus_ =
  StopMultiplexResponse'
    { availabilityZones =
        Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      pipelinesRunningCount = Core.Nothing,
      programCount = Core.Nothing,
      destinations = Core.Nothing,
      state = Core.Nothing,
      name = Core.Nothing,
      tags = Core.Nothing,
      multiplexSettings = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of availability zones for the multiplex.
stopMultiplexResponse_availabilityZones :: Lens.Lens' StopMultiplexResponse (Core.Maybe [Core.Text])
stopMultiplexResponse_availabilityZones = Lens.lens (\StopMultiplexResponse' {availabilityZones} -> availabilityZones) (\s@StopMultiplexResponse' {} a -> s {availabilityZones = a} :: StopMultiplexResponse) Core.. Lens.mapping Lens._Coerce

-- | The unique arn of the multiplex.
stopMultiplexResponse_arn :: Lens.Lens' StopMultiplexResponse (Core.Maybe Core.Text)
stopMultiplexResponse_arn = Lens.lens (\StopMultiplexResponse' {arn} -> arn) (\s@StopMultiplexResponse' {} a -> s {arn = a} :: StopMultiplexResponse)

-- | The unique id of the multiplex.
stopMultiplexResponse_id :: Lens.Lens' StopMultiplexResponse (Core.Maybe Core.Text)
stopMultiplexResponse_id = Lens.lens (\StopMultiplexResponse' {id} -> id) (\s@StopMultiplexResponse' {} a -> s {id = a} :: StopMultiplexResponse)

-- | The number of currently healthy pipelines.
stopMultiplexResponse_pipelinesRunningCount :: Lens.Lens' StopMultiplexResponse (Core.Maybe Core.Int)
stopMultiplexResponse_pipelinesRunningCount = Lens.lens (\StopMultiplexResponse' {pipelinesRunningCount} -> pipelinesRunningCount) (\s@StopMultiplexResponse' {} a -> s {pipelinesRunningCount = a} :: StopMultiplexResponse)

-- | The number of programs in the multiplex.
stopMultiplexResponse_programCount :: Lens.Lens' StopMultiplexResponse (Core.Maybe Core.Int)
stopMultiplexResponse_programCount = Lens.lens (\StopMultiplexResponse' {programCount} -> programCount) (\s@StopMultiplexResponse' {} a -> s {programCount = a} :: StopMultiplexResponse)

-- | A list of the multiplex output destinations.
stopMultiplexResponse_destinations :: Lens.Lens' StopMultiplexResponse (Core.Maybe [MultiplexOutputDestination])
stopMultiplexResponse_destinations = Lens.lens (\StopMultiplexResponse' {destinations} -> destinations) (\s@StopMultiplexResponse' {} a -> s {destinations = a} :: StopMultiplexResponse) Core.. Lens.mapping Lens._Coerce

-- | The current state of the multiplex.
stopMultiplexResponse_state :: Lens.Lens' StopMultiplexResponse (Core.Maybe MultiplexState)
stopMultiplexResponse_state = Lens.lens (\StopMultiplexResponse' {state} -> state) (\s@StopMultiplexResponse' {} a -> s {state = a} :: StopMultiplexResponse)

-- | The name of the multiplex.
stopMultiplexResponse_name :: Lens.Lens' StopMultiplexResponse (Core.Maybe Core.Text)
stopMultiplexResponse_name = Lens.lens (\StopMultiplexResponse' {name} -> name) (\s@StopMultiplexResponse' {} a -> s {name = a} :: StopMultiplexResponse)

-- | A collection of key-value pairs.
stopMultiplexResponse_tags :: Lens.Lens' StopMultiplexResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
stopMultiplexResponse_tags = Lens.lens (\StopMultiplexResponse' {tags} -> tags) (\s@StopMultiplexResponse' {} a -> s {tags = a} :: StopMultiplexResponse) Core.. Lens.mapping Lens._Coerce

-- | Configuration for a multiplex event.
stopMultiplexResponse_multiplexSettings :: Lens.Lens' StopMultiplexResponse (Core.Maybe MultiplexSettings)
stopMultiplexResponse_multiplexSettings = Lens.lens (\StopMultiplexResponse' {multiplexSettings} -> multiplexSettings) (\s@StopMultiplexResponse' {} a -> s {multiplexSettings = a} :: StopMultiplexResponse)

-- | The response's http status code.
stopMultiplexResponse_httpStatus :: Lens.Lens' StopMultiplexResponse Core.Int
stopMultiplexResponse_httpStatus = Lens.lens (\StopMultiplexResponse' {httpStatus} -> httpStatus) (\s@StopMultiplexResponse' {} a -> s {httpStatus = a} :: StopMultiplexResponse)

instance Core.NFData StopMultiplexResponse
