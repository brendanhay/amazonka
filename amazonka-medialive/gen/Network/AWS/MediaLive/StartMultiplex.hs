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
-- Module      : Network.AWS.MediaLive.StartMultiplex
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Start (run) the multiplex. Starting the multiplex does not start the
-- channels. You must explicitly start each channel.
module Network.AWS.MediaLive.StartMultiplex
  ( -- * Creating a Request
    StartMultiplex (..),
    newStartMultiplex,

    -- * Request Lenses
    startMultiplex_multiplexId,

    -- * Destructuring the Response
    StartMultiplexResponse (..),
    newStartMultiplexResponse,

    -- * Response Lenses
    startMultiplexResponse_availabilityZones,
    startMultiplexResponse_arn,
    startMultiplexResponse_id,
    startMultiplexResponse_pipelinesRunningCount,
    startMultiplexResponse_programCount,
    startMultiplexResponse_destinations,
    startMultiplexResponse_state,
    startMultiplexResponse_name,
    startMultiplexResponse_tags,
    startMultiplexResponse_multiplexSettings,
    startMultiplexResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for StartMultiplexRequest
--
-- /See:/ 'newStartMultiplex' smart constructor.
data StartMultiplex = StartMultiplex'
  { -- | The ID of the multiplex.
    multiplexId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartMultiplex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'multiplexId', 'startMultiplex_multiplexId' - The ID of the multiplex.
newStartMultiplex ::
  -- | 'multiplexId'
  Core.Text ->
  StartMultiplex
newStartMultiplex pMultiplexId_ =
  StartMultiplex' {multiplexId = pMultiplexId_}

-- | The ID of the multiplex.
startMultiplex_multiplexId :: Lens.Lens' StartMultiplex Core.Text
startMultiplex_multiplexId = Lens.lens (\StartMultiplex' {multiplexId} -> multiplexId) (\s@StartMultiplex' {} a -> s {multiplexId = a} :: StartMultiplex)

instance Core.AWSRequest StartMultiplex where
  type
    AWSResponse StartMultiplex =
      StartMultiplexResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartMultiplexResponse'
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

instance Core.Hashable StartMultiplex

instance Core.NFData StartMultiplex

instance Core.ToHeaders StartMultiplex where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartMultiplex where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath StartMultiplex where
  toPath StartMultiplex' {..} =
    Core.mconcat
      [ "/prod/multiplexes/",
        Core.toBS multiplexId,
        "/start"
      ]

instance Core.ToQuery StartMultiplex where
  toQuery = Core.const Core.mempty

-- | Placeholder documentation for StartMultiplexResponse
--
-- /See:/ 'newStartMultiplexResponse' smart constructor.
data StartMultiplexResponse = StartMultiplexResponse'
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
-- Create a value of 'StartMultiplexResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZones', 'startMultiplexResponse_availabilityZones' - A list of availability zones for the multiplex.
--
-- 'arn', 'startMultiplexResponse_arn' - The unique arn of the multiplex.
--
-- 'id', 'startMultiplexResponse_id' - The unique id of the multiplex.
--
-- 'pipelinesRunningCount', 'startMultiplexResponse_pipelinesRunningCount' - The number of currently healthy pipelines.
--
-- 'programCount', 'startMultiplexResponse_programCount' - The number of programs in the multiplex.
--
-- 'destinations', 'startMultiplexResponse_destinations' - A list of the multiplex output destinations.
--
-- 'state', 'startMultiplexResponse_state' - The current state of the multiplex.
--
-- 'name', 'startMultiplexResponse_name' - The name of the multiplex.
--
-- 'tags', 'startMultiplexResponse_tags' - A collection of key-value pairs.
--
-- 'multiplexSettings', 'startMultiplexResponse_multiplexSettings' - Configuration for a multiplex event.
--
-- 'httpStatus', 'startMultiplexResponse_httpStatus' - The response's http status code.
newStartMultiplexResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartMultiplexResponse
newStartMultiplexResponse pHttpStatus_ =
  StartMultiplexResponse'
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
startMultiplexResponse_availabilityZones :: Lens.Lens' StartMultiplexResponse (Core.Maybe [Core.Text])
startMultiplexResponse_availabilityZones = Lens.lens (\StartMultiplexResponse' {availabilityZones} -> availabilityZones) (\s@StartMultiplexResponse' {} a -> s {availabilityZones = a} :: StartMultiplexResponse) Core.. Lens.mapping Lens._Coerce

-- | The unique arn of the multiplex.
startMultiplexResponse_arn :: Lens.Lens' StartMultiplexResponse (Core.Maybe Core.Text)
startMultiplexResponse_arn = Lens.lens (\StartMultiplexResponse' {arn} -> arn) (\s@StartMultiplexResponse' {} a -> s {arn = a} :: StartMultiplexResponse)

-- | The unique id of the multiplex.
startMultiplexResponse_id :: Lens.Lens' StartMultiplexResponse (Core.Maybe Core.Text)
startMultiplexResponse_id = Lens.lens (\StartMultiplexResponse' {id} -> id) (\s@StartMultiplexResponse' {} a -> s {id = a} :: StartMultiplexResponse)

-- | The number of currently healthy pipelines.
startMultiplexResponse_pipelinesRunningCount :: Lens.Lens' StartMultiplexResponse (Core.Maybe Core.Int)
startMultiplexResponse_pipelinesRunningCount = Lens.lens (\StartMultiplexResponse' {pipelinesRunningCount} -> pipelinesRunningCount) (\s@StartMultiplexResponse' {} a -> s {pipelinesRunningCount = a} :: StartMultiplexResponse)

-- | The number of programs in the multiplex.
startMultiplexResponse_programCount :: Lens.Lens' StartMultiplexResponse (Core.Maybe Core.Int)
startMultiplexResponse_programCount = Lens.lens (\StartMultiplexResponse' {programCount} -> programCount) (\s@StartMultiplexResponse' {} a -> s {programCount = a} :: StartMultiplexResponse)

-- | A list of the multiplex output destinations.
startMultiplexResponse_destinations :: Lens.Lens' StartMultiplexResponse (Core.Maybe [MultiplexOutputDestination])
startMultiplexResponse_destinations = Lens.lens (\StartMultiplexResponse' {destinations} -> destinations) (\s@StartMultiplexResponse' {} a -> s {destinations = a} :: StartMultiplexResponse) Core.. Lens.mapping Lens._Coerce

-- | The current state of the multiplex.
startMultiplexResponse_state :: Lens.Lens' StartMultiplexResponse (Core.Maybe MultiplexState)
startMultiplexResponse_state = Lens.lens (\StartMultiplexResponse' {state} -> state) (\s@StartMultiplexResponse' {} a -> s {state = a} :: StartMultiplexResponse)

-- | The name of the multiplex.
startMultiplexResponse_name :: Lens.Lens' StartMultiplexResponse (Core.Maybe Core.Text)
startMultiplexResponse_name = Lens.lens (\StartMultiplexResponse' {name} -> name) (\s@StartMultiplexResponse' {} a -> s {name = a} :: StartMultiplexResponse)

-- | A collection of key-value pairs.
startMultiplexResponse_tags :: Lens.Lens' StartMultiplexResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
startMultiplexResponse_tags = Lens.lens (\StartMultiplexResponse' {tags} -> tags) (\s@StartMultiplexResponse' {} a -> s {tags = a} :: StartMultiplexResponse) Core.. Lens.mapping Lens._Coerce

-- | Configuration for a multiplex event.
startMultiplexResponse_multiplexSettings :: Lens.Lens' StartMultiplexResponse (Core.Maybe MultiplexSettings)
startMultiplexResponse_multiplexSettings = Lens.lens (\StartMultiplexResponse' {multiplexSettings} -> multiplexSettings) (\s@StartMultiplexResponse' {} a -> s {multiplexSettings = a} :: StartMultiplexResponse)

-- | The response's http status code.
startMultiplexResponse_httpStatus :: Lens.Lens' StartMultiplexResponse Core.Int
startMultiplexResponse_httpStatus = Lens.lens (\StartMultiplexResponse' {httpStatus} -> httpStatus) (\s@StartMultiplexResponse' {} a -> s {httpStatus = a} :: StartMultiplexResponse)

instance Core.NFData StartMultiplexResponse
