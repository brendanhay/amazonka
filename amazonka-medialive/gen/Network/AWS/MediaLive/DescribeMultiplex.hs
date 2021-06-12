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
-- Module      : Network.AWS.MediaLive.DescribeMultiplex
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a multiplex.
module Network.AWS.MediaLive.DescribeMultiplex
  ( -- * Creating a Request
    DescribeMultiplex (..),
    newDescribeMultiplex,

    -- * Request Lenses
    describeMultiplex_multiplexId,

    -- * Destructuring the Response
    DescribeMultiplexResponse (..),
    newDescribeMultiplexResponse,

    -- * Response Lenses
    describeMultiplexResponse_availabilityZones,
    describeMultiplexResponse_arn,
    describeMultiplexResponse_id,
    describeMultiplexResponse_pipelinesRunningCount,
    describeMultiplexResponse_programCount,
    describeMultiplexResponse_destinations,
    describeMultiplexResponse_state,
    describeMultiplexResponse_name,
    describeMultiplexResponse_tags,
    describeMultiplexResponse_multiplexSettings,
    describeMultiplexResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DescribeMultiplexRequest
--
-- /See:/ 'newDescribeMultiplex' smart constructor.
data DescribeMultiplex = DescribeMultiplex'
  { -- | The ID of the multiplex.
    multiplexId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeMultiplex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'multiplexId', 'describeMultiplex_multiplexId' - The ID of the multiplex.
newDescribeMultiplex ::
  -- | 'multiplexId'
  Core.Text ->
  DescribeMultiplex
newDescribeMultiplex pMultiplexId_ =
  DescribeMultiplex' {multiplexId = pMultiplexId_}

-- | The ID of the multiplex.
describeMultiplex_multiplexId :: Lens.Lens' DescribeMultiplex Core.Text
describeMultiplex_multiplexId = Lens.lens (\DescribeMultiplex' {multiplexId} -> multiplexId) (\s@DescribeMultiplex' {} a -> s {multiplexId = a} :: DescribeMultiplex)

instance Core.AWSRequest DescribeMultiplex where
  type
    AWSResponse DescribeMultiplex =
      DescribeMultiplexResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMultiplexResponse'
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

instance Core.Hashable DescribeMultiplex

instance Core.NFData DescribeMultiplex

instance Core.ToHeaders DescribeMultiplex where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DescribeMultiplex where
  toPath DescribeMultiplex' {..} =
    Core.mconcat
      ["/prod/multiplexes/", Core.toBS multiplexId]

instance Core.ToQuery DescribeMultiplex where
  toQuery = Core.const Core.mempty

-- | Placeholder documentation for DescribeMultiplexResponse
--
-- /See:/ 'newDescribeMultiplexResponse' smart constructor.
data DescribeMultiplexResponse = DescribeMultiplexResponse'
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
-- Create a value of 'DescribeMultiplexResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZones', 'describeMultiplexResponse_availabilityZones' - A list of availability zones for the multiplex.
--
-- 'arn', 'describeMultiplexResponse_arn' - The unique arn of the multiplex.
--
-- 'id', 'describeMultiplexResponse_id' - The unique id of the multiplex.
--
-- 'pipelinesRunningCount', 'describeMultiplexResponse_pipelinesRunningCount' - The number of currently healthy pipelines.
--
-- 'programCount', 'describeMultiplexResponse_programCount' - The number of programs in the multiplex.
--
-- 'destinations', 'describeMultiplexResponse_destinations' - A list of the multiplex output destinations.
--
-- 'state', 'describeMultiplexResponse_state' - The current state of the multiplex.
--
-- 'name', 'describeMultiplexResponse_name' - The name of the multiplex.
--
-- 'tags', 'describeMultiplexResponse_tags' - A collection of key-value pairs.
--
-- 'multiplexSettings', 'describeMultiplexResponse_multiplexSettings' - Configuration for a multiplex event.
--
-- 'httpStatus', 'describeMultiplexResponse_httpStatus' - The response's http status code.
newDescribeMultiplexResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeMultiplexResponse
newDescribeMultiplexResponse pHttpStatus_ =
  DescribeMultiplexResponse'
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
describeMultiplexResponse_availabilityZones :: Lens.Lens' DescribeMultiplexResponse (Core.Maybe [Core.Text])
describeMultiplexResponse_availabilityZones = Lens.lens (\DescribeMultiplexResponse' {availabilityZones} -> availabilityZones) (\s@DescribeMultiplexResponse' {} a -> s {availabilityZones = a} :: DescribeMultiplexResponse) Core.. Lens.mapping Lens._Coerce

-- | The unique arn of the multiplex.
describeMultiplexResponse_arn :: Lens.Lens' DescribeMultiplexResponse (Core.Maybe Core.Text)
describeMultiplexResponse_arn = Lens.lens (\DescribeMultiplexResponse' {arn} -> arn) (\s@DescribeMultiplexResponse' {} a -> s {arn = a} :: DescribeMultiplexResponse)

-- | The unique id of the multiplex.
describeMultiplexResponse_id :: Lens.Lens' DescribeMultiplexResponse (Core.Maybe Core.Text)
describeMultiplexResponse_id = Lens.lens (\DescribeMultiplexResponse' {id} -> id) (\s@DescribeMultiplexResponse' {} a -> s {id = a} :: DescribeMultiplexResponse)

-- | The number of currently healthy pipelines.
describeMultiplexResponse_pipelinesRunningCount :: Lens.Lens' DescribeMultiplexResponse (Core.Maybe Core.Int)
describeMultiplexResponse_pipelinesRunningCount = Lens.lens (\DescribeMultiplexResponse' {pipelinesRunningCount} -> pipelinesRunningCount) (\s@DescribeMultiplexResponse' {} a -> s {pipelinesRunningCount = a} :: DescribeMultiplexResponse)

-- | The number of programs in the multiplex.
describeMultiplexResponse_programCount :: Lens.Lens' DescribeMultiplexResponse (Core.Maybe Core.Int)
describeMultiplexResponse_programCount = Lens.lens (\DescribeMultiplexResponse' {programCount} -> programCount) (\s@DescribeMultiplexResponse' {} a -> s {programCount = a} :: DescribeMultiplexResponse)

-- | A list of the multiplex output destinations.
describeMultiplexResponse_destinations :: Lens.Lens' DescribeMultiplexResponse (Core.Maybe [MultiplexOutputDestination])
describeMultiplexResponse_destinations = Lens.lens (\DescribeMultiplexResponse' {destinations} -> destinations) (\s@DescribeMultiplexResponse' {} a -> s {destinations = a} :: DescribeMultiplexResponse) Core.. Lens.mapping Lens._Coerce

-- | The current state of the multiplex.
describeMultiplexResponse_state :: Lens.Lens' DescribeMultiplexResponse (Core.Maybe MultiplexState)
describeMultiplexResponse_state = Lens.lens (\DescribeMultiplexResponse' {state} -> state) (\s@DescribeMultiplexResponse' {} a -> s {state = a} :: DescribeMultiplexResponse)

-- | The name of the multiplex.
describeMultiplexResponse_name :: Lens.Lens' DescribeMultiplexResponse (Core.Maybe Core.Text)
describeMultiplexResponse_name = Lens.lens (\DescribeMultiplexResponse' {name} -> name) (\s@DescribeMultiplexResponse' {} a -> s {name = a} :: DescribeMultiplexResponse)

-- | A collection of key-value pairs.
describeMultiplexResponse_tags :: Lens.Lens' DescribeMultiplexResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
describeMultiplexResponse_tags = Lens.lens (\DescribeMultiplexResponse' {tags} -> tags) (\s@DescribeMultiplexResponse' {} a -> s {tags = a} :: DescribeMultiplexResponse) Core.. Lens.mapping Lens._Coerce

-- | Configuration for a multiplex event.
describeMultiplexResponse_multiplexSettings :: Lens.Lens' DescribeMultiplexResponse (Core.Maybe MultiplexSettings)
describeMultiplexResponse_multiplexSettings = Lens.lens (\DescribeMultiplexResponse' {multiplexSettings} -> multiplexSettings) (\s@DescribeMultiplexResponse' {} a -> s {multiplexSettings = a} :: DescribeMultiplexResponse)

-- | The response's http status code.
describeMultiplexResponse_httpStatus :: Lens.Lens' DescribeMultiplexResponse Core.Int
describeMultiplexResponse_httpStatus = Lens.lens (\DescribeMultiplexResponse' {httpStatus} -> httpStatus) (\s@DescribeMultiplexResponse' {} a -> s {httpStatus = a} :: DescribeMultiplexResponse)

instance Core.NFData DescribeMultiplexResponse
