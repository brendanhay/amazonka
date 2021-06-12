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
-- Module      : Network.AWS.MediaLive.DeleteMultiplex
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a multiplex. The multiplex must be idle.
module Network.AWS.MediaLive.DeleteMultiplex
  ( -- * Creating a Request
    DeleteMultiplex (..),
    newDeleteMultiplex,

    -- * Request Lenses
    deleteMultiplex_multiplexId,

    -- * Destructuring the Response
    DeleteMultiplexResponse (..),
    newDeleteMultiplexResponse,

    -- * Response Lenses
    deleteMultiplexResponse_availabilityZones,
    deleteMultiplexResponse_arn,
    deleteMultiplexResponse_id,
    deleteMultiplexResponse_pipelinesRunningCount,
    deleteMultiplexResponse_programCount,
    deleteMultiplexResponse_destinations,
    deleteMultiplexResponse_state,
    deleteMultiplexResponse_name,
    deleteMultiplexResponse_tags,
    deleteMultiplexResponse_multiplexSettings,
    deleteMultiplexResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DeleteMultiplexRequest
--
-- /See:/ 'newDeleteMultiplex' smart constructor.
data DeleteMultiplex = DeleteMultiplex'
  { -- | The ID of the multiplex.
    multiplexId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteMultiplex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'multiplexId', 'deleteMultiplex_multiplexId' - The ID of the multiplex.
newDeleteMultiplex ::
  -- | 'multiplexId'
  Core.Text ->
  DeleteMultiplex
newDeleteMultiplex pMultiplexId_ =
  DeleteMultiplex' {multiplexId = pMultiplexId_}

-- | The ID of the multiplex.
deleteMultiplex_multiplexId :: Lens.Lens' DeleteMultiplex Core.Text
deleteMultiplex_multiplexId = Lens.lens (\DeleteMultiplex' {multiplexId} -> multiplexId) (\s@DeleteMultiplex' {} a -> s {multiplexId = a} :: DeleteMultiplex)

instance Core.AWSRequest DeleteMultiplex where
  type
    AWSResponse DeleteMultiplex =
      DeleteMultiplexResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteMultiplexResponse'
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

instance Core.Hashable DeleteMultiplex

instance Core.NFData DeleteMultiplex

instance Core.ToHeaders DeleteMultiplex where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteMultiplex where
  toPath DeleteMultiplex' {..} =
    Core.mconcat
      ["/prod/multiplexes/", Core.toBS multiplexId]

instance Core.ToQuery DeleteMultiplex where
  toQuery = Core.const Core.mempty

-- | Placeholder documentation for DeleteMultiplexResponse
--
-- /See:/ 'newDeleteMultiplexResponse' smart constructor.
data DeleteMultiplexResponse = DeleteMultiplexResponse'
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
-- Create a value of 'DeleteMultiplexResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZones', 'deleteMultiplexResponse_availabilityZones' - A list of availability zones for the multiplex.
--
-- 'arn', 'deleteMultiplexResponse_arn' - The unique arn of the multiplex.
--
-- 'id', 'deleteMultiplexResponse_id' - The unique id of the multiplex.
--
-- 'pipelinesRunningCount', 'deleteMultiplexResponse_pipelinesRunningCount' - The number of currently healthy pipelines.
--
-- 'programCount', 'deleteMultiplexResponse_programCount' - The number of programs in the multiplex.
--
-- 'destinations', 'deleteMultiplexResponse_destinations' - A list of the multiplex output destinations.
--
-- 'state', 'deleteMultiplexResponse_state' - The current state of the multiplex.
--
-- 'name', 'deleteMultiplexResponse_name' - The name of the multiplex.
--
-- 'tags', 'deleteMultiplexResponse_tags' - A collection of key-value pairs.
--
-- 'multiplexSettings', 'deleteMultiplexResponse_multiplexSettings' - Configuration for a multiplex event.
--
-- 'httpStatus', 'deleteMultiplexResponse_httpStatus' - The response's http status code.
newDeleteMultiplexResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteMultiplexResponse
newDeleteMultiplexResponse pHttpStatus_ =
  DeleteMultiplexResponse'
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
deleteMultiplexResponse_availabilityZones :: Lens.Lens' DeleteMultiplexResponse (Core.Maybe [Core.Text])
deleteMultiplexResponse_availabilityZones = Lens.lens (\DeleteMultiplexResponse' {availabilityZones} -> availabilityZones) (\s@DeleteMultiplexResponse' {} a -> s {availabilityZones = a} :: DeleteMultiplexResponse) Core.. Lens.mapping Lens._Coerce

-- | The unique arn of the multiplex.
deleteMultiplexResponse_arn :: Lens.Lens' DeleteMultiplexResponse (Core.Maybe Core.Text)
deleteMultiplexResponse_arn = Lens.lens (\DeleteMultiplexResponse' {arn} -> arn) (\s@DeleteMultiplexResponse' {} a -> s {arn = a} :: DeleteMultiplexResponse)

-- | The unique id of the multiplex.
deleteMultiplexResponse_id :: Lens.Lens' DeleteMultiplexResponse (Core.Maybe Core.Text)
deleteMultiplexResponse_id = Lens.lens (\DeleteMultiplexResponse' {id} -> id) (\s@DeleteMultiplexResponse' {} a -> s {id = a} :: DeleteMultiplexResponse)

-- | The number of currently healthy pipelines.
deleteMultiplexResponse_pipelinesRunningCount :: Lens.Lens' DeleteMultiplexResponse (Core.Maybe Core.Int)
deleteMultiplexResponse_pipelinesRunningCount = Lens.lens (\DeleteMultiplexResponse' {pipelinesRunningCount} -> pipelinesRunningCount) (\s@DeleteMultiplexResponse' {} a -> s {pipelinesRunningCount = a} :: DeleteMultiplexResponse)

-- | The number of programs in the multiplex.
deleteMultiplexResponse_programCount :: Lens.Lens' DeleteMultiplexResponse (Core.Maybe Core.Int)
deleteMultiplexResponse_programCount = Lens.lens (\DeleteMultiplexResponse' {programCount} -> programCount) (\s@DeleteMultiplexResponse' {} a -> s {programCount = a} :: DeleteMultiplexResponse)

-- | A list of the multiplex output destinations.
deleteMultiplexResponse_destinations :: Lens.Lens' DeleteMultiplexResponse (Core.Maybe [MultiplexOutputDestination])
deleteMultiplexResponse_destinations = Lens.lens (\DeleteMultiplexResponse' {destinations} -> destinations) (\s@DeleteMultiplexResponse' {} a -> s {destinations = a} :: DeleteMultiplexResponse) Core.. Lens.mapping Lens._Coerce

-- | The current state of the multiplex.
deleteMultiplexResponse_state :: Lens.Lens' DeleteMultiplexResponse (Core.Maybe MultiplexState)
deleteMultiplexResponse_state = Lens.lens (\DeleteMultiplexResponse' {state} -> state) (\s@DeleteMultiplexResponse' {} a -> s {state = a} :: DeleteMultiplexResponse)

-- | The name of the multiplex.
deleteMultiplexResponse_name :: Lens.Lens' DeleteMultiplexResponse (Core.Maybe Core.Text)
deleteMultiplexResponse_name = Lens.lens (\DeleteMultiplexResponse' {name} -> name) (\s@DeleteMultiplexResponse' {} a -> s {name = a} :: DeleteMultiplexResponse)

-- | A collection of key-value pairs.
deleteMultiplexResponse_tags :: Lens.Lens' DeleteMultiplexResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
deleteMultiplexResponse_tags = Lens.lens (\DeleteMultiplexResponse' {tags} -> tags) (\s@DeleteMultiplexResponse' {} a -> s {tags = a} :: DeleteMultiplexResponse) Core.. Lens.mapping Lens._Coerce

-- | Configuration for a multiplex event.
deleteMultiplexResponse_multiplexSettings :: Lens.Lens' DeleteMultiplexResponse (Core.Maybe MultiplexSettings)
deleteMultiplexResponse_multiplexSettings = Lens.lens (\DeleteMultiplexResponse' {multiplexSettings} -> multiplexSettings) (\s@DeleteMultiplexResponse' {} a -> s {multiplexSettings = a} :: DeleteMultiplexResponse)

-- | The response's http status code.
deleteMultiplexResponse_httpStatus :: Lens.Lens' DeleteMultiplexResponse Core.Int
deleteMultiplexResponse_httpStatus = Lens.lens (\DeleteMultiplexResponse' {httpStatus} -> httpStatus) (\s@DeleteMultiplexResponse' {} a -> s {httpStatus = a} :: DeleteMultiplexResponse)

instance Core.NFData DeleteMultiplexResponse
