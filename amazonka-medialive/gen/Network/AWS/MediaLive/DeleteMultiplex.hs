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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DeleteMultiplexRequest
--
-- /See:/ 'newDeleteMultiplex' smart constructor.
data DeleteMultiplex = DeleteMultiplex'
  { -- | The ID of the multiplex.
    multiplexId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteMultiplex
newDeleteMultiplex pMultiplexId_ =
  DeleteMultiplex' {multiplexId = pMultiplexId_}

-- | The ID of the multiplex.
deleteMultiplex_multiplexId :: Lens.Lens' DeleteMultiplex Prelude.Text
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
            Prelude.<$> ( x Core..?> "availabilityZones"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "id")
            Prelude.<*> (x Core..?> "pipelinesRunningCount")
            Prelude.<*> (x Core..?> "programCount")
            Prelude.<*> (x Core..?> "destinations" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "state")
            Prelude.<*> (x Core..?> "name")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "multiplexSettings")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteMultiplex

instance Prelude.NFData DeleteMultiplex

instance Core.ToHeaders DeleteMultiplex where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteMultiplex where
  toPath DeleteMultiplex' {..} =
    Prelude.mconcat
      ["/prod/multiplexes/", Core.toBS multiplexId]

instance Core.ToQuery DeleteMultiplex where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for DeleteMultiplexResponse
--
-- /See:/ 'newDeleteMultiplexResponse' smart constructor.
data DeleteMultiplexResponse = DeleteMultiplexResponse'
  { -- | A list of availability zones for the multiplex.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The unique arn of the multiplex.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique id of the multiplex.
    id :: Prelude.Maybe Prelude.Text,
    -- | The number of currently healthy pipelines.
    pipelinesRunningCount :: Prelude.Maybe Prelude.Int,
    -- | The number of programs in the multiplex.
    programCount :: Prelude.Maybe Prelude.Int,
    -- | A list of the multiplex output destinations.
    destinations :: Prelude.Maybe [MultiplexOutputDestination],
    -- | The current state of the multiplex.
    state :: Prelude.Maybe MultiplexState,
    -- | The name of the multiplex.
    name :: Prelude.Maybe Prelude.Text,
    -- | A collection of key-value pairs.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Configuration for a multiplex event.
    multiplexSettings :: Prelude.Maybe MultiplexSettings,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteMultiplexResponse
newDeleteMultiplexResponse pHttpStatus_ =
  DeleteMultiplexResponse'
    { availabilityZones =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      pipelinesRunningCount = Prelude.Nothing,
      programCount = Prelude.Nothing,
      destinations = Prelude.Nothing,
      state = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing,
      multiplexSettings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of availability zones for the multiplex.
deleteMultiplexResponse_availabilityZones :: Lens.Lens' DeleteMultiplexResponse (Prelude.Maybe [Prelude.Text])
deleteMultiplexResponse_availabilityZones = Lens.lens (\DeleteMultiplexResponse' {availabilityZones} -> availabilityZones) (\s@DeleteMultiplexResponse' {} a -> s {availabilityZones = a} :: DeleteMultiplexResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The unique arn of the multiplex.
deleteMultiplexResponse_arn :: Lens.Lens' DeleteMultiplexResponse (Prelude.Maybe Prelude.Text)
deleteMultiplexResponse_arn = Lens.lens (\DeleteMultiplexResponse' {arn} -> arn) (\s@DeleteMultiplexResponse' {} a -> s {arn = a} :: DeleteMultiplexResponse)

-- | The unique id of the multiplex.
deleteMultiplexResponse_id :: Lens.Lens' DeleteMultiplexResponse (Prelude.Maybe Prelude.Text)
deleteMultiplexResponse_id = Lens.lens (\DeleteMultiplexResponse' {id} -> id) (\s@DeleteMultiplexResponse' {} a -> s {id = a} :: DeleteMultiplexResponse)

-- | The number of currently healthy pipelines.
deleteMultiplexResponse_pipelinesRunningCount :: Lens.Lens' DeleteMultiplexResponse (Prelude.Maybe Prelude.Int)
deleteMultiplexResponse_pipelinesRunningCount = Lens.lens (\DeleteMultiplexResponse' {pipelinesRunningCount} -> pipelinesRunningCount) (\s@DeleteMultiplexResponse' {} a -> s {pipelinesRunningCount = a} :: DeleteMultiplexResponse)

-- | The number of programs in the multiplex.
deleteMultiplexResponse_programCount :: Lens.Lens' DeleteMultiplexResponse (Prelude.Maybe Prelude.Int)
deleteMultiplexResponse_programCount = Lens.lens (\DeleteMultiplexResponse' {programCount} -> programCount) (\s@DeleteMultiplexResponse' {} a -> s {programCount = a} :: DeleteMultiplexResponse)

-- | A list of the multiplex output destinations.
deleteMultiplexResponse_destinations :: Lens.Lens' DeleteMultiplexResponse (Prelude.Maybe [MultiplexOutputDestination])
deleteMultiplexResponse_destinations = Lens.lens (\DeleteMultiplexResponse' {destinations} -> destinations) (\s@DeleteMultiplexResponse' {} a -> s {destinations = a} :: DeleteMultiplexResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The current state of the multiplex.
deleteMultiplexResponse_state :: Lens.Lens' DeleteMultiplexResponse (Prelude.Maybe MultiplexState)
deleteMultiplexResponse_state = Lens.lens (\DeleteMultiplexResponse' {state} -> state) (\s@DeleteMultiplexResponse' {} a -> s {state = a} :: DeleteMultiplexResponse)

-- | The name of the multiplex.
deleteMultiplexResponse_name :: Lens.Lens' DeleteMultiplexResponse (Prelude.Maybe Prelude.Text)
deleteMultiplexResponse_name = Lens.lens (\DeleteMultiplexResponse' {name} -> name) (\s@DeleteMultiplexResponse' {} a -> s {name = a} :: DeleteMultiplexResponse)

-- | A collection of key-value pairs.
deleteMultiplexResponse_tags :: Lens.Lens' DeleteMultiplexResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
deleteMultiplexResponse_tags = Lens.lens (\DeleteMultiplexResponse' {tags} -> tags) (\s@DeleteMultiplexResponse' {} a -> s {tags = a} :: DeleteMultiplexResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Configuration for a multiplex event.
deleteMultiplexResponse_multiplexSettings :: Lens.Lens' DeleteMultiplexResponse (Prelude.Maybe MultiplexSettings)
deleteMultiplexResponse_multiplexSettings = Lens.lens (\DeleteMultiplexResponse' {multiplexSettings} -> multiplexSettings) (\s@DeleteMultiplexResponse' {} a -> s {multiplexSettings = a} :: DeleteMultiplexResponse)

-- | The response's http status code.
deleteMultiplexResponse_httpStatus :: Lens.Lens' DeleteMultiplexResponse Prelude.Int
deleteMultiplexResponse_httpStatus = Lens.lens (\DeleteMultiplexResponse' {httpStatus} -> httpStatus) (\s@DeleteMultiplexResponse' {} a -> s {httpStatus = a} :: DeleteMultiplexResponse)

instance Prelude.NFData DeleteMultiplexResponse
