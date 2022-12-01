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
-- Module      : Amazonka.MediaLive.DeleteMultiplex
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a multiplex. The multiplex must be idle.
module Amazonka.MediaLive.DeleteMultiplex
  ( -- * Creating a Request
    DeleteMultiplex (..),
    newDeleteMultiplex,

    -- * Request Lenses
    deleteMultiplex_multiplexId,

    -- * Destructuring the Response
    DeleteMultiplexResponse (..),
    newDeleteMultiplexResponse,

    -- * Response Lenses
    deleteMultiplexResponse_tags,
    deleteMultiplexResponse_name,
    deleteMultiplexResponse_availabilityZones,
    deleteMultiplexResponse_arn,
    deleteMultiplexResponse_state,
    deleteMultiplexResponse_multiplexSettings,
    deleteMultiplexResponse_id,
    deleteMultiplexResponse_pipelinesRunningCount,
    deleteMultiplexResponse_destinations,
    deleteMultiplexResponse_programCount,
    deleteMultiplexResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteMultiplexResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "name")
            Prelude.<*> ( x Core..?> "availabilityZones"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "state")
            Prelude.<*> (x Core..?> "multiplexSettings")
            Prelude.<*> (x Core..?> "id")
            Prelude.<*> (x Core..?> "pipelinesRunningCount")
            Prelude.<*> (x Core..?> "destinations" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "programCount")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteMultiplex where
  hashWithSalt _salt DeleteMultiplex' {..} =
    _salt `Prelude.hashWithSalt` multiplexId

instance Prelude.NFData DeleteMultiplex where
  rnf DeleteMultiplex' {..} = Prelude.rnf multiplexId

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
  { -- | A collection of key-value pairs.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the multiplex.
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of availability zones for the multiplex.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The unique arn of the multiplex.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The current state of the multiplex.
    state :: Prelude.Maybe MultiplexState,
    -- | Configuration for a multiplex event.
    multiplexSettings :: Prelude.Maybe MultiplexSettings,
    -- | The unique id of the multiplex.
    id :: Prelude.Maybe Prelude.Text,
    -- | The number of currently healthy pipelines.
    pipelinesRunningCount :: Prelude.Maybe Prelude.Int,
    -- | A list of the multiplex output destinations.
    destinations :: Prelude.Maybe [MultiplexOutputDestination],
    -- | The number of programs in the multiplex.
    programCount :: Prelude.Maybe Prelude.Int,
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
-- 'tags', 'deleteMultiplexResponse_tags' - A collection of key-value pairs.
--
-- 'name', 'deleteMultiplexResponse_name' - The name of the multiplex.
--
-- 'availabilityZones', 'deleteMultiplexResponse_availabilityZones' - A list of availability zones for the multiplex.
--
-- 'arn', 'deleteMultiplexResponse_arn' - The unique arn of the multiplex.
--
-- 'state', 'deleteMultiplexResponse_state' - The current state of the multiplex.
--
-- 'multiplexSettings', 'deleteMultiplexResponse_multiplexSettings' - Configuration for a multiplex event.
--
-- 'id', 'deleteMultiplexResponse_id' - The unique id of the multiplex.
--
-- 'pipelinesRunningCount', 'deleteMultiplexResponse_pipelinesRunningCount' - The number of currently healthy pipelines.
--
-- 'destinations', 'deleteMultiplexResponse_destinations' - A list of the multiplex output destinations.
--
-- 'programCount', 'deleteMultiplexResponse_programCount' - The number of programs in the multiplex.
--
-- 'httpStatus', 'deleteMultiplexResponse_httpStatus' - The response's http status code.
newDeleteMultiplexResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteMultiplexResponse
newDeleteMultiplexResponse pHttpStatus_ =
  DeleteMultiplexResponse'
    { tags = Prelude.Nothing,
      name = Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      arn = Prelude.Nothing,
      state = Prelude.Nothing,
      multiplexSettings = Prelude.Nothing,
      id = Prelude.Nothing,
      pipelinesRunningCount = Prelude.Nothing,
      destinations = Prelude.Nothing,
      programCount = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of key-value pairs.
deleteMultiplexResponse_tags :: Lens.Lens' DeleteMultiplexResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
deleteMultiplexResponse_tags = Lens.lens (\DeleteMultiplexResponse' {tags} -> tags) (\s@DeleteMultiplexResponse' {} a -> s {tags = a} :: DeleteMultiplexResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the multiplex.
deleteMultiplexResponse_name :: Lens.Lens' DeleteMultiplexResponse (Prelude.Maybe Prelude.Text)
deleteMultiplexResponse_name = Lens.lens (\DeleteMultiplexResponse' {name} -> name) (\s@DeleteMultiplexResponse' {} a -> s {name = a} :: DeleteMultiplexResponse)

-- | A list of availability zones for the multiplex.
deleteMultiplexResponse_availabilityZones :: Lens.Lens' DeleteMultiplexResponse (Prelude.Maybe [Prelude.Text])
deleteMultiplexResponse_availabilityZones = Lens.lens (\DeleteMultiplexResponse' {availabilityZones} -> availabilityZones) (\s@DeleteMultiplexResponse' {} a -> s {availabilityZones = a} :: DeleteMultiplexResponse) Prelude.. Lens.mapping Lens.coerced

-- | The unique arn of the multiplex.
deleteMultiplexResponse_arn :: Lens.Lens' DeleteMultiplexResponse (Prelude.Maybe Prelude.Text)
deleteMultiplexResponse_arn = Lens.lens (\DeleteMultiplexResponse' {arn} -> arn) (\s@DeleteMultiplexResponse' {} a -> s {arn = a} :: DeleteMultiplexResponse)

-- | The current state of the multiplex.
deleteMultiplexResponse_state :: Lens.Lens' DeleteMultiplexResponse (Prelude.Maybe MultiplexState)
deleteMultiplexResponse_state = Lens.lens (\DeleteMultiplexResponse' {state} -> state) (\s@DeleteMultiplexResponse' {} a -> s {state = a} :: DeleteMultiplexResponse)

-- | Configuration for a multiplex event.
deleteMultiplexResponse_multiplexSettings :: Lens.Lens' DeleteMultiplexResponse (Prelude.Maybe MultiplexSettings)
deleteMultiplexResponse_multiplexSettings = Lens.lens (\DeleteMultiplexResponse' {multiplexSettings} -> multiplexSettings) (\s@DeleteMultiplexResponse' {} a -> s {multiplexSettings = a} :: DeleteMultiplexResponse)

-- | The unique id of the multiplex.
deleteMultiplexResponse_id :: Lens.Lens' DeleteMultiplexResponse (Prelude.Maybe Prelude.Text)
deleteMultiplexResponse_id = Lens.lens (\DeleteMultiplexResponse' {id} -> id) (\s@DeleteMultiplexResponse' {} a -> s {id = a} :: DeleteMultiplexResponse)

-- | The number of currently healthy pipelines.
deleteMultiplexResponse_pipelinesRunningCount :: Lens.Lens' DeleteMultiplexResponse (Prelude.Maybe Prelude.Int)
deleteMultiplexResponse_pipelinesRunningCount = Lens.lens (\DeleteMultiplexResponse' {pipelinesRunningCount} -> pipelinesRunningCount) (\s@DeleteMultiplexResponse' {} a -> s {pipelinesRunningCount = a} :: DeleteMultiplexResponse)

-- | A list of the multiplex output destinations.
deleteMultiplexResponse_destinations :: Lens.Lens' DeleteMultiplexResponse (Prelude.Maybe [MultiplexOutputDestination])
deleteMultiplexResponse_destinations = Lens.lens (\DeleteMultiplexResponse' {destinations} -> destinations) (\s@DeleteMultiplexResponse' {} a -> s {destinations = a} :: DeleteMultiplexResponse) Prelude.. Lens.mapping Lens.coerced

-- | The number of programs in the multiplex.
deleteMultiplexResponse_programCount :: Lens.Lens' DeleteMultiplexResponse (Prelude.Maybe Prelude.Int)
deleteMultiplexResponse_programCount = Lens.lens (\DeleteMultiplexResponse' {programCount} -> programCount) (\s@DeleteMultiplexResponse' {} a -> s {programCount = a} :: DeleteMultiplexResponse)

-- | The response's http status code.
deleteMultiplexResponse_httpStatus :: Lens.Lens' DeleteMultiplexResponse Prelude.Int
deleteMultiplexResponse_httpStatus = Lens.lens (\DeleteMultiplexResponse' {httpStatus} -> httpStatus) (\s@DeleteMultiplexResponse' {} a -> s {httpStatus = a} :: DeleteMultiplexResponse)

instance Prelude.NFData DeleteMultiplexResponse where
  rnf DeleteMultiplexResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf multiplexSettings
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf pipelinesRunningCount
      `Prelude.seq` Prelude.rnf destinations
      `Prelude.seq` Prelude.rnf programCount
      `Prelude.seq` Prelude.rnf httpStatus
