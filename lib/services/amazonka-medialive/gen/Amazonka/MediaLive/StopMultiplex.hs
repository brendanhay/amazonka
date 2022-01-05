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
-- Module      : Amazonka.MediaLive.StopMultiplex
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running multiplex. If the multiplex isn\'t running, this action
-- has no effect.
module Amazonka.MediaLive.StopMultiplex
  ( -- * Creating a Request
    StopMultiplex (..),
    newStopMultiplex,

    -- * Request Lenses
    stopMultiplex_multiplexId,

    -- * Destructuring the Response
    StopMultiplexResponse (..),
    newStopMultiplexResponse,

    -- * Response Lenses
    stopMultiplexResponse_state,
    stopMultiplexResponse_arn,
    stopMultiplexResponse_pipelinesRunningCount,
    stopMultiplexResponse_availabilityZones,
    stopMultiplexResponse_programCount,
    stopMultiplexResponse_destinations,
    stopMultiplexResponse_name,
    stopMultiplexResponse_id,
    stopMultiplexResponse_multiplexSettings,
    stopMultiplexResponse_tags,
    stopMultiplexResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Placeholder documentation for StopMultiplexRequest
--
-- /See:/ 'newStopMultiplex' smart constructor.
data StopMultiplex = StopMultiplex'
  { -- | The ID of the multiplex.
    multiplexId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  StopMultiplex
newStopMultiplex pMultiplexId_ =
  StopMultiplex' {multiplexId = pMultiplexId_}

-- | The ID of the multiplex.
stopMultiplex_multiplexId :: Lens.Lens' StopMultiplex Prelude.Text
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
            Prelude.<$> (x Core..?> "state")
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "pipelinesRunningCount")
            Prelude.<*> ( x Core..?> "availabilityZones"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "programCount")
            Prelude.<*> (x Core..?> "destinations" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "name")
            Prelude.<*> (x Core..?> "id")
            Prelude.<*> (x Core..?> "multiplexSettings")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopMultiplex where
  hashWithSalt _salt StopMultiplex' {..} =
    _salt `Prelude.hashWithSalt` multiplexId

instance Prelude.NFData StopMultiplex where
  rnf StopMultiplex' {..} = Prelude.rnf multiplexId

instance Core.ToHeaders StopMultiplex where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StopMultiplex where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath StopMultiplex where
  toPath StopMultiplex' {..} =
    Prelude.mconcat
      [ "/prod/multiplexes/",
        Core.toBS multiplexId,
        "/stop"
      ]

instance Core.ToQuery StopMultiplex where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for StopMultiplexResponse
--
-- /See:/ 'newStopMultiplexResponse' smart constructor.
data StopMultiplexResponse = StopMultiplexResponse'
  { -- | The current state of the multiplex.
    state :: Prelude.Maybe MultiplexState,
    -- | The unique arn of the multiplex.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The number of currently healthy pipelines.
    pipelinesRunningCount :: Prelude.Maybe Prelude.Int,
    -- | A list of availability zones for the multiplex.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The number of programs in the multiplex.
    programCount :: Prelude.Maybe Prelude.Int,
    -- | A list of the multiplex output destinations.
    destinations :: Prelude.Maybe [MultiplexOutputDestination],
    -- | The name of the multiplex.
    name :: Prelude.Maybe Prelude.Text,
    -- | The unique id of the multiplex.
    id :: Prelude.Maybe Prelude.Text,
    -- | Configuration for a multiplex event.
    multiplexSettings :: Prelude.Maybe MultiplexSettings,
    -- | A collection of key-value pairs.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopMultiplexResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'stopMultiplexResponse_state' - The current state of the multiplex.
--
-- 'arn', 'stopMultiplexResponse_arn' - The unique arn of the multiplex.
--
-- 'pipelinesRunningCount', 'stopMultiplexResponse_pipelinesRunningCount' - The number of currently healthy pipelines.
--
-- 'availabilityZones', 'stopMultiplexResponse_availabilityZones' - A list of availability zones for the multiplex.
--
-- 'programCount', 'stopMultiplexResponse_programCount' - The number of programs in the multiplex.
--
-- 'destinations', 'stopMultiplexResponse_destinations' - A list of the multiplex output destinations.
--
-- 'name', 'stopMultiplexResponse_name' - The name of the multiplex.
--
-- 'id', 'stopMultiplexResponse_id' - The unique id of the multiplex.
--
-- 'multiplexSettings', 'stopMultiplexResponse_multiplexSettings' - Configuration for a multiplex event.
--
-- 'tags', 'stopMultiplexResponse_tags' - A collection of key-value pairs.
--
-- 'httpStatus', 'stopMultiplexResponse_httpStatus' - The response's http status code.
newStopMultiplexResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopMultiplexResponse
newStopMultiplexResponse pHttpStatus_ =
  StopMultiplexResponse'
    { state = Prelude.Nothing,
      arn = Prelude.Nothing,
      pipelinesRunningCount = Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      programCount = Prelude.Nothing,
      destinations = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      multiplexSettings = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current state of the multiplex.
stopMultiplexResponse_state :: Lens.Lens' StopMultiplexResponse (Prelude.Maybe MultiplexState)
stopMultiplexResponse_state = Lens.lens (\StopMultiplexResponse' {state} -> state) (\s@StopMultiplexResponse' {} a -> s {state = a} :: StopMultiplexResponse)

-- | The unique arn of the multiplex.
stopMultiplexResponse_arn :: Lens.Lens' StopMultiplexResponse (Prelude.Maybe Prelude.Text)
stopMultiplexResponse_arn = Lens.lens (\StopMultiplexResponse' {arn} -> arn) (\s@StopMultiplexResponse' {} a -> s {arn = a} :: StopMultiplexResponse)

-- | The number of currently healthy pipelines.
stopMultiplexResponse_pipelinesRunningCount :: Lens.Lens' StopMultiplexResponse (Prelude.Maybe Prelude.Int)
stopMultiplexResponse_pipelinesRunningCount = Lens.lens (\StopMultiplexResponse' {pipelinesRunningCount} -> pipelinesRunningCount) (\s@StopMultiplexResponse' {} a -> s {pipelinesRunningCount = a} :: StopMultiplexResponse)

-- | A list of availability zones for the multiplex.
stopMultiplexResponse_availabilityZones :: Lens.Lens' StopMultiplexResponse (Prelude.Maybe [Prelude.Text])
stopMultiplexResponse_availabilityZones = Lens.lens (\StopMultiplexResponse' {availabilityZones} -> availabilityZones) (\s@StopMultiplexResponse' {} a -> s {availabilityZones = a} :: StopMultiplexResponse) Prelude.. Lens.mapping Lens.coerced

-- | The number of programs in the multiplex.
stopMultiplexResponse_programCount :: Lens.Lens' StopMultiplexResponse (Prelude.Maybe Prelude.Int)
stopMultiplexResponse_programCount = Lens.lens (\StopMultiplexResponse' {programCount} -> programCount) (\s@StopMultiplexResponse' {} a -> s {programCount = a} :: StopMultiplexResponse)

-- | A list of the multiplex output destinations.
stopMultiplexResponse_destinations :: Lens.Lens' StopMultiplexResponse (Prelude.Maybe [MultiplexOutputDestination])
stopMultiplexResponse_destinations = Lens.lens (\StopMultiplexResponse' {destinations} -> destinations) (\s@StopMultiplexResponse' {} a -> s {destinations = a} :: StopMultiplexResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the multiplex.
stopMultiplexResponse_name :: Lens.Lens' StopMultiplexResponse (Prelude.Maybe Prelude.Text)
stopMultiplexResponse_name = Lens.lens (\StopMultiplexResponse' {name} -> name) (\s@StopMultiplexResponse' {} a -> s {name = a} :: StopMultiplexResponse)

-- | The unique id of the multiplex.
stopMultiplexResponse_id :: Lens.Lens' StopMultiplexResponse (Prelude.Maybe Prelude.Text)
stopMultiplexResponse_id = Lens.lens (\StopMultiplexResponse' {id} -> id) (\s@StopMultiplexResponse' {} a -> s {id = a} :: StopMultiplexResponse)

-- | Configuration for a multiplex event.
stopMultiplexResponse_multiplexSettings :: Lens.Lens' StopMultiplexResponse (Prelude.Maybe MultiplexSettings)
stopMultiplexResponse_multiplexSettings = Lens.lens (\StopMultiplexResponse' {multiplexSettings} -> multiplexSettings) (\s@StopMultiplexResponse' {} a -> s {multiplexSettings = a} :: StopMultiplexResponse)

-- | A collection of key-value pairs.
stopMultiplexResponse_tags :: Lens.Lens' StopMultiplexResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
stopMultiplexResponse_tags = Lens.lens (\StopMultiplexResponse' {tags} -> tags) (\s@StopMultiplexResponse' {} a -> s {tags = a} :: StopMultiplexResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
stopMultiplexResponse_httpStatus :: Lens.Lens' StopMultiplexResponse Prelude.Int
stopMultiplexResponse_httpStatus = Lens.lens (\StopMultiplexResponse' {httpStatus} -> httpStatus) (\s@StopMultiplexResponse' {} a -> s {httpStatus = a} :: StopMultiplexResponse)

instance Prelude.NFData StopMultiplexResponse where
  rnf StopMultiplexResponse' {..} =
    Prelude.rnf state
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf pipelinesRunningCount
      `Prelude.seq` Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf programCount
      `Prelude.seq` Prelude.rnf destinations
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf multiplexSettings
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
