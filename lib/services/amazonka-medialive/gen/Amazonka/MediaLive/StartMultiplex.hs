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
-- Module      : Amazonka.MediaLive.StartMultiplex
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Start (run) the multiplex. Starting the multiplex does not start the
-- channels. You must explicitly start each channel.
module Amazonka.MediaLive.StartMultiplex
  ( -- * Creating a Request
    StartMultiplex (..),
    newStartMultiplex,

    -- * Request Lenses
    startMultiplex_multiplexId,

    -- * Destructuring the Response
    StartMultiplexResponse (..),
    newStartMultiplexResponse,

    -- * Response Lenses
    startMultiplexResponse_arn,
    startMultiplexResponse_availabilityZones,
    startMultiplexResponse_destinations,
    startMultiplexResponse_id,
    startMultiplexResponse_multiplexSettings,
    startMultiplexResponse_name,
    startMultiplexResponse_pipelinesRunningCount,
    startMultiplexResponse_programCount,
    startMultiplexResponse_state,
    startMultiplexResponse_tags,
    startMultiplexResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Placeholder documentation for StartMultiplexRequest
--
-- /See:/ 'newStartMultiplex' smart constructor.
data StartMultiplex = StartMultiplex'
  { -- | The ID of the multiplex.
    multiplexId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  StartMultiplex
newStartMultiplex pMultiplexId_ =
  StartMultiplex' {multiplexId = pMultiplexId_}

-- | The ID of the multiplex.
startMultiplex_multiplexId :: Lens.Lens' StartMultiplex Prelude.Text
startMultiplex_multiplexId = Lens.lens (\StartMultiplex' {multiplexId} -> multiplexId) (\s@StartMultiplex' {} a -> s {multiplexId = a} :: StartMultiplex)

instance Core.AWSRequest StartMultiplex where
  type
    AWSResponse StartMultiplex =
      StartMultiplexResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartMultiplexResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> ( x
                            Data..?> "availabilityZones"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "destinations" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "multiplexSettings")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "pipelinesRunningCount")
            Prelude.<*> (x Data..?> "programCount")
            Prelude.<*> (x Data..?> "state")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartMultiplex where
  hashWithSalt _salt StartMultiplex' {..} =
    _salt `Prelude.hashWithSalt` multiplexId

instance Prelude.NFData StartMultiplex where
  rnf StartMultiplex' {..} = Prelude.rnf multiplexId

instance Data.ToHeaders StartMultiplex where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartMultiplex where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath StartMultiplex where
  toPath StartMultiplex' {..} =
    Prelude.mconcat
      [ "/prod/multiplexes/",
        Data.toBS multiplexId,
        "/start"
      ]

instance Data.ToQuery StartMultiplex where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for StartMultiplexResponse
--
-- /See:/ 'newStartMultiplexResponse' smart constructor.
data StartMultiplexResponse = StartMultiplexResponse'
  { -- | The unique arn of the multiplex.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A list of availability zones for the multiplex.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | A list of the multiplex output destinations.
    destinations :: Prelude.Maybe [MultiplexOutputDestination],
    -- | The unique id of the multiplex.
    id :: Prelude.Maybe Prelude.Text,
    -- | Configuration for a multiplex event.
    multiplexSettings :: Prelude.Maybe MultiplexSettings,
    -- | The name of the multiplex.
    name :: Prelude.Maybe Prelude.Text,
    -- | The number of currently healthy pipelines.
    pipelinesRunningCount :: Prelude.Maybe Prelude.Int,
    -- | The number of programs in the multiplex.
    programCount :: Prelude.Maybe Prelude.Int,
    -- | The current state of the multiplex.
    state :: Prelude.Maybe MultiplexState,
    -- | A collection of key-value pairs.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartMultiplexResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'startMultiplexResponse_arn' - The unique arn of the multiplex.
--
-- 'availabilityZones', 'startMultiplexResponse_availabilityZones' - A list of availability zones for the multiplex.
--
-- 'destinations', 'startMultiplexResponse_destinations' - A list of the multiplex output destinations.
--
-- 'id', 'startMultiplexResponse_id' - The unique id of the multiplex.
--
-- 'multiplexSettings', 'startMultiplexResponse_multiplexSettings' - Configuration for a multiplex event.
--
-- 'name', 'startMultiplexResponse_name' - The name of the multiplex.
--
-- 'pipelinesRunningCount', 'startMultiplexResponse_pipelinesRunningCount' - The number of currently healthy pipelines.
--
-- 'programCount', 'startMultiplexResponse_programCount' - The number of programs in the multiplex.
--
-- 'state', 'startMultiplexResponse_state' - The current state of the multiplex.
--
-- 'tags', 'startMultiplexResponse_tags' - A collection of key-value pairs.
--
-- 'httpStatus', 'startMultiplexResponse_httpStatus' - The response's http status code.
newStartMultiplexResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartMultiplexResponse
newStartMultiplexResponse pHttpStatus_ =
  StartMultiplexResponse'
    { arn = Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      destinations = Prelude.Nothing,
      id = Prelude.Nothing,
      multiplexSettings = Prelude.Nothing,
      name = Prelude.Nothing,
      pipelinesRunningCount = Prelude.Nothing,
      programCount = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique arn of the multiplex.
startMultiplexResponse_arn :: Lens.Lens' StartMultiplexResponse (Prelude.Maybe Prelude.Text)
startMultiplexResponse_arn = Lens.lens (\StartMultiplexResponse' {arn} -> arn) (\s@StartMultiplexResponse' {} a -> s {arn = a} :: StartMultiplexResponse)

-- | A list of availability zones for the multiplex.
startMultiplexResponse_availabilityZones :: Lens.Lens' StartMultiplexResponse (Prelude.Maybe [Prelude.Text])
startMultiplexResponse_availabilityZones = Lens.lens (\StartMultiplexResponse' {availabilityZones} -> availabilityZones) (\s@StartMultiplexResponse' {} a -> s {availabilityZones = a} :: StartMultiplexResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of the multiplex output destinations.
startMultiplexResponse_destinations :: Lens.Lens' StartMultiplexResponse (Prelude.Maybe [MultiplexOutputDestination])
startMultiplexResponse_destinations = Lens.lens (\StartMultiplexResponse' {destinations} -> destinations) (\s@StartMultiplexResponse' {} a -> s {destinations = a} :: StartMultiplexResponse) Prelude.. Lens.mapping Lens.coerced

-- | The unique id of the multiplex.
startMultiplexResponse_id :: Lens.Lens' StartMultiplexResponse (Prelude.Maybe Prelude.Text)
startMultiplexResponse_id = Lens.lens (\StartMultiplexResponse' {id} -> id) (\s@StartMultiplexResponse' {} a -> s {id = a} :: StartMultiplexResponse)

-- | Configuration for a multiplex event.
startMultiplexResponse_multiplexSettings :: Lens.Lens' StartMultiplexResponse (Prelude.Maybe MultiplexSettings)
startMultiplexResponse_multiplexSettings = Lens.lens (\StartMultiplexResponse' {multiplexSettings} -> multiplexSettings) (\s@StartMultiplexResponse' {} a -> s {multiplexSettings = a} :: StartMultiplexResponse)

-- | The name of the multiplex.
startMultiplexResponse_name :: Lens.Lens' StartMultiplexResponse (Prelude.Maybe Prelude.Text)
startMultiplexResponse_name = Lens.lens (\StartMultiplexResponse' {name} -> name) (\s@StartMultiplexResponse' {} a -> s {name = a} :: StartMultiplexResponse)

-- | The number of currently healthy pipelines.
startMultiplexResponse_pipelinesRunningCount :: Lens.Lens' StartMultiplexResponse (Prelude.Maybe Prelude.Int)
startMultiplexResponse_pipelinesRunningCount = Lens.lens (\StartMultiplexResponse' {pipelinesRunningCount} -> pipelinesRunningCount) (\s@StartMultiplexResponse' {} a -> s {pipelinesRunningCount = a} :: StartMultiplexResponse)

-- | The number of programs in the multiplex.
startMultiplexResponse_programCount :: Lens.Lens' StartMultiplexResponse (Prelude.Maybe Prelude.Int)
startMultiplexResponse_programCount = Lens.lens (\StartMultiplexResponse' {programCount} -> programCount) (\s@StartMultiplexResponse' {} a -> s {programCount = a} :: StartMultiplexResponse)

-- | The current state of the multiplex.
startMultiplexResponse_state :: Lens.Lens' StartMultiplexResponse (Prelude.Maybe MultiplexState)
startMultiplexResponse_state = Lens.lens (\StartMultiplexResponse' {state} -> state) (\s@StartMultiplexResponse' {} a -> s {state = a} :: StartMultiplexResponse)

-- | A collection of key-value pairs.
startMultiplexResponse_tags :: Lens.Lens' StartMultiplexResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startMultiplexResponse_tags = Lens.lens (\StartMultiplexResponse' {tags} -> tags) (\s@StartMultiplexResponse' {} a -> s {tags = a} :: StartMultiplexResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
startMultiplexResponse_httpStatus :: Lens.Lens' StartMultiplexResponse Prelude.Int
startMultiplexResponse_httpStatus = Lens.lens (\StartMultiplexResponse' {httpStatus} -> httpStatus) (\s@StartMultiplexResponse' {} a -> s {httpStatus = a} :: StartMultiplexResponse)

instance Prelude.NFData StartMultiplexResponse where
  rnf StartMultiplexResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf destinations
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf multiplexSettings
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf pipelinesRunningCount
      `Prelude.seq` Prelude.rnf programCount
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
