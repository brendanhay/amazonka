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
-- Module      : Amazonka.MediaLive.DescribeMultiplex
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a multiplex.
module Amazonka.MediaLive.DescribeMultiplex
  ( -- * Creating a Request
    DescribeMultiplex (..),
    newDescribeMultiplex,

    -- * Request Lenses
    describeMultiplex_multiplexId,

    -- * Destructuring the Response
    DescribeMultiplexResponse (..),
    newDescribeMultiplexResponse,

    -- * Response Lenses
    describeMultiplexResponse_arn,
    describeMultiplexResponse_availabilityZones,
    describeMultiplexResponse_destinations,
    describeMultiplexResponse_id,
    describeMultiplexResponse_multiplexSettings,
    describeMultiplexResponse_name,
    describeMultiplexResponse_pipelinesRunningCount,
    describeMultiplexResponse_programCount,
    describeMultiplexResponse_state,
    describeMultiplexResponse_tags,
    describeMultiplexResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Placeholder documentation for DescribeMultiplexRequest
--
-- /See:/ 'newDescribeMultiplex' smart constructor.
data DescribeMultiplex = DescribeMultiplex'
  { -- | The ID of the multiplex.
    multiplexId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeMultiplex
newDescribeMultiplex pMultiplexId_ =
  DescribeMultiplex' {multiplexId = pMultiplexId_}

-- | The ID of the multiplex.
describeMultiplex_multiplexId :: Lens.Lens' DescribeMultiplex Prelude.Text
describeMultiplex_multiplexId = Lens.lens (\DescribeMultiplex' {multiplexId} -> multiplexId) (\s@DescribeMultiplex' {} a -> s {multiplexId = a} :: DescribeMultiplex)

instance Core.AWSRequest DescribeMultiplex where
  type
    AWSResponse DescribeMultiplex =
      DescribeMultiplexResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMultiplexResponse'
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

instance Prelude.Hashable DescribeMultiplex where
  hashWithSalt _salt DescribeMultiplex' {..} =
    _salt `Prelude.hashWithSalt` multiplexId

instance Prelude.NFData DescribeMultiplex where
  rnf DescribeMultiplex' {..} = Prelude.rnf multiplexId

instance Data.ToHeaders DescribeMultiplex where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeMultiplex where
  toPath DescribeMultiplex' {..} =
    Prelude.mconcat
      ["/prod/multiplexes/", Data.toBS multiplexId]

instance Data.ToQuery DescribeMultiplex where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for DescribeMultiplexResponse
--
-- /See:/ 'newDescribeMultiplexResponse' smart constructor.
data DescribeMultiplexResponse = DescribeMultiplexResponse'
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
-- Create a value of 'DescribeMultiplexResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'describeMultiplexResponse_arn' - The unique arn of the multiplex.
--
-- 'availabilityZones', 'describeMultiplexResponse_availabilityZones' - A list of availability zones for the multiplex.
--
-- 'destinations', 'describeMultiplexResponse_destinations' - A list of the multiplex output destinations.
--
-- 'id', 'describeMultiplexResponse_id' - The unique id of the multiplex.
--
-- 'multiplexSettings', 'describeMultiplexResponse_multiplexSettings' - Configuration for a multiplex event.
--
-- 'name', 'describeMultiplexResponse_name' - The name of the multiplex.
--
-- 'pipelinesRunningCount', 'describeMultiplexResponse_pipelinesRunningCount' - The number of currently healthy pipelines.
--
-- 'programCount', 'describeMultiplexResponse_programCount' - The number of programs in the multiplex.
--
-- 'state', 'describeMultiplexResponse_state' - The current state of the multiplex.
--
-- 'tags', 'describeMultiplexResponse_tags' - A collection of key-value pairs.
--
-- 'httpStatus', 'describeMultiplexResponse_httpStatus' - The response's http status code.
newDescribeMultiplexResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeMultiplexResponse
newDescribeMultiplexResponse pHttpStatus_ =
  DescribeMultiplexResponse'
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
describeMultiplexResponse_arn :: Lens.Lens' DescribeMultiplexResponse (Prelude.Maybe Prelude.Text)
describeMultiplexResponse_arn = Lens.lens (\DescribeMultiplexResponse' {arn} -> arn) (\s@DescribeMultiplexResponse' {} a -> s {arn = a} :: DescribeMultiplexResponse)

-- | A list of availability zones for the multiplex.
describeMultiplexResponse_availabilityZones :: Lens.Lens' DescribeMultiplexResponse (Prelude.Maybe [Prelude.Text])
describeMultiplexResponse_availabilityZones = Lens.lens (\DescribeMultiplexResponse' {availabilityZones} -> availabilityZones) (\s@DescribeMultiplexResponse' {} a -> s {availabilityZones = a} :: DescribeMultiplexResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of the multiplex output destinations.
describeMultiplexResponse_destinations :: Lens.Lens' DescribeMultiplexResponse (Prelude.Maybe [MultiplexOutputDestination])
describeMultiplexResponse_destinations = Lens.lens (\DescribeMultiplexResponse' {destinations} -> destinations) (\s@DescribeMultiplexResponse' {} a -> s {destinations = a} :: DescribeMultiplexResponse) Prelude.. Lens.mapping Lens.coerced

-- | The unique id of the multiplex.
describeMultiplexResponse_id :: Lens.Lens' DescribeMultiplexResponse (Prelude.Maybe Prelude.Text)
describeMultiplexResponse_id = Lens.lens (\DescribeMultiplexResponse' {id} -> id) (\s@DescribeMultiplexResponse' {} a -> s {id = a} :: DescribeMultiplexResponse)

-- | Configuration for a multiplex event.
describeMultiplexResponse_multiplexSettings :: Lens.Lens' DescribeMultiplexResponse (Prelude.Maybe MultiplexSettings)
describeMultiplexResponse_multiplexSettings = Lens.lens (\DescribeMultiplexResponse' {multiplexSettings} -> multiplexSettings) (\s@DescribeMultiplexResponse' {} a -> s {multiplexSettings = a} :: DescribeMultiplexResponse)

-- | The name of the multiplex.
describeMultiplexResponse_name :: Lens.Lens' DescribeMultiplexResponse (Prelude.Maybe Prelude.Text)
describeMultiplexResponse_name = Lens.lens (\DescribeMultiplexResponse' {name} -> name) (\s@DescribeMultiplexResponse' {} a -> s {name = a} :: DescribeMultiplexResponse)

-- | The number of currently healthy pipelines.
describeMultiplexResponse_pipelinesRunningCount :: Lens.Lens' DescribeMultiplexResponse (Prelude.Maybe Prelude.Int)
describeMultiplexResponse_pipelinesRunningCount = Lens.lens (\DescribeMultiplexResponse' {pipelinesRunningCount} -> pipelinesRunningCount) (\s@DescribeMultiplexResponse' {} a -> s {pipelinesRunningCount = a} :: DescribeMultiplexResponse)

-- | The number of programs in the multiplex.
describeMultiplexResponse_programCount :: Lens.Lens' DescribeMultiplexResponse (Prelude.Maybe Prelude.Int)
describeMultiplexResponse_programCount = Lens.lens (\DescribeMultiplexResponse' {programCount} -> programCount) (\s@DescribeMultiplexResponse' {} a -> s {programCount = a} :: DescribeMultiplexResponse)

-- | The current state of the multiplex.
describeMultiplexResponse_state :: Lens.Lens' DescribeMultiplexResponse (Prelude.Maybe MultiplexState)
describeMultiplexResponse_state = Lens.lens (\DescribeMultiplexResponse' {state} -> state) (\s@DescribeMultiplexResponse' {} a -> s {state = a} :: DescribeMultiplexResponse)

-- | A collection of key-value pairs.
describeMultiplexResponse_tags :: Lens.Lens' DescribeMultiplexResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeMultiplexResponse_tags = Lens.lens (\DescribeMultiplexResponse' {tags} -> tags) (\s@DescribeMultiplexResponse' {} a -> s {tags = a} :: DescribeMultiplexResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeMultiplexResponse_httpStatus :: Lens.Lens' DescribeMultiplexResponse Prelude.Int
describeMultiplexResponse_httpStatus = Lens.lens (\DescribeMultiplexResponse' {httpStatus} -> httpStatus) (\s@DescribeMultiplexResponse' {} a -> s {httpStatus = a} :: DescribeMultiplexResponse)

instance Prelude.NFData DescribeMultiplexResponse where
  rnf DescribeMultiplexResponse' {..} =
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
