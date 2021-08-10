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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeMultiplexResponse'
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

instance Prelude.Hashable DescribeMultiplex

instance Prelude.NFData DescribeMultiplex

instance Core.ToHeaders DescribeMultiplex where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeMultiplex where
  toPath DescribeMultiplex' {..} =
    Prelude.mconcat
      ["/prod/multiplexes/", Core.toBS multiplexId]

instance Core.ToQuery DescribeMultiplex where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for DescribeMultiplexResponse
--
-- /See:/ 'newDescribeMultiplexResponse' smart constructor.
data DescribeMultiplexResponse = DescribeMultiplexResponse'
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
  Prelude.Int ->
  DescribeMultiplexResponse
newDescribeMultiplexResponse pHttpStatus_ =
  DescribeMultiplexResponse'
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
describeMultiplexResponse_availabilityZones :: Lens.Lens' DescribeMultiplexResponse (Prelude.Maybe [Prelude.Text])
describeMultiplexResponse_availabilityZones = Lens.lens (\DescribeMultiplexResponse' {availabilityZones} -> availabilityZones) (\s@DescribeMultiplexResponse' {} a -> s {availabilityZones = a} :: DescribeMultiplexResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The unique arn of the multiplex.
describeMultiplexResponse_arn :: Lens.Lens' DescribeMultiplexResponse (Prelude.Maybe Prelude.Text)
describeMultiplexResponse_arn = Lens.lens (\DescribeMultiplexResponse' {arn} -> arn) (\s@DescribeMultiplexResponse' {} a -> s {arn = a} :: DescribeMultiplexResponse)

-- | The unique id of the multiplex.
describeMultiplexResponse_id :: Lens.Lens' DescribeMultiplexResponse (Prelude.Maybe Prelude.Text)
describeMultiplexResponse_id = Lens.lens (\DescribeMultiplexResponse' {id} -> id) (\s@DescribeMultiplexResponse' {} a -> s {id = a} :: DescribeMultiplexResponse)

-- | The number of currently healthy pipelines.
describeMultiplexResponse_pipelinesRunningCount :: Lens.Lens' DescribeMultiplexResponse (Prelude.Maybe Prelude.Int)
describeMultiplexResponse_pipelinesRunningCount = Lens.lens (\DescribeMultiplexResponse' {pipelinesRunningCount} -> pipelinesRunningCount) (\s@DescribeMultiplexResponse' {} a -> s {pipelinesRunningCount = a} :: DescribeMultiplexResponse)

-- | The number of programs in the multiplex.
describeMultiplexResponse_programCount :: Lens.Lens' DescribeMultiplexResponse (Prelude.Maybe Prelude.Int)
describeMultiplexResponse_programCount = Lens.lens (\DescribeMultiplexResponse' {programCount} -> programCount) (\s@DescribeMultiplexResponse' {} a -> s {programCount = a} :: DescribeMultiplexResponse)

-- | A list of the multiplex output destinations.
describeMultiplexResponse_destinations :: Lens.Lens' DescribeMultiplexResponse (Prelude.Maybe [MultiplexOutputDestination])
describeMultiplexResponse_destinations = Lens.lens (\DescribeMultiplexResponse' {destinations} -> destinations) (\s@DescribeMultiplexResponse' {} a -> s {destinations = a} :: DescribeMultiplexResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The current state of the multiplex.
describeMultiplexResponse_state :: Lens.Lens' DescribeMultiplexResponse (Prelude.Maybe MultiplexState)
describeMultiplexResponse_state = Lens.lens (\DescribeMultiplexResponse' {state} -> state) (\s@DescribeMultiplexResponse' {} a -> s {state = a} :: DescribeMultiplexResponse)

-- | The name of the multiplex.
describeMultiplexResponse_name :: Lens.Lens' DescribeMultiplexResponse (Prelude.Maybe Prelude.Text)
describeMultiplexResponse_name = Lens.lens (\DescribeMultiplexResponse' {name} -> name) (\s@DescribeMultiplexResponse' {} a -> s {name = a} :: DescribeMultiplexResponse)

-- | A collection of key-value pairs.
describeMultiplexResponse_tags :: Lens.Lens' DescribeMultiplexResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeMultiplexResponse_tags = Lens.lens (\DescribeMultiplexResponse' {tags} -> tags) (\s@DescribeMultiplexResponse' {} a -> s {tags = a} :: DescribeMultiplexResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Configuration for a multiplex event.
describeMultiplexResponse_multiplexSettings :: Lens.Lens' DescribeMultiplexResponse (Prelude.Maybe MultiplexSettings)
describeMultiplexResponse_multiplexSettings = Lens.lens (\DescribeMultiplexResponse' {multiplexSettings} -> multiplexSettings) (\s@DescribeMultiplexResponse' {} a -> s {multiplexSettings = a} :: DescribeMultiplexResponse)

-- | The response's http status code.
describeMultiplexResponse_httpStatus :: Lens.Lens' DescribeMultiplexResponse Prelude.Int
describeMultiplexResponse_httpStatus = Lens.lens (\DescribeMultiplexResponse' {httpStatus} -> httpStatus) (\s@DescribeMultiplexResponse' {} a -> s {httpStatus = a} :: DescribeMultiplexResponse)

instance Prelude.NFData DescribeMultiplexResponse
