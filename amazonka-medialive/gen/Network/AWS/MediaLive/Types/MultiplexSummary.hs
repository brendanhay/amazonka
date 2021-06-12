{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.MultiplexSettingsSummary
import Network.AWS.MediaLive.Types.MultiplexState

-- | Placeholder documentation for MultiplexSummary
--
-- /See:/ 'newMultiplexSummary' smart constructor.
data MultiplexSummary = MultiplexSummary'
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
    -- | The current state of the multiplex.
    state :: Core.Maybe MultiplexState,
    -- | The name of the multiplex.
    name :: Core.Maybe Core.Text,
    -- | A collection of key-value pairs.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Configuration for a multiplex event.
    multiplexSettings :: Core.Maybe MultiplexSettingsSummary
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MultiplexSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZones', 'multiplexSummary_availabilityZones' - A list of availability zones for the multiplex.
--
-- 'arn', 'multiplexSummary_arn' - The unique arn of the multiplex.
--
-- 'id', 'multiplexSummary_id' - The unique id of the multiplex.
--
-- 'pipelinesRunningCount', 'multiplexSummary_pipelinesRunningCount' - The number of currently healthy pipelines.
--
-- 'programCount', 'multiplexSummary_programCount' - The number of programs in the multiplex.
--
-- 'state', 'multiplexSummary_state' - The current state of the multiplex.
--
-- 'name', 'multiplexSummary_name' - The name of the multiplex.
--
-- 'tags', 'multiplexSummary_tags' - A collection of key-value pairs.
--
-- 'multiplexSettings', 'multiplexSummary_multiplexSettings' - Configuration for a multiplex event.
newMultiplexSummary ::
  MultiplexSummary
newMultiplexSummary =
  MultiplexSummary'
    { availabilityZones = Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      pipelinesRunningCount = Core.Nothing,
      programCount = Core.Nothing,
      state = Core.Nothing,
      name = Core.Nothing,
      tags = Core.Nothing,
      multiplexSettings = Core.Nothing
    }

-- | A list of availability zones for the multiplex.
multiplexSummary_availabilityZones :: Lens.Lens' MultiplexSummary (Core.Maybe [Core.Text])
multiplexSummary_availabilityZones = Lens.lens (\MultiplexSummary' {availabilityZones} -> availabilityZones) (\s@MultiplexSummary' {} a -> s {availabilityZones = a} :: MultiplexSummary) Core.. Lens.mapping Lens._Coerce

-- | The unique arn of the multiplex.
multiplexSummary_arn :: Lens.Lens' MultiplexSummary (Core.Maybe Core.Text)
multiplexSummary_arn = Lens.lens (\MultiplexSummary' {arn} -> arn) (\s@MultiplexSummary' {} a -> s {arn = a} :: MultiplexSummary)

-- | The unique id of the multiplex.
multiplexSummary_id :: Lens.Lens' MultiplexSummary (Core.Maybe Core.Text)
multiplexSummary_id = Lens.lens (\MultiplexSummary' {id} -> id) (\s@MultiplexSummary' {} a -> s {id = a} :: MultiplexSummary)

-- | The number of currently healthy pipelines.
multiplexSummary_pipelinesRunningCount :: Lens.Lens' MultiplexSummary (Core.Maybe Core.Int)
multiplexSummary_pipelinesRunningCount = Lens.lens (\MultiplexSummary' {pipelinesRunningCount} -> pipelinesRunningCount) (\s@MultiplexSummary' {} a -> s {pipelinesRunningCount = a} :: MultiplexSummary)

-- | The number of programs in the multiplex.
multiplexSummary_programCount :: Lens.Lens' MultiplexSummary (Core.Maybe Core.Int)
multiplexSummary_programCount = Lens.lens (\MultiplexSummary' {programCount} -> programCount) (\s@MultiplexSummary' {} a -> s {programCount = a} :: MultiplexSummary)

-- | The current state of the multiplex.
multiplexSummary_state :: Lens.Lens' MultiplexSummary (Core.Maybe MultiplexState)
multiplexSummary_state = Lens.lens (\MultiplexSummary' {state} -> state) (\s@MultiplexSummary' {} a -> s {state = a} :: MultiplexSummary)

-- | The name of the multiplex.
multiplexSummary_name :: Lens.Lens' MultiplexSummary (Core.Maybe Core.Text)
multiplexSummary_name = Lens.lens (\MultiplexSummary' {name} -> name) (\s@MultiplexSummary' {} a -> s {name = a} :: MultiplexSummary)

-- | A collection of key-value pairs.
multiplexSummary_tags :: Lens.Lens' MultiplexSummary (Core.Maybe (Core.HashMap Core.Text Core.Text))
multiplexSummary_tags = Lens.lens (\MultiplexSummary' {tags} -> tags) (\s@MultiplexSummary' {} a -> s {tags = a} :: MultiplexSummary) Core.. Lens.mapping Lens._Coerce

-- | Configuration for a multiplex event.
multiplexSummary_multiplexSettings :: Lens.Lens' MultiplexSummary (Core.Maybe MultiplexSettingsSummary)
multiplexSummary_multiplexSettings = Lens.lens (\MultiplexSummary' {multiplexSettings} -> multiplexSettings) (\s@MultiplexSummary' {} a -> s {multiplexSettings = a} :: MultiplexSummary)

instance Core.FromJSON MultiplexSummary where
  parseJSON =
    Core.withObject
      "MultiplexSummary"
      ( \x ->
          MultiplexSummary'
            Core.<$> (x Core..:? "availabilityZones" Core..!= Core.mempty)
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "pipelinesRunningCount")
            Core.<*> (x Core..:? "programCount")
            Core.<*> (x Core..:? "state")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "multiplexSettings")
      )

instance Core.Hashable MultiplexSummary

instance Core.NFData MultiplexSummary
