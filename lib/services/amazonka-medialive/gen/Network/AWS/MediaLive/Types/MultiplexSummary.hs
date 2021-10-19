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
import qualified Network.AWS.Prelude as Prelude

-- | Placeholder documentation for MultiplexSummary
--
-- /See:/ 'newMultiplexSummary' smart constructor.
data MultiplexSummary = MultiplexSummary'
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
    -- | The name of the multiplex.
    name :: Prelude.Maybe Prelude.Text,
    -- | The unique id of the multiplex.
    id :: Prelude.Maybe Prelude.Text,
    -- | Configuration for a multiplex event.
    multiplexSettings :: Prelude.Maybe MultiplexSettingsSummary,
    -- | A collection of key-value pairs.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MultiplexSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'multiplexSummary_state' - The current state of the multiplex.
--
-- 'arn', 'multiplexSummary_arn' - The unique arn of the multiplex.
--
-- 'pipelinesRunningCount', 'multiplexSummary_pipelinesRunningCount' - The number of currently healthy pipelines.
--
-- 'availabilityZones', 'multiplexSummary_availabilityZones' - A list of availability zones for the multiplex.
--
-- 'programCount', 'multiplexSummary_programCount' - The number of programs in the multiplex.
--
-- 'name', 'multiplexSummary_name' - The name of the multiplex.
--
-- 'id', 'multiplexSummary_id' - The unique id of the multiplex.
--
-- 'multiplexSettings', 'multiplexSummary_multiplexSettings' - Configuration for a multiplex event.
--
-- 'tags', 'multiplexSummary_tags' - A collection of key-value pairs.
newMultiplexSummary ::
  MultiplexSummary
newMultiplexSummary =
  MultiplexSummary'
    { state = Prelude.Nothing,
      arn = Prelude.Nothing,
      pipelinesRunningCount = Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      programCount = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      multiplexSettings = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The current state of the multiplex.
multiplexSummary_state :: Lens.Lens' MultiplexSummary (Prelude.Maybe MultiplexState)
multiplexSummary_state = Lens.lens (\MultiplexSummary' {state} -> state) (\s@MultiplexSummary' {} a -> s {state = a} :: MultiplexSummary)

-- | The unique arn of the multiplex.
multiplexSummary_arn :: Lens.Lens' MultiplexSummary (Prelude.Maybe Prelude.Text)
multiplexSummary_arn = Lens.lens (\MultiplexSummary' {arn} -> arn) (\s@MultiplexSummary' {} a -> s {arn = a} :: MultiplexSummary)

-- | The number of currently healthy pipelines.
multiplexSummary_pipelinesRunningCount :: Lens.Lens' MultiplexSummary (Prelude.Maybe Prelude.Int)
multiplexSummary_pipelinesRunningCount = Lens.lens (\MultiplexSummary' {pipelinesRunningCount} -> pipelinesRunningCount) (\s@MultiplexSummary' {} a -> s {pipelinesRunningCount = a} :: MultiplexSummary)

-- | A list of availability zones for the multiplex.
multiplexSummary_availabilityZones :: Lens.Lens' MultiplexSummary (Prelude.Maybe [Prelude.Text])
multiplexSummary_availabilityZones = Lens.lens (\MultiplexSummary' {availabilityZones} -> availabilityZones) (\s@MultiplexSummary' {} a -> s {availabilityZones = a} :: MultiplexSummary) Prelude.. Lens.mapping Lens.coerced

-- | The number of programs in the multiplex.
multiplexSummary_programCount :: Lens.Lens' MultiplexSummary (Prelude.Maybe Prelude.Int)
multiplexSummary_programCount = Lens.lens (\MultiplexSummary' {programCount} -> programCount) (\s@MultiplexSummary' {} a -> s {programCount = a} :: MultiplexSummary)

-- | The name of the multiplex.
multiplexSummary_name :: Lens.Lens' MultiplexSummary (Prelude.Maybe Prelude.Text)
multiplexSummary_name = Lens.lens (\MultiplexSummary' {name} -> name) (\s@MultiplexSummary' {} a -> s {name = a} :: MultiplexSummary)

-- | The unique id of the multiplex.
multiplexSummary_id :: Lens.Lens' MultiplexSummary (Prelude.Maybe Prelude.Text)
multiplexSummary_id = Lens.lens (\MultiplexSummary' {id} -> id) (\s@MultiplexSummary' {} a -> s {id = a} :: MultiplexSummary)

-- | Configuration for a multiplex event.
multiplexSummary_multiplexSettings :: Lens.Lens' MultiplexSummary (Prelude.Maybe MultiplexSettingsSummary)
multiplexSummary_multiplexSettings = Lens.lens (\MultiplexSummary' {multiplexSettings} -> multiplexSettings) (\s@MultiplexSummary' {} a -> s {multiplexSettings = a} :: MultiplexSummary)

-- | A collection of key-value pairs.
multiplexSummary_tags :: Lens.Lens' MultiplexSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
multiplexSummary_tags = Lens.lens (\MultiplexSummary' {tags} -> tags) (\s@MultiplexSummary' {} a -> s {tags = a} :: MultiplexSummary) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON MultiplexSummary where
  parseJSON =
    Core.withObject
      "MultiplexSummary"
      ( \x ->
          MultiplexSummary'
            Prelude.<$> (x Core..:? "state")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "pipelinesRunningCount")
            Prelude.<*> ( x Core..:? "availabilityZones"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "programCount")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "multiplexSettings")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable MultiplexSummary

instance Prelude.NFData MultiplexSummary
