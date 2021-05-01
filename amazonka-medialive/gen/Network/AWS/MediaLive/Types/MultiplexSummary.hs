{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.MultiplexSettingsSummary
import Network.AWS.MediaLive.Types.MultiplexState
import qualified Network.AWS.Prelude as Prelude

-- | Placeholder documentation for MultiplexSummary
--
-- /See:/ 'newMultiplexSummary' smart constructor.
data MultiplexSummary = MultiplexSummary'
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
    -- | The current state of the multiplex.
    state :: Prelude.Maybe MultiplexState,
    -- | The name of the multiplex.
    name :: Prelude.Maybe Prelude.Text,
    -- | A collection of key-value pairs.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Configuration for a multiplex event.
    multiplexSettings :: Prelude.Maybe MultiplexSettingsSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { availabilityZones =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      pipelinesRunningCount = Prelude.Nothing,
      programCount = Prelude.Nothing,
      state = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing,
      multiplexSettings = Prelude.Nothing
    }

-- | A list of availability zones for the multiplex.
multiplexSummary_availabilityZones :: Lens.Lens' MultiplexSummary (Prelude.Maybe [Prelude.Text])
multiplexSummary_availabilityZones = Lens.lens (\MultiplexSummary' {availabilityZones} -> availabilityZones) (\s@MultiplexSummary' {} a -> s {availabilityZones = a} :: MultiplexSummary) Prelude.. Lens.mapping Prelude._Coerce

-- | The unique arn of the multiplex.
multiplexSummary_arn :: Lens.Lens' MultiplexSummary (Prelude.Maybe Prelude.Text)
multiplexSummary_arn = Lens.lens (\MultiplexSummary' {arn} -> arn) (\s@MultiplexSummary' {} a -> s {arn = a} :: MultiplexSummary)

-- | The unique id of the multiplex.
multiplexSummary_id :: Lens.Lens' MultiplexSummary (Prelude.Maybe Prelude.Text)
multiplexSummary_id = Lens.lens (\MultiplexSummary' {id} -> id) (\s@MultiplexSummary' {} a -> s {id = a} :: MultiplexSummary)

-- | The number of currently healthy pipelines.
multiplexSummary_pipelinesRunningCount :: Lens.Lens' MultiplexSummary (Prelude.Maybe Prelude.Int)
multiplexSummary_pipelinesRunningCount = Lens.lens (\MultiplexSummary' {pipelinesRunningCount} -> pipelinesRunningCount) (\s@MultiplexSummary' {} a -> s {pipelinesRunningCount = a} :: MultiplexSummary)

-- | The number of programs in the multiplex.
multiplexSummary_programCount :: Lens.Lens' MultiplexSummary (Prelude.Maybe Prelude.Int)
multiplexSummary_programCount = Lens.lens (\MultiplexSummary' {programCount} -> programCount) (\s@MultiplexSummary' {} a -> s {programCount = a} :: MultiplexSummary)

-- | The current state of the multiplex.
multiplexSummary_state :: Lens.Lens' MultiplexSummary (Prelude.Maybe MultiplexState)
multiplexSummary_state = Lens.lens (\MultiplexSummary' {state} -> state) (\s@MultiplexSummary' {} a -> s {state = a} :: MultiplexSummary)

-- | The name of the multiplex.
multiplexSummary_name :: Lens.Lens' MultiplexSummary (Prelude.Maybe Prelude.Text)
multiplexSummary_name = Lens.lens (\MultiplexSummary' {name} -> name) (\s@MultiplexSummary' {} a -> s {name = a} :: MultiplexSummary)

-- | A collection of key-value pairs.
multiplexSummary_tags :: Lens.Lens' MultiplexSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
multiplexSummary_tags = Lens.lens (\MultiplexSummary' {tags} -> tags) (\s@MultiplexSummary' {} a -> s {tags = a} :: MultiplexSummary) Prelude.. Lens.mapping Prelude._Coerce

-- | Configuration for a multiplex event.
multiplexSummary_multiplexSettings :: Lens.Lens' MultiplexSummary (Prelude.Maybe MultiplexSettingsSummary)
multiplexSummary_multiplexSettings = Lens.lens (\MultiplexSummary' {multiplexSettings} -> multiplexSettings) (\s@MultiplexSummary' {} a -> s {multiplexSettings = a} :: MultiplexSummary)

instance Prelude.FromJSON MultiplexSummary where
  parseJSON =
    Prelude.withObject
      "MultiplexSummary"
      ( \x ->
          MultiplexSummary'
            Prelude.<$> ( x Prelude..:? "availabilityZones"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "id")
            Prelude.<*> (x Prelude..:? "pipelinesRunningCount")
            Prelude.<*> (x Prelude..:? "programCount")
            Prelude.<*> (x Prelude..:? "state")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "tags" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "multiplexSettings")
      )

instance Prelude.Hashable MultiplexSummary

instance Prelude.NFData MultiplexSummary
