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
-- Module      : Network.AWS.MediaLive.Types.MultiplexProgramSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexProgramSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Placeholder documentation for MultiplexProgramSummary
--
-- /See:/ 'newMultiplexProgramSummary' smart constructor.
data MultiplexProgramSummary = MultiplexProgramSummary'
  { -- | The MediaLive Channel associated with the program.
    channelId :: Prelude.Maybe Prelude.Text,
    -- | The name of the multiplex program.
    programName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MultiplexProgramSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelId', 'multiplexProgramSummary_channelId' - The MediaLive Channel associated with the program.
--
-- 'programName', 'multiplexProgramSummary_programName' - The name of the multiplex program.
newMultiplexProgramSummary ::
  MultiplexProgramSummary
newMultiplexProgramSummary =
  MultiplexProgramSummary'
    { channelId =
        Prelude.Nothing,
      programName = Prelude.Nothing
    }

-- | The MediaLive Channel associated with the program.
multiplexProgramSummary_channelId :: Lens.Lens' MultiplexProgramSummary (Prelude.Maybe Prelude.Text)
multiplexProgramSummary_channelId = Lens.lens (\MultiplexProgramSummary' {channelId} -> channelId) (\s@MultiplexProgramSummary' {} a -> s {channelId = a} :: MultiplexProgramSummary)

-- | The name of the multiplex program.
multiplexProgramSummary_programName :: Lens.Lens' MultiplexProgramSummary (Prelude.Maybe Prelude.Text)
multiplexProgramSummary_programName = Lens.lens (\MultiplexProgramSummary' {programName} -> programName) (\s@MultiplexProgramSummary' {} a -> s {programName = a} :: MultiplexProgramSummary)

instance Prelude.FromJSON MultiplexProgramSummary where
  parseJSON =
    Prelude.withObject
      "MultiplexProgramSummary"
      ( \x ->
          MultiplexProgramSummary'
            Prelude.<$> (x Prelude..:? "channelId")
            Prelude.<*> (x Prelude..:? "programName")
      )

instance Prelude.Hashable MultiplexProgramSummary

instance Prelude.NFData MultiplexProgramSummary
