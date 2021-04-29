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
-- Module      : Network.AWS.MediaLive.Types.MultiplexProgramChannelDestinationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexProgramChannelDestinationSettings where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Multiplex Program Input Destination Settings for outputting a Channel to
-- a Multiplex
--
-- /See:/ 'newMultiplexProgramChannelDestinationSettings' smart constructor.
data MultiplexProgramChannelDestinationSettings = MultiplexProgramChannelDestinationSettings'
  { -- | The ID of the Multiplex that the encoder is providing output to. You do
    -- not need to specify the individual inputs to the Multiplex; MediaLive
    -- will handle the connection of the two MediaLive pipelines to the two
    -- Multiplex instances. The Multiplex must be in the same region as the
    -- Channel.
    multiplexId :: Prelude.Maybe Prelude.Text,
    -- | The program name of the Multiplex program that the encoder is providing
    -- output to.
    programName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MultiplexProgramChannelDestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'multiplexId', 'multiplexProgramChannelDestinationSettings_multiplexId' - The ID of the Multiplex that the encoder is providing output to. You do
-- not need to specify the individual inputs to the Multiplex; MediaLive
-- will handle the connection of the two MediaLive pipelines to the two
-- Multiplex instances. The Multiplex must be in the same region as the
-- Channel.
--
-- 'programName', 'multiplexProgramChannelDestinationSettings_programName' - The program name of the Multiplex program that the encoder is providing
-- output to.
newMultiplexProgramChannelDestinationSettings ::
  MultiplexProgramChannelDestinationSettings
newMultiplexProgramChannelDestinationSettings =
  MultiplexProgramChannelDestinationSettings'
    { multiplexId =
        Prelude.Nothing,
      programName = Prelude.Nothing
    }

-- | The ID of the Multiplex that the encoder is providing output to. You do
-- not need to specify the individual inputs to the Multiplex; MediaLive
-- will handle the connection of the two MediaLive pipelines to the two
-- Multiplex instances. The Multiplex must be in the same region as the
-- Channel.
multiplexProgramChannelDestinationSettings_multiplexId :: Lens.Lens' MultiplexProgramChannelDestinationSettings (Prelude.Maybe Prelude.Text)
multiplexProgramChannelDestinationSettings_multiplexId = Lens.lens (\MultiplexProgramChannelDestinationSettings' {multiplexId} -> multiplexId) (\s@MultiplexProgramChannelDestinationSettings' {} a -> s {multiplexId = a} :: MultiplexProgramChannelDestinationSettings)

-- | The program name of the Multiplex program that the encoder is providing
-- output to.
multiplexProgramChannelDestinationSettings_programName :: Lens.Lens' MultiplexProgramChannelDestinationSettings (Prelude.Maybe Prelude.Text)
multiplexProgramChannelDestinationSettings_programName = Lens.lens (\MultiplexProgramChannelDestinationSettings' {programName} -> programName) (\s@MultiplexProgramChannelDestinationSettings' {} a -> s {programName = a} :: MultiplexProgramChannelDestinationSettings)

instance
  Prelude.FromJSON
    MultiplexProgramChannelDestinationSettings
  where
  parseJSON =
    Prelude.withObject
      "MultiplexProgramChannelDestinationSettings"
      ( \x ->
          MultiplexProgramChannelDestinationSettings'
            Prelude.<$> (x Prelude..:? "multiplexId")
              Prelude.<*> (x Prelude..:? "programName")
      )

instance
  Prelude.Hashable
    MultiplexProgramChannelDestinationSettings

instance
  Prelude.NFData
    MultiplexProgramChannelDestinationSettings

instance
  Prelude.ToJSON
    MultiplexProgramChannelDestinationSettings
  where
  toJSON
    MultiplexProgramChannelDestinationSettings' {..} =
      Prelude.object
        ( Prelude.catMaybes
            [ ("multiplexId" Prelude..=) Prelude.<$> multiplexId,
              ("programName" Prelude..=) Prelude.<$> programName
            ]
        )
