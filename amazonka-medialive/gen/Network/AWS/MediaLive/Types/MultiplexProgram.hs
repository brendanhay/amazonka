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
-- Module      : Network.AWS.MediaLive.Types.MultiplexProgram
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexProgram where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.MultiplexProgramPacketIdentifiersMap
import Network.AWS.MediaLive.Types.MultiplexProgramPipelineDetail
import Network.AWS.MediaLive.Types.MultiplexProgramSettings
import qualified Network.AWS.Prelude as Prelude

-- | The multiplex program object.
--
-- /See:/ 'newMultiplexProgram' smart constructor.
data MultiplexProgram = MultiplexProgram'
  { -- | The packet identifier map for this multiplex program.
    packetIdentifiersMap :: Prelude.Maybe MultiplexProgramPacketIdentifiersMap,
    -- | The settings for this multiplex program.
    multiplexProgramSettings :: Prelude.Maybe MultiplexProgramSettings,
    -- | The MediaLive channel associated with the program.
    channelId :: Prelude.Maybe Prelude.Text,
    -- | The name of the multiplex program.
    programName :: Prelude.Maybe Prelude.Text,
    -- | Contains information about the current sources for the specified program
    -- in the specified multiplex. Keep in mind that each multiplex pipeline
    -- connects to both pipelines in a given source channel (the channel
    -- identified by the program). But only one of those channel pipelines is
    -- ever active at one time.
    pipelineDetails :: Prelude.Maybe [MultiplexProgramPipelineDetail]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MultiplexProgram' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'packetIdentifiersMap', 'multiplexProgram_packetIdentifiersMap' - The packet identifier map for this multiplex program.
--
-- 'multiplexProgramSettings', 'multiplexProgram_multiplexProgramSettings' - The settings for this multiplex program.
--
-- 'channelId', 'multiplexProgram_channelId' - The MediaLive channel associated with the program.
--
-- 'programName', 'multiplexProgram_programName' - The name of the multiplex program.
--
-- 'pipelineDetails', 'multiplexProgram_pipelineDetails' - Contains information about the current sources for the specified program
-- in the specified multiplex. Keep in mind that each multiplex pipeline
-- connects to both pipelines in a given source channel (the channel
-- identified by the program). But only one of those channel pipelines is
-- ever active at one time.
newMultiplexProgram ::
  MultiplexProgram
newMultiplexProgram =
  MultiplexProgram'
    { packetIdentifiersMap =
        Prelude.Nothing,
      multiplexProgramSettings = Prelude.Nothing,
      channelId = Prelude.Nothing,
      programName = Prelude.Nothing,
      pipelineDetails = Prelude.Nothing
    }

-- | The packet identifier map for this multiplex program.
multiplexProgram_packetIdentifiersMap :: Lens.Lens' MultiplexProgram (Prelude.Maybe MultiplexProgramPacketIdentifiersMap)
multiplexProgram_packetIdentifiersMap = Lens.lens (\MultiplexProgram' {packetIdentifiersMap} -> packetIdentifiersMap) (\s@MultiplexProgram' {} a -> s {packetIdentifiersMap = a} :: MultiplexProgram)

-- | The settings for this multiplex program.
multiplexProgram_multiplexProgramSettings :: Lens.Lens' MultiplexProgram (Prelude.Maybe MultiplexProgramSettings)
multiplexProgram_multiplexProgramSettings = Lens.lens (\MultiplexProgram' {multiplexProgramSettings} -> multiplexProgramSettings) (\s@MultiplexProgram' {} a -> s {multiplexProgramSettings = a} :: MultiplexProgram)

-- | The MediaLive channel associated with the program.
multiplexProgram_channelId :: Lens.Lens' MultiplexProgram (Prelude.Maybe Prelude.Text)
multiplexProgram_channelId = Lens.lens (\MultiplexProgram' {channelId} -> channelId) (\s@MultiplexProgram' {} a -> s {channelId = a} :: MultiplexProgram)

-- | The name of the multiplex program.
multiplexProgram_programName :: Lens.Lens' MultiplexProgram (Prelude.Maybe Prelude.Text)
multiplexProgram_programName = Lens.lens (\MultiplexProgram' {programName} -> programName) (\s@MultiplexProgram' {} a -> s {programName = a} :: MultiplexProgram)

-- | Contains information about the current sources for the specified program
-- in the specified multiplex. Keep in mind that each multiplex pipeline
-- connects to both pipelines in a given source channel (the channel
-- identified by the program). But only one of those channel pipelines is
-- ever active at one time.
multiplexProgram_pipelineDetails :: Lens.Lens' MultiplexProgram (Prelude.Maybe [MultiplexProgramPipelineDetail])
multiplexProgram_pipelineDetails = Lens.lens (\MultiplexProgram' {pipelineDetails} -> pipelineDetails) (\s@MultiplexProgram' {} a -> s {pipelineDetails = a} :: MultiplexProgram) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON MultiplexProgram where
  parseJSON =
    Prelude.withObject
      "MultiplexProgram"
      ( \x ->
          MultiplexProgram'
            Prelude.<$> (x Prelude..:? "packetIdentifiersMap")
            Prelude.<*> (x Prelude..:? "multiplexProgramSettings")
            Prelude.<*> (x Prelude..:? "channelId")
            Prelude.<*> (x Prelude..:? "programName")
            Prelude.<*> ( x Prelude..:? "pipelineDetails"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable MultiplexProgram

instance Prelude.NFData MultiplexProgram
