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
-- Module      : Amazonka.MediaLive.Types.MultiplexProgram
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.MultiplexProgram where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.MultiplexProgramPacketIdentifiersMap
import Amazonka.MediaLive.Types.MultiplexProgramPipelineDetail
import Amazonka.MediaLive.Types.MultiplexProgramSettings
import qualified Amazonka.Prelude as Prelude

-- | The multiplex program object.
--
-- /See:/ 'newMultiplexProgram' smart constructor.
data MultiplexProgram = MultiplexProgram'
  { -- | The name of the multiplex program.
    programName :: Prelude.Maybe Prelude.Text,
    -- | The packet identifier map for this multiplex program.
    packetIdentifiersMap :: Prelude.Maybe MultiplexProgramPacketIdentifiersMap,
    -- | Contains information about the current sources for the specified program
    -- in the specified multiplex. Keep in mind that each multiplex pipeline
    -- connects to both pipelines in a given source channel (the channel
    -- identified by the program). But only one of those channel pipelines is
    -- ever active at one time.
    pipelineDetails :: Prelude.Maybe [MultiplexProgramPipelineDetail],
    -- | The settings for this multiplex program.
    multiplexProgramSettings :: Prelude.Maybe MultiplexProgramSettings,
    -- | The MediaLive channel associated with the program.
    channelId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MultiplexProgram' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'programName', 'multiplexProgram_programName' - The name of the multiplex program.
--
-- 'packetIdentifiersMap', 'multiplexProgram_packetIdentifiersMap' - The packet identifier map for this multiplex program.
--
-- 'pipelineDetails', 'multiplexProgram_pipelineDetails' - Contains information about the current sources for the specified program
-- in the specified multiplex. Keep in mind that each multiplex pipeline
-- connects to both pipelines in a given source channel (the channel
-- identified by the program). But only one of those channel pipelines is
-- ever active at one time.
--
-- 'multiplexProgramSettings', 'multiplexProgram_multiplexProgramSettings' - The settings for this multiplex program.
--
-- 'channelId', 'multiplexProgram_channelId' - The MediaLive channel associated with the program.
newMultiplexProgram ::
  MultiplexProgram
newMultiplexProgram =
  MultiplexProgram'
    { programName = Prelude.Nothing,
      packetIdentifiersMap = Prelude.Nothing,
      pipelineDetails = Prelude.Nothing,
      multiplexProgramSettings = Prelude.Nothing,
      channelId = Prelude.Nothing
    }

-- | The name of the multiplex program.
multiplexProgram_programName :: Lens.Lens' MultiplexProgram (Prelude.Maybe Prelude.Text)
multiplexProgram_programName = Lens.lens (\MultiplexProgram' {programName} -> programName) (\s@MultiplexProgram' {} a -> s {programName = a} :: MultiplexProgram)

-- | The packet identifier map for this multiplex program.
multiplexProgram_packetIdentifiersMap :: Lens.Lens' MultiplexProgram (Prelude.Maybe MultiplexProgramPacketIdentifiersMap)
multiplexProgram_packetIdentifiersMap = Lens.lens (\MultiplexProgram' {packetIdentifiersMap} -> packetIdentifiersMap) (\s@MultiplexProgram' {} a -> s {packetIdentifiersMap = a} :: MultiplexProgram)

-- | Contains information about the current sources for the specified program
-- in the specified multiplex. Keep in mind that each multiplex pipeline
-- connects to both pipelines in a given source channel (the channel
-- identified by the program). But only one of those channel pipelines is
-- ever active at one time.
multiplexProgram_pipelineDetails :: Lens.Lens' MultiplexProgram (Prelude.Maybe [MultiplexProgramPipelineDetail])
multiplexProgram_pipelineDetails = Lens.lens (\MultiplexProgram' {pipelineDetails} -> pipelineDetails) (\s@MultiplexProgram' {} a -> s {pipelineDetails = a} :: MultiplexProgram) Prelude.. Lens.mapping Lens.coerced

-- | The settings for this multiplex program.
multiplexProgram_multiplexProgramSettings :: Lens.Lens' MultiplexProgram (Prelude.Maybe MultiplexProgramSettings)
multiplexProgram_multiplexProgramSettings = Lens.lens (\MultiplexProgram' {multiplexProgramSettings} -> multiplexProgramSettings) (\s@MultiplexProgram' {} a -> s {multiplexProgramSettings = a} :: MultiplexProgram)

-- | The MediaLive channel associated with the program.
multiplexProgram_channelId :: Lens.Lens' MultiplexProgram (Prelude.Maybe Prelude.Text)
multiplexProgram_channelId = Lens.lens (\MultiplexProgram' {channelId} -> channelId) (\s@MultiplexProgram' {} a -> s {channelId = a} :: MultiplexProgram)

instance Data.FromJSON MultiplexProgram where
  parseJSON =
    Data.withObject
      "MultiplexProgram"
      ( \x ->
          MultiplexProgram'
            Prelude.<$> (x Data..:? "programName")
            Prelude.<*> (x Data..:? "packetIdentifiersMap")
            Prelude.<*> ( x Data..:? "pipelineDetails"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "multiplexProgramSettings")
            Prelude.<*> (x Data..:? "channelId")
      )

instance Prelude.Hashable MultiplexProgram where
  hashWithSalt _salt MultiplexProgram' {..} =
    _salt `Prelude.hashWithSalt` programName
      `Prelude.hashWithSalt` packetIdentifiersMap
      `Prelude.hashWithSalt` pipelineDetails
      `Prelude.hashWithSalt` multiplexProgramSettings
      `Prelude.hashWithSalt` channelId

instance Prelude.NFData MultiplexProgram where
  rnf MultiplexProgram' {..} =
    Prelude.rnf programName
      `Prelude.seq` Prelude.rnf packetIdentifiersMap
      `Prelude.seq` Prelude.rnf pipelineDetails
      `Prelude.seq` Prelude.rnf multiplexProgramSettings
      `Prelude.seq` Prelude.rnf channelId
