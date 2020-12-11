-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexProgram
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexProgram
  ( MultiplexProgram (..),

    -- * Smart constructor
    mkMultiplexProgram,

    -- * Lenses
    mpPacketIdentifiersMap,
    mpPipelineDetails,
    mpProgramName,
    mpChannelId,
    mpMultiplexProgramSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.MultiplexProgramPacketIdentifiersMap
import Network.AWS.MediaLive.Types.MultiplexProgramPipelineDetail
import Network.AWS.MediaLive.Types.MultiplexProgramSettings
import qualified Network.AWS.Prelude as Lude

-- | The multiplex program object.
--
-- /See:/ 'mkMultiplexProgram' smart constructor.
data MultiplexProgram = MultiplexProgram'
  { packetIdentifiersMap ::
      Lude.Maybe MultiplexProgramPacketIdentifiersMap,
    pipelineDetails ::
      Lude.Maybe [MultiplexProgramPipelineDetail],
    programName :: Lude.Maybe Lude.Text,
    channelId :: Lude.Maybe Lude.Text,
    multiplexProgramSettings ::
      Lude.Maybe MultiplexProgramSettings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MultiplexProgram' with the minimum fields required to make a request.
--
-- * 'channelId' - The MediaLive channel associated with the program.
-- * 'multiplexProgramSettings' - The settings for this multiplex program.
-- * 'packetIdentifiersMap' - The packet identifier map for this multiplex program.
-- * 'pipelineDetails' - Contains information about the current sources for the specified program in the specified multiplex. Keep in mind that each multiplex pipeline connects to both pipelines in a given source channel (the channel identified by the program). But only one of those channel pipelines is ever active at one time.
-- * 'programName' - The name of the multiplex program.
mkMultiplexProgram ::
  MultiplexProgram
mkMultiplexProgram =
  MultiplexProgram'
    { packetIdentifiersMap = Lude.Nothing,
      pipelineDetails = Lude.Nothing,
      programName = Lude.Nothing,
      channelId = Lude.Nothing,
      multiplexProgramSettings = Lude.Nothing
    }

-- | The packet identifier map for this multiplex program.
--
-- /Note:/ Consider using 'packetIdentifiersMap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpPacketIdentifiersMap :: Lens.Lens' MultiplexProgram (Lude.Maybe MultiplexProgramPacketIdentifiersMap)
mpPacketIdentifiersMap = Lens.lens (packetIdentifiersMap :: MultiplexProgram -> Lude.Maybe MultiplexProgramPacketIdentifiersMap) (\s a -> s {packetIdentifiersMap = a} :: MultiplexProgram)
{-# DEPRECATED mpPacketIdentifiersMap "Use generic-lens or generic-optics with 'packetIdentifiersMap' instead." #-}

-- | Contains information about the current sources for the specified program in the specified multiplex. Keep in mind that each multiplex pipeline connects to both pipelines in a given source channel (the channel identified by the program). But only one of those channel pipelines is ever active at one time.
--
-- /Note:/ Consider using 'pipelineDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpPipelineDetails :: Lens.Lens' MultiplexProgram (Lude.Maybe [MultiplexProgramPipelineDetail])
mpPipelineDetails = Lens.lens (pipelineDetails :: MultiplexProgram -> Lude.Maybe [MultiplexProgramPipelineDetail]) (\s a -> s {pipelineDetails = a} :: MultiplexProgram)
{-# DEPRECATED mpPipelineDetails "Use generic-lens or generic-optics with 'pipelineDetails' instead." #-}

-- | The name of the multiplex program.
--
-- /Note:/ Consider using 'programName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpProgramName :: Lens.Lens' MultiplexProgram (Lude.Maybe Lude.Text)
mpProgramName = Lens.lens (programName :: MultiplexProgram -> Lude.Maybe Lude.Text) (\s a -> s {programName = a} :: MultiplexProgram)
{-# DEPRECATED mpProgramName "Use generic-lens or generic-optics with 'programName' instead." #-}

-- | The MediaLive channel associated with the program.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpChannelId :: Lens.Lens' MultiplexProgram (Lude.Maybe Lude.Text)
mpChannelId = Lens.lens (channelId :: MultiplexProgram -> Lude.Maybe Lude.Text) (\s a -> s {channelId = a} :: MultiplexProgram)
{-# DEPRECATED mpChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

-- | The settings for this multiplex program.
--
-- /Note:/ Consider using 'multiplexProgramSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpMultiplexProgramSettings :: Lens.Lens' MultiplexProgram (Lude.Maybe MultiplexProgramSettings)
mpMultiplexProgramSettings = Lens.lens (multiplexProgramSettings :: MultiplexProgram -> Lude.Maybe MultiplexProgramSettings) (\s a -> s {multiplexProgramSettings = a} :: MultiplexProgram)
{-# DEPRECATED mpMultiplexProgramSettings "Use generic-lens or generic-optics with 'multiplexProgramSettings' instead." #-}

instance Lude.FromJSON MultiplexProgram where
  parseJSON =
    Lude.withObject
      "MultiplexProgram"
      ( \x ->
          MultiplexProgram'
            Lude.<$> (x Lude..:? "packetIdentifiersMap")
            Lude.<*> (x Lude..:? "pipelineDetails" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "programName")
            Lude.<*> (x Lude..:? "channelId")
            Lude.<*> (x Lude..:? "multiplexProgramSettings")
      )
