-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexProgramSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexProgramSummary
  ( MultiplexProgramSummary (..),

    -- * Smart constructor
    mkMultiplexProgramSummary,

    -- * Lenses
    mpsProgramName,
    mpsChannelId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Placeholder documentation for MultiplexProgramSummary
--
-- /See:/ 'mkMultiplexProgramSummary' smart constructor.
data MultiplexProgramSummary = MultiplexProgramSummary'
  { programName ::
      Lude.Maybe Lude.Text,
    channelId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MultiplexProgramSummary' with the minimum fields required to make a request.
--
-- * 'channelId' - The MediaLive Channel associated with the program.
-- * 'programName' - The name of the multiplex program.
mkMultiplexProgramSummary ::
  MultiplexProgramSummary
mkMultiplexProgramSummary =
  MultiplexProgramSummary'
    { programName = Lude.Nothing,
      channelId = Lude.Nothing
    }

-- | The name of the multiplex program.
--
-- /Note:/ Consider using 'programName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsProgramName :: Lens.Lens' MultiplexProgramSummary (Lude.Maybe Lude.Text)
mpsProgramName = Lens.lens (programName :: MultiplexProgramSummary -> Lude.Maybe Lude.Text) (\s a -> s {programName = a} :: MultiplexProgramSummary)
{-# DEPRECATED mpsProgramName "Use generic-lens or generic-optics with 'programName' instead." #-}

-- | The MediaLive Channel associated with the program.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsChannelId :: Lens.Lens' MultiplexProgramSummary (Lude.Maybe Lude.Text)
mpsChannelId = Lens.lens (channelId :: MultiplexProgramSummary -> Lude.Maybe Lude.Text) (\s a -> s {channelId = a} :: MultiplexProgramSummary)
{-# DEPRECATED mpsChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

instance Lude.FromJSON MultiplexProgramSummary where
  parseJSON =
    Lude.withObject
      "MultiplexProgramSummary"
      ( \x ->
          MultiplexProgramSummary'
            Lude.<$> (x Lude..:? "programName") Lude.<*> (x Lude..:? "channelId")
      )
