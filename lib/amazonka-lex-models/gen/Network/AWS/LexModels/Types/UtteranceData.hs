{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.UtteranceData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.UtteranceData
  ( UtteranceData (..),

    -- * Smart constructor
    mkUtteranceData,

    -- * Lenses
    udFirstUtteredDate,
    udCount,
    udUtteranceString,
    udLastUtteredDate,
    udDistinctUsers,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about a single utterance that was made to your bot.
--
-- /See:/ 'mkUtteranceData' smart constructor.
data UtteranceData = UtteranceData'
  { -- | The date that the utterance was first recorded.
    firstUtteredDate :: Lude.Maybe Lude.Timestamp,
    -- | The number of times that the utterance was processed.
    count :: Lude.Maybe Lude.Int,
    -- | The text that was entered by the user or the text representation of an audio clip.
    utteranceString :: Lude.Maybe Lude.Text,
    -- | The date that the utterance was last recorded.
    lastUtteredDate :: Lude.Maybe Lude.Timestamp,
    -- | The total number of individuals that used the utterance.
    distinctUsers :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UtteranceData' with the minimum fields required to make a request.
--
-- * 'firstUtteredDate' - The date that the utterance was first recorded.
-- * 'count' - The number of times that the utterance was processed.
-- * 'utteranceString' - The text that was entered by the user or the text representation of an audio clip.
-- * 'lastUtteredDate' - The date that the utterance was last recorded.
-- * 'distinctUsers' - The total number of individuals that used the utterance.
mkUtteranceData ::
  UtteranceData
mkUtteranceData =
  UtteranceData'
    { firstUtteredDate = Lude.Nothing,
      count = Lude.Nothing,
      utteranceString = Lude.Nothing,
      lastUtteredDate = Lude.Nothing,
      distinctUsers = Lude.Nothing
    }

-- | The date that the utterance was first recorded.
--
-- /Note:/ Consider using 'firstUtteredDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udFirstUtteredDate :: Lens.Lens' UtteranceData (Lude.Maybe Lude.Timestamp)
udFirstUtteredDate = Lens.lens (firstUtteredDate :: UtteranceData -> Lude.Maybe Lude.Timestamp) (\s a -> s {firstUtteredDate = a} :: UtteranceData)
{-# DEPRECATED udFirstUtteredDate "Use generic-lens or generic-optics with 'firstUtteredDate' instead." #-}

-- | The number of times that the utterance was processed.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udCount :: Lens.Lens' UtteranceData (Lude.Maybe Lude.Int)
udCount = Lens.lens (count :: UtteranceData -> Lude.Maybe Lude.Int) (\s a -> s {count = a} :: UtteranceData)
{-# DEPRECATED udCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | The text that was entered by the user or the text representation of an audio clip.
--
-- /Note:/ Consider using 'utteranceString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udUtteranceString :: Lens.Lens' UtteranceData (Lude.Maybe Lude.Text)
udUtteranceString = Lens.lens (utteranceString :: UtteranceData -> Lude.Maybe Lude.Text) (\s a -> s {utteranceString = a} :: UtteranceData)
{-# DEPRECATED udUtteranceString "Use generic-lens or generic-optics with 'utteranceString' instead." #-}

-- | The date that the utterance was last recorded.
--
-- /Note:/ Consider using 'lastUtteredDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udLastUtteredDate :: Lens.Lens' UtteranceData (Lude.Maybe Lude.Timestamp)
udLastUtteredDate = Lens.lens (lastUtteredDate :: UtteranceData -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUtteredDate = a} :: UtteranceData)
{-# DEPRECATED udLastUtteredDate "Use generic-lens or generic-optics with 'lastUtteredDate' instead." #-}

-- | The total number of individuals that used the utterance.
--
-- /Note:/ Consider using 'distinctUsers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDistinctUsers :: Lens.Lens' UtteranceData (Lude.Maybe Lude.Int)
udDistinctUsers = Lens.lens (distinctUsers :: UtteranceData -> Lude.Maybe Lude.Int) (\s a -> s {distinctUsers = a} :: UtteranceData)
{-# DEPRECATED udDistinctUsers "Use generic-lens or generic-optics with 'distinctUsers' instead." #-}

instance Lude.FromJSON UtteranceData where
  parseJSON =
    Lude.withObject
      "UtteranceData"
      ( \x ->
          UtteranceData'
            Lude.<$> (x Lude..:? "firstUtteredDate")
            Lude.<*> (x Lude..:? "count")
            Lude.<*> (x Lude..:? "utteranceString")
            Lude.<*> (x Lude..:? "lastUtteredDate")
            Lude.<*> (x Lude..:? "distinctUsers")
      )
