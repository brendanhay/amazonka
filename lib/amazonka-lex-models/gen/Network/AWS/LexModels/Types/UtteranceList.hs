-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.UtteranceList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.UtteranceList
  ( UtteranceList (..),

    -- * Smart constructor
    mkUtteranceList,

    -- * Lenses
    ulBotVersion,
    ulUtterances,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types.UtteranceData
import qualified Network.AWS.Prelude as Lude

-- | Provides a list of utterances that have been made to a specific version of your bot. The list contains a maximum of 100 utterances.
--
-- /See:/ 'mkUtteranceList' smart constructor.
data UtteranceList = UtteranceList'
  { botVersion ::
      Lude.Maybe Lude.Text,
    utterances :: Lude.Maybe [UtteranceData]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UtteranceList' with the minimum fields required to make a request.
--
-- * 'botVersion' - The version of the bot that processed the list.
-- * 'utterances' - One or more 'UtteranceData' objects that contain information about the utterances that have been made to a bot. The maximum number of object is 100.
mkUtteranceList ::
  UtteranceList
mkUtteranceList =
  UtteranceList'
    { botVersion = Lude.Nothing,
      utterances = Lude.Nothing
    }

-- | The version of the bot that processed the list.
--
-- /Note:/ Consider using 'botVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulBotVersion :: Lens.Lens' UtteranceList (Lude.Maybe Lude.Text)
ulBotVersion = Lens.lens (botVersion :: UtteranceList -> Lude.Maybe Lude.Text) (\s a -> s {botVersion = a} :: UtteranceList)
{-# DEPRECATED ulBotVersion "Use generic-lens or generic-optics with 'botVersion' instead." #-}

-- | One or more 'UtteranceData' objects that contain information about the utterances that have been made to a bot. The maximum number of object is 100.
--
-- /Note:/ Consider using 'utterances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulUtterances :: Lens.Lens' UtteranceList (Lude.Maybe [UtteranceData])
ulUtterances = Lens.lens (utterances :: UtteranceList -> Lude.Maybe [UtteranceData]) (\s a -> s {utterances = a} :: UtteranceList)
{-# DEPRECATED ulUtterances "Use generic-lens or generic-optics with 'utterances' instead." #-}

instance Lude.FromJSON UtteranceList where
  parseJSON =
    Lude.withObject
      "UtteranceList"
      ( \x ->
          UtteranceList'
            Lude.<$> (x Lude..:? "botVersion")
            Lude.<*> (x Lude..:? "utterances" Lude..!= Lude.mempty)
      )
