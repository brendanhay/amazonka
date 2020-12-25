{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
import qualified Network.AWS.LexModels.Types.UtteranceData as Types
import qualified Network.AWS.LexModels.Types.Version as Types
import qualified Network.AWS.Prelude as Core

-- | Provides a list of utterances that have been made to a specific version of your bot. The list contains a maximum of 100 utterances.
--
-- /See:/ 'mkUtteranceList' smart constructor.
data UtteranceList = UtteranceList'
  { -- | The version of the bot that processed the list.
    botVersion :: Core.Maybe Types.Version,
    -- | One or more 'UtteranceData' objects that contain information about the utterances that have been made to a bot. The maximum number of object is 100.
    utterances :: Core.Maybe [Types.UtteranceData]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UtteranceList' value with any optional fields omitted.
mkUtteranceList ::
  UtteranceList
mkUtteranceList =
  UtteranceList'
    { botVersion = Core.Nothing,
      utterances = Core.Nothing
    }

-- | The version of the bot that processed the list.
--
-- /Note:/ Consider using 'botVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulBotVersion :: Lens.Lens' UtteranceList (Core.Maybe Types.Version)
ulBotVersion = Lens.field @"botVersion"
{-# DEPRECATED ulBotVersion "Use generic-lens or generic-optics with 'botVersion' instead." #-}

-- | One or more 'UtteranceData' objects that contain information about the utterances that have been made to a bot. The maximum number of object is 100.
--
-- /Note:/ Consider using 'utterances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ulUtterances :: Lens.Lens' UtteranceList (Core.Maybe [Types.UtteranceData])
ulUtterances = Lens.field @"utterances"
{-# DEPRECATED ulUtterances "Use generic-lens or generic-optics with 'utterances' instead." #-}

instance Core.FromJSON UtteranceList where
  parseJSON =
    Core.withObject "UtteranceList" Core.$
      \x ->
        UtteranceList'
          Core.<$> (x Core..:? "botVersion") Core.<*> (x Core..:? "utterances")
