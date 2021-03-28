{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.FolderConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkMail.Types.FolderConfiguration
  ( FolderConfiguration (..)
  -- * Smart constructor
  , mkFolderConfiguration
  -- * Lenses
  , fcName
  , fcAction
  , fcPeriod
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkMail.Types.FolderName as Types
import qualified Network.AWS.WorkMail.Types.RetentionAction as Types

-- | The configuration applied to an organization's folders by its retention policy.
--
-- /See:/ 'mkFolderConfiguration' smart constructor.
data FolderConfiguration = FolderConfiguration'
  { name :: Types.FolderName
    -- ^ The folder name.
  , action :: Types.RetentionAction
    -- ^ The action to take on the folder contents at the end of the folder configuration period.
  , period :: Core.Maybe Core.Natural
    -- ^ The period of time at which the folder configuration action is applied.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FolderConfiguration' value with any optional fields omitted.
mkFolderConfiguration
    :: Types.FolderName -- ^ 'name'
    -> Types.RetentionAction -- ^ 'action'
    -> FolderConfiguration
mkFolderConfiguration name action
  = FolderConfiguration'{name, action, period = Core.Nothing}

-- | The folder name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcName :: Lens.Lens' FolderConfiguration Types.FolderName
fcName = Lens.field @"name"
{-# INLINEABLE fcName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The action to take on the folder contents at the end of the folder configuration period.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcAction :: Lens.Lens' FolderConfiguration Types.RetentionAction
fcAction = Lens.field @"action"
{-# INLINEABLE fcAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

-- | The period of time at which the folder configuration action is applied.
--
-- /Note:/ Consider using 'period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcPeriod :: Lens.Lens' FolderConfiguration (Core.Maybe Core.Natural)
fcPeriod = Lens.field @"period"
{-# INLINEABLE fcPeriod #-}
{-# DEPRECATED period "Use generic-lens or generic-optics with 'period' instead"  #-}

instance Core.FromJSON FolderConfiguration where
        toJSON FolderConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("Action" Core..= action),
                  ("Period" Core..=) Core.<$> period])

instance Core.FromJSON FolderConfiguration where
        parseJSON
          = Core.withObject "FolderConfiguration" Core.$
              \ x ->
                FolderConfiguration' Core.<$>
                  (x Core..: "Name") Core.<*> x Core..: "Action" Core.<*>
                    x Core..:? "Period"
