{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.OutputGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.OutputGroup
  ( OutputGroup (..)
  -- * Smart constructor
  , mkOutputGroup
  -- * Lenses
  , ogOutputs
  , ogOutputGroupSettings
  , ogName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.Output as Types
import qualified Network.AWS.MediaLive.Types.OutputGroupSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Output groups for this Live Event. Output groups contain information about where streams should be distributed.
--
-- /See:/ 'mkOutputGroup' smart constructor.
data OutputGroup = OutputGroup'
  { outputs :: [Types.Output]
  , outputGroupSettings :: Types.OutputGroupSettings
    -- ^ Settings associated with the output group.
  , name :: Core.Maybe Core.Text
    -- ^ Custom output group name optionally defined by the user.  Only letters, numbers, and the underscore character allowed; only 32 characters allowed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OutputGroup' value with any optional fields omitted.
mkOutputGroup
    :: Types.OutputGroupSettings -- ^ 'outputGroupSettings'
    -> OutputGroup
mkOutputGroup outputGroupSettings
  = OutputGroup'{outputs = Core.mempty, outputGroupSettings,
                 name = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'outputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogOutputs :: Lens.Lens' OutputGroup [Types.Output]
ogOutputs = Lens.field @"outputs"
{-# INLINEABLE ogOutputs #-}
{-# DEPRECATED outputs "Use generic-lens or generic-optics with 'outputs' instead"  #-}

-- | Settings associated with the output group.
--
-- /Note:/ Consider using 'outputGroupSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogOutputGroupSettings :: Lens.Lens' OutputGroup Types.OutputGroupSettings
ogOutputGroupSettings = Lens.field @"outputGroupSettings"
{-# INLINEABLE ogOutputGroupSettings #-}
{-# DEPRECATED outputGroupSettings "Use generic-lens or generic-optics with 'outputGroupSettings' instead"  #-}

-- | Custom output group name optionally defined by the user.  Only letters, numbers, and the underscore character allowed; only 32 characters allowed.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogName :: Lens.Lens' OutputGroup (Core.Maybe Core.Text)
ogName = Lens.field @"name"
{-# INLINEABLE ogName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON OutputGroup where
        toJSON OutputGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("outputs" Core..= outputs),
                  Core.Just ("outputGroupSettings" Core..= outputGroupSettings),
                  ("name" Core..=) Core.<$> name])

instance Core.FromJSON OutputGroup where
        parseJSON
          = Core.withObject "OutputGroup" Core.$
              \ x ->
                OutputGroup' Core.<$>
                  (x Core..:? "outputs" Core..!= Core.mempty) Core.<*>
                    x Core..: "outputGroupSettings"
                    Core.<*> x Core..:? "name"
