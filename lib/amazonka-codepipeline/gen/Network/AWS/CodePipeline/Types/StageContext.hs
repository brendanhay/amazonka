{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.StageContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.StageContext
  ( StageContext (..)
  -- * Smart constructor
  , mkStageContext
  -- * Lenses
  , scName
  ) where

import qualified Network.AWS.CodePipeline.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents information about a stage to a job worker.
--
-- /See:/ 'mkStageContext' smart constructor.
newtype StageContext = StageContext'
  { name :: Core.Maybe Types.Name
    -- ^ The name of the stage.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StageContext' value with any optional fields omitted.
mkStageContext
    :: StageContext
mkStageContext = StageContext'{name = Core.Nothing}

-- | The name of the stage.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scName :: Lens.Lens' StageContext (Core.Maybe Types.Name)
scName = Lens.field @"name"
{-# INLINEABLE scName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON StageContext where
        parseJSON
          = Core.withObject "StageContext" Core.$
              \ x -> StageContext' Core.<$> (x Core..:? "name")
