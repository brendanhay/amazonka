{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.EngineVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MQ.Types.EngineVersion
  ( EngineVersion (..)
  -- * Smart constructor
  , mkEngineVersion
  -- * Lenses
  , evName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Id of the engine version.
--
-- /See:/ 'mkEngineVersion' smart constructor.
newtype EngineVersion = EngineVersion'
  { name :: Core.Maybe Core.Text
    -- ^ Id for the version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EngineVersion' value with any optional fields omitted.
mkEngineVersion
    :: EngineVersion
mkEngineVersion = EngineVersion'{name = Core.Nothing}

-- | Id for the version.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evName :: Lens.Lens' EngineVersion (Core.Maybe Core.Text)
evName = Lens.field @"name"
{-# INLINEABLE evName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON EngineVersion where
        parseJSON
          = Core.withObject "EngineVersion" Core.$
              \ x -> EngineVersion' Core.<$> (x Core..:? "name")
