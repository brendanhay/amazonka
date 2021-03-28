{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.FaultRootCauseEntity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.XRay.Types.FaultRootCauseEntity
  ( FaultRootCauseEntity (..)
  -- * Smart constructor
  , mkFaultRootCauseEntity
  -- * Lenses
  , frceExceptions
  , frceName
  , frceRemote
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.RootCauseException as Types

-- | A collection of segments and corresponding subsegments associated to a trace summary fault error.
--
-- /See:/ 'mkFaultRootCauseEntity' smart constructor.
data FaultRootCauseEntity = FaultRootCauseEntity'
  { exceptions :: Core.Maybe [Types.RootCauseException]
    -- ^ The types and messages of the exceptions.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the entity.
  , remote :: Core.Maybe Core.Bool
    -- ^ A flag that denotes a remote subsegment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FaultRootCauseEntity' value with any optional fields omitted.
mkFaultRootCauseEntity
    :: FaultRootCauseEntity
mkFaultRootCauseEntity
  = FaultRootCauseEntity'{exceptions = Core.Nothing,
                          name = Core.Nothing, remote = Core.Nothing}

-- | The types and messages of the exceptions.
--
-- /Note:/ Consider using 'exceptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frceExceptions :: Lens.Lens' FaultRootCauseEntity (Core.Maybe [Types.RootCauseException])
frceExceptions = Lens.field @"exceptions"
{-# INLINEABLE frceExceptions #-}
{-# DEPRECATED exceptions "Use generic-lens or generic-optics with 'exceptions' instead"  #-}

-- | The name of the entity.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frceName :: Lens.Lens' FaultRootCauseEntity (Core.Maybe Core.Text)
frceName = Lens.field @"name"
{-# INLINEABLE frceName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A flag that denotes a remote subsegment.
--
-- /Note:/ Consider using 'remote' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frceRemote :: Lens.Lens' FaultRootCauseEntity (Core.Maybe Core.Bool)
frceRemote = Lens.field @"remote"
{-# INLINEABLE frceRemote #-}
{-# DEPRECATED remote "Use generic-lens or generic-optics with 'remote' instead"  #-}

instance Core.FromJSON FaultRootCauseEntity where
        parseJSON
          = Core.withObject "FaultRootCauseEntity" Core.$
              \ x ->
                FaultRootCauseEntity' Core.<$>
                  (x Core..:? "Exceptions") Core.<*> x Core..:? "Name" Core.<*>
                    x Core..:? "Remote"
