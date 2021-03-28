{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchDetachFromIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.BatchDetachFromIndex
  ( BatchDetachFromIndex (..)
  -- * Smart constructor
  , mkBatchDetachFromIndex
  -- * Lenses
  , bdfiIndexReference
  , bdfiTargetReference
  ) where

import qualified Network.AWS.CloudDirectory.Types.ObjectReference as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Detaches the specified object from the specified index inside a 'BatchRead' operation. For more information, see 'DetachFromIndex' and 'BatchReadRequest$Operations' .
--
-- /See:/ 'mkBatchDetachFromIndex' smart constructor.
data BatchDetachFromIndex = BatchDetachFromIndex'
  { indexReference :: Types.ObjectReference
    -- ^ A reference to the index object.
  , targetReference :: Types.ObjectReference
    -- ^ A reference to the object being detached from the index.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDetachFromIndex' value with any optional fields omitted.
mkBatchDetachFromIndex
    :: Types.ObjectReference -- ^ 'indexReference'
    -> Types.ObjectReference -- ^ 'targetReference'
    -> BatchDetachFromIndex
mkBatchDetachFromIndex indexReference targetReference
  = BatchDetachFromIndex'{indexReference, targetReference}

-- | A reference to the index object.
--
-- /Note:/ Consider using 'indexReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdfiIndexReference :: Lens.Lens' BatchDetachFromIndex Types.ObjectReference
bdfiIndexReference = Lens.field @"indexReference"
{-# INLINEABLE bdfiIndexReference #-}
{-# DEPRECATED indexReference "Use generic-lens or generic-optics with 'indexReference' instead"  #-}

-- | A reference to the object being detached from the index.
--
-- /Note:/ Consider using 'targetReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdfiTargetReference :: Lens.Lens' BatchDetachFromIndex Types.ObjectReference
bdfiTargetReference = Lens.field @"targetReference"
{-# INLINEABLE bdfiTargetReference #-}
{-# DEPRECATED targetReference "Use generic-lens or generic-optics with 'targetReference' instead"  #-}

instance Core.FromJSON BatchDetachFromIndex where
        toJSON BatchDetachFromIndex{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("IndexReference" Core..= indexReference),
                  Core.Just ("TargetReference" Core..= targetReference)])
