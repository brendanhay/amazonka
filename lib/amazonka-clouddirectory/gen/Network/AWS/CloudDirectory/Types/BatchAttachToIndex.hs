{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchAttachToIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.BatchAttachToIndex
  ( BatchAttachToIndex (..)
  -- * Smart constructor
  , mkBatchAttachToIndex
  -- * Lenses
  , batiIndexReference
  , batiTargetReference
  ) where

import qualified Network.AWS.CloudDirectory.Types.ObjectReference as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Attaches the specified object to the specified index inside a 'BatchRead' operation. For more information, see 'AttachToIndex' and 'BatchReadRequest$Operations' .
--
-- /See:/ 'mkBatchAttachToIndex' smart constructor.
data BatchAttachToIndex = BatchAttachToIndex'
  { indexReference :: Types.ObjectReference
    -- ^ A reference to the index that you are attaching the object to.
  , targetReference :: Types.ObjectReference
    -- ^ A reference to the object that you are attaching to the index.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchAttachToIndex' value with any optional fields omitted.
mkBatchAttachToIndex
    :: Types.ObjectReference -- ^ 'indexReference'
    -> Types.ObjectReference -- ^ 'targetReference'
    -> BatchAttachToIndex
mkBatchAttachToIndex indexReference targetReference
  = BatchAttachToIndex'{indexReference, targetReference}

-- | A reference to the index that you are attaching the object to.
--
-- /Note:/ Consider using 'indexReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
batiIndexReference :: Lens.Lens' BatchAttachToIndex Types.ObjectReference
batiIndexReference = Lens.field @"indexReference"
{-# INLINEABLE batiIndexReference #-}
{-# DEPRECATED indexReference "Use generic-lens or generic-optics with 'indexReference' instead"  #-}

-- | A reference to the object that you are attaching to the index.
--
-- /Note:/ Consider using 'targetReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
batiTargetReference :: Lens.Lens' BatchAttachToIndex Types.ObjectReference
batiTargetReference = Lens.field @"targetReference"
{-# INLINEABLE batiTargetReference #-}
{-# DEPRECATED targetReference "Use generic-lens or generic-optics with 'targetReference' instead"  #-}

instance Core.FromJSON BatchAttachToIndex where
        toJSON BatchAttachToIndex{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("IndexReference" Core..= indexReference),
                  Core.Just ("TargetReference" Core..= targetReference)])
