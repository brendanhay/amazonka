{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchDeleteObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.BatchDeleteObject
  ( BatchDeleteObject (..)
  -- * Smart constructor
  , mkBatchDeleteObject
  -- * Lenses
  , bdoObjectReference
  ) where

import qualified Network.AWS.CloudDirectory.Types.ObjectReference as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a 'DeleteObject' operation.
--
-- /See:/ 'mkBatchDeleteObject' smart constructor.
newtype BatchDeleteObject = BatchDeleteObject'
  { objectReference :: Types.ObjectReference
    -- ^ The reference that identifies the object.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDeleteObject' value with any optional fields omitted.
mkBatchDeleteObject
    :: Types.ObjectReference -- ^ 'objectReference'
    -> BatchDeleteObject
mkBatchDeleteObject objectReference
  = BatchDeleteObject'{objectReference}

-- | The reference that identifies the object.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdoObjectReference :: Lens.Lens' BatchDeleteObject Types.ObjectReference
bdoObjectReference = Lens.field @"objectReference"
{-# INLINEABLE bdoObjectReference #-}
{-# DEPRECATED objectReference "Use generic-lens or generic-optics with 'objectReference' instead"  #-}

instance Core.FromJSON BatchDeleteObject where
        toJSON BatchDeleteObject{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ObjectReference" Core..= objectReference)])
