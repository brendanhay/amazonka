{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchDetachObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.BatchDetachObject
  ( BatchDetachObject (..)
  -- * Smart constructor
  , mkBatchDetachObject
  -- * Lenses
  , bdoParentReference
  , bdoLinkName
  , bdoBatchReferenceName
  ) where

import qualified Network.AWS.CloudDirectory.Types.BatchReferenceName as Types
import qualified Network.AWS.CloudDirectory.Types.LinkName as Types
import qualified Network.AWS.CloudDirectory.Types.ObjectReference as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a 'DetachObject' operation.
--
-- /See:/ 'mkBatchDetachObject' smart constructor.
data BatchDetachObject = BatchDetachObject'
  { parentReference :: Types.ObjectReference
    -- ^ Parent reference from which the object with the specified link name is detached.
  , linkName :: Types.LinkName
    -- ^ The name of the link.
  , batchReferenceName :: Core.Maybe Types.BatchReferenceName
    -- ^ The batch reference name. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/transaction_support.html Transaction Support> for more information.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDetachObject' value with any optional fields omitted.
mkBatchDetachObject
    :: Types.ObjectReference -- ^ 'parentReference'
    -> Types.LinkName -- ^ 'linkName'
    -> BatchDetachObject
mkBatchDetachObject parentReference linkName
  = BatchDetachObject'{parentReference, linkName,
                       batchReferenceName = Core.Nothing}

-- | Parent reference from which the object with the specified link name is detached.
--
-- /Note:/ Consider using 'parentReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdoParentReference :: Lens.Lens' BatchDetachObject Types.ObjectReference
bdoParentReference = Lens.field @"parentReference"
{-# INLINEABLE bdoParentReference #-}
{-# DEPRECATED parentReference "Use generic-lens or generic-optics with 'parentReference' instead"  #-}

-- | The name of the link.
--
-- /Note:/ Consider using 'linkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdoLinkName :: Lens.Lens' BatchDetachObject Types.LinkName
bdoLinkName = Lens.field @"linkName"
{-# INLINEABLE bdoLinkName #-}
{-# DEPRECATED linkName "Use generic-lens or generic-optics with 'linkName' instead"  #-}

-- | The batch reference name. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/transaction_support.html Transaction Support> for more information.
--
-- /Note:/ Consider using 'batchReferenceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdoBatchReferenceName :: Lens.Lens' BatchDetachObject (Core.Maybe Types.BatchReferenceName)
bdoBatchReferenceName = Lens.field @"batchReferenceName"
{-# INLINEABLE bdoBatchReferenceName #-}
{-# DEPRECATED batchReferenceName "Use generic-lens or generic-optics with 'batchReferenceName' instead"  #-}

instance Core.FromJSON BatchDetachObject where
        toJSON BatchDetachObject{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ParentReference" Core..= parentReference),
                  Core.Just ("LinkName" Core..= linkName),
                  ("BatchReferenceName" Core..=) Core.<$> batchReferenceName])
