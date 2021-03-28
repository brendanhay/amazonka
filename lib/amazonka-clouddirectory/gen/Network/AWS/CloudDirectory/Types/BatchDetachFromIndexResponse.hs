{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchDetachFromIndexResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.BatchDetachFromIndexResponse
  ( BatchDetachFromIndexResponse (..)
  -- * Smart constructor
  , mkBatchDetachFromIndexResponse
  -- * Lenses
  , bdfirDetachedObjectIdentifier
  ) where

import qualified Network.AWS.CloudDirectory.Types.DetachedObjectIdentifier as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a 'DetachFromIndex' response operation.
--
-- /See:/ 'mkBatchDetachFromIndexResponse' smart constructor.
newtype BatchDetachFromIndexResponse = BatchDetachFromIndexResponse'
  { detachedObjectIdentifier :: Core.Maybe Types.DetachedObjectIdentifier
    -- ^ The @ObjectIdentifier@ of the object that was detached from the index.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDetachFromIndexResponse' value with any optional fields omitted.
mkBatchDetachFromIndexResponse
    :: BatchDetachFromIndexResponse
mkBatchDetachFromIndexResponse
  = BatchDetachFromIndexResponse'{detachedObjectIdentifier =
                                    Core.Nothing}

-- | The @ObjectIdentifier@ of the object that was detached from the index.
--
-- /Note:/ Consider using 'detachedObjectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdfirDetachedObjectIdentifier :: Lens.Lens' BatchDetachFromIndexResponse (Core.Maybe Types.DetachedObjectIdentifier)
bdfirDetachedObjectIdentifier = Lens.field @"detachedObjectIdentifier"
{-# INLINEABLE bdfirDetachedObjectIdentifier #-}
{-# DEPRECATED detachedObjectIdentifier "Use generic-lens or generic-optics with 'detachedObjectIdentifier' instead"  #-}

instance Core.FromJSON BatchDetachFromIndexResponse where
        parseJSON
          = Core.withObject "BatchDetachFromIndexResponse" Core.$
              \ x ->
                BatchDetachFromIndexResponse' Core.<$>
                  (x Core..:? "DetachedObjectIdentifier")
