{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchDetachObjectResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.BatchDetachObjectResponse
  ( BatchDetachObjectResponse (..)
  -- * Smart constructor
  , mkBatchDetachObjectResponse
  -- * Lenses
  , bdorDetachedObjectIdentifier
  ) where

import qualified Network.AWS.CloudDirectory.Types.ObjectIdentifier as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a 'DetachObject' response operation.
--
-- /See:/ 'mkBatchDetachObjectResponse' smart constructor.
newtype BatchDetachObjectResponse = BatchDetachObjectResponse'
  { detachedObjectIdentifier :: Core.Maybe Types.ObjectIdentifier
    -- ^ The @ObjectIdentifier@ of the detached object.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDetachObjectResponse' value with any optional fields omitted.
mkBatchDetachObjectResponse
    :: BatchDetachObjectResponse
mkBatchDetachObjectResponse
  = BatchDetachObjectResponse'{detachedObjectIdentifier =
                                 Core.Nothing}

-- | The @ObjectIdentifier@ of the detached object.
--
-- /Note:/ Consider using 'detachedObjectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdorDetachedObjectIdentifier :: Lens.Lens' BatchDetachObjectResponse (Core.Maybe Types.ObjectIdentifier)
bdorDetachedObjectIdentifier = Lens.field @"detachedObjectIdentifier"
{-# INLINEABLE bdorDetachedObjectIdentifier #-}
{-# DEPRECATED detachedObjectIdentifier "Use generic-lens or generic-optics with 'detachedObjectIdentifier' instead"  #-}

instance Core.FromJSON BatchDetachObjectResponse where
        parseJSON
          = Core.withObject "BatchDetachObjectResponse" Core.$
              \ x ->
                BatchDetachObjectResponse' Core.<$>
                  (x Core..:? "detachedObjectIdentifier")
