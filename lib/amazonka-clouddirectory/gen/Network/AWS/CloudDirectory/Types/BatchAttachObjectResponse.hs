{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchAttachObjectResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.BatchAttachObjectResponse
  ( BatchAttachObjectResponse (..)
  -- * Smart constructor
  , mkBatchAttachObjectResponse
  -- * Lenses
  , baorAttachedObjectIdentifier
  ) where

import qualified Network.AWS.CloudDirectory.Types.ObjectIdentifier as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output batch 'AttachObject' response operation.
--
-- /See:/ 'mkBatchAttachObjectResponse' smart constructor.
newtype BatchAttachObjectResponse = BatchAttachObjectResponse'
  { attachedObjectIdentifier :: Core.Maybe Types.ObjectIdentifier
    -- ^ The @ObjectIdentifier@ of the object that has been attached.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchAttachObjectResponse' value with any optional fields omitted.
mkBatchAttachObjectResponse
    :: BatchAttachObjectResponse
mkBatchAttachObjectResponse
  = BatchAttachObjectResponse'{attachedObjectIdentifier =
                                 Core.Nothing}

-- | The @ObjectIdentifier@ of the object that has been attached.
--
-- /Note:/ Consider using 'attachedObjectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baorAttachedObjectIdentifier :: Lens.Lens' BatchAttachObjectResponse (Core.Maybe Types.ObjectIdentifier)
baorAttachedObjectIdentifier = Lens.field @"attachedObjectIdentifier"
{-# INLINEABLE baorAttachedObjectIdentifier #-}
{-# DEPRECATED attachedObjectIdentifier "Use generic-lens or generic-optics with 'attachedObjectIdentifier' instead"  #-}

instance Core.FromJSON BatchAttachObjectResponse where
        parseJSON
          = Core.withObject "BatchAttachObjectResponse" Core.$
              \ x ->
                BatchAttachObjectResponse' Core.<$>
                  (x Core..:? "attachedObjectIdentifier")
