{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchAttachToIndexResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchAttachToIndexResponse
  ( BatchAttachToIndexResponse (..),

    -- * Smart constructor
    mkBatchAttachToIndexResponse,

    -- * Lenses
    batirAttachedObjectIdentifier,
  )
where

import qualified Network.AWS.CloudDirectory.Types.ObjectIdentifier as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a 'AttachToIndex' response operation.
--
-- /See:/ 'mkBatchAttachToIndexResponse' smart constructor.
newtype BatchAttachToIndexResponse = BatchAttachToIndexResponse'
  { -- | The @ObjectIdentifier@ of the object that was attached to the index.
    attachedObjectIdentifier :: Core.Maybe Types.ObjectIdentifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchAttachToIndexResponse' value with any optional fields omitted.
mkBatchAttachToIndexResponse ::
  BatchAttachToIndexResponse
mkBatchAttachToIndexResponse =
  BatchAttachToIndexResponse'
    { attachedObjectIdentifier =
        Core.Nothing
    }

-- | The @ObjectIdentifier@ of the object that was attached to the index.
--
-- /Note:/ Consider using 'attachedObjectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
batirAttachedObjectIdentifier :: Lens.Lens' BatchAttachToIndexResponse (Core.Maybe Types.ObjectIdentifier)
batirAttachedObjectIdentifier = Lens.field @"attachedObjectIdentifier"
{-# DEPRECATED batirAttachedObjectIdentifier "Use generic-lens or generic-optics with 'attachedObjectIdentifier' instead." #-}

instance Core.FromJSON BatchAttachToIndexResponse where
  parseJSON =
    Core.withObject "BatchAttachToIndexResponse" Core.$
      \x ->
        BatchAttachToIndexResponse'
          Core.<$> (x Core..:? "AttachedObjectIdentifier")
