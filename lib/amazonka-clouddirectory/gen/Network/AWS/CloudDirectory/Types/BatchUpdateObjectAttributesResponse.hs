{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchUpdateObjectAttributesResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchUpdateObjectAttributesResponse
  ( BatchUpdateObjectAttributesResponse (..),

    -- * Smart constructor
    mkBatchUpdateObjectAttributesResponse,

    -- * Lenses
    buoarObjectIdentifier,
  )
where

import qualified Network.AWS.CloudDirectory.Types.ObjectIdentifier as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a @BatchUpdate@ response operation.
--
-- /See:/ 'mkBatchUpdateObjectAttributesResponse' smart constructor.
newtype BatchUpdateObjectAttributesResponse = BatchUpdateObjectAttributesResponse'
  { -- | ID that is associated with the object.
    objectIdentifier :: Core.Maybe Types.ObjectIdentifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchUpdateObjectAttributesResponse' value with any optional fields omitted.
mkBatchUpdateObjectAttributesResponse ::
  BatchUpdateObjectAttributesResponse
mkBatchUpdateObjectAttributesResponse =
  BatchUpdateObjectAttributesResponse'
    { objectIdentifier =
        Core.Nothing
    }

-- | ID that is associated with the object.
--
-- /Note:/ Consider using 'objectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
buoarObjectIdentifier :: Lens.Lens' BatchUpdateObjectAttributesResponse (Core.Maybe Types.ObjectIdentifier)
buoarObjectIdentifier = Lens.field @"objectIdentifier"
{-# DEPRECATED buoarObjectIdentifier "Use generic-lens or generic-optics with 'objectIdentifier' instead." #-}

instance Core.FromJSON BatchUpdateObjectAttributesResponse where
  parseJSON =
    Core.withObject "BatchUpdateObjectAttributesResponse" Core.$
      \x ->
        BatchUpdateObjectAttributesResponse'
          Core.<$> (x Core..:? "ObjectIdentifier")
