{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchCreateIndexResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchCreateIndexResponse
  ( BatchCreateIndexResponse (..),

    -- * Smart constructor
    mkBatchCreateIndexResponse,

    -- * Lenses
    bcirObjectIdentifier,
  )
where

import qualified Network.AWS.CloudDirectory.Types.ObjectIdentifier as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a 'CreateIndex' response operation.
--
-- /See:/ 'mkBatchCreateIndexResponse' smart constructor.
newtype BatchCreateIndexResponse = BatchCreateIndexResponse'
  { -- | The @ObjectIdentifier@ of the index created by this operation.
    objectIdentifier :: Core.Maybe Types.ObjectIdentifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchCreateIndexResponse' value with any optional fields omitted.
mkBatchCreateIndexResponse ::
  BatchCreateIndexResponse
mkBatchCreateIndexResponse =
  BatchCreateIndexResponse' {objectIdentifier = Core.Nothing}

-- | The @ObjectIdentifier@ of the index created by this operation.
--
-- /Note:/ Consider using 'objectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcirObjectIdentifier :: Lens.Lens' BatchCreateIndexResponse (Core.Maybe Types.ObjectIdentifier)
bcirObjectIdentifier = Lens.field @"objectIdentifier"
{-# DEPRECATED bcirObjectIdentifier "Use generic-lens or generic-optics with 'objectIdentifier' instead." #-}

instance Core.FromJSON BatchCreateIndexResponse where
  parseJSON =
    Core.withObject "BatchCreateIndexResponse" Core.$
      \x ->
        BatchCreateIndexResponse' Core.<$> (x Core..:? "ObjectIdentifier")
