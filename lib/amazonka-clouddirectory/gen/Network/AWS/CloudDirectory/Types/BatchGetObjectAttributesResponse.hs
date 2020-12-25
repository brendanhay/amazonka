{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchGetObjectAttributesResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchGetObjectAttributesResponse
  ( BatchGetObjectAttributesResponse (..),

    -- * Smart constructor
    mkBatchGetObjectAttributesResponse,

    -- * Lenses
    bgoarAttributes,
  )
where

import qualified Network.AWS.CloudDirectory.Types.AttributeKeyAndValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a 'GetObjectAttributes' response operation.
--
-- /See:/ 'mkBatchGetObjectAttributesResponse' smart constructor.
newtype BatchGetObjectAttributesResponse = BatchGetObjectAttributesResponse'
  { -- | The attribute values that are associated with an object.
    attributes :: Core.Maybe [Types.AttributeKeyAndValue]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.NFData)

-- | Creates a 'BatchGetObjectAttributesResponse' value with any optional fields omitted.
mkBatchGetObjectAttributesResponse ::
  BatchGetObjectAttributesResponse
mkBatchGetObjectAttributesResponse =
  BatchGetObjectAttributesResponse' {attributes = Core.Nothing}

-- | The attribute values that are associated with an object.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgoarAttributes :: Lens.Lens' BatchGetObjectAttributesResponse (Core.Maybe [Types.AttributeKeyAndValue])
bgoarAttributes = Lens.field @"attributes"
{-# DEPRECATED bgoarAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Core.FromJSON BatchGetObjectAttributesResponse where
  parseJSON =
    Core.withObject "BatchGetObjectAttributesResponse" Core.$
      \x ->
        BatchGetObjectAttributesResponse'
          Core.<$> (x Core..:? "Attributes")
