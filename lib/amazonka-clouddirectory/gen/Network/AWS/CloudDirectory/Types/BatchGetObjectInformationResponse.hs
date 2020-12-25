{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchGetObjectInformationResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchGetObjectInformationResponse
  ( BatchGetObjectInformationResponse (..),

    -- * Smart constructor
    mkBatchGetObjectInformationResponse,

    -- * Lenses
    bgoirObjectIdentifier,
    bgoirSchemaFacets,
  )
where

import qualified Network.AWS.CloudDirectory.Types.ObjectIdentifier as Types
import qualified Network.AWS.CloudDirectory.Types.SchemaFacet as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a 'GetObjectInformation' response operation.
--
-- /See:/ 'mkBatchGetObjectInformationResponse' smart constructor.
data BatchGetObjectInformationResponse = BatchGetObjectInformationResponse'
  { -- | The @ObjectIdentifier@ of the specified object.
    objectIdentifier :: Core.Maybe Types.ObjectIdentifier,
    -- | The facets attached to the specified object.
    schemaFacets :: Core.Maybe [Types.SchemaFacet]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetObjectInformationResponse' value with any optional fields omitted.
mkBatchGetObjectInformationResponse ::
  BatchGetObjectInformationResponse
mkBatchGetObjectInformationResponse =
  BatchGetObjectInformationResponse'
    { objectIdentifier =
        Core.Nothing,
      schemaFacets = Core.Nothing
    }

-- | The @ObjectIdentifier@ of the specified object.
--
-- /Note:/ Consider using 'objectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgoirObjectIdentifier :: Lens.Lens' BatchGetObjectInformationResponse (Core.Maybe Types.ObjectIdentifier)
bgoirObjectIdentifier = Lens.field @"objectIdentifier"
{-# DEPRECATED bgoirObjectIdentifier "Use generic-lens or generic-optics with 'objectIdentifier' instead." #-}

-- | The facets attached to the specified object.
--
-- /Note:/ Consider using 'schemaFacets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgoirSchemaFacets :: Lens.Lens' BatchGetObjectInformationResponse (Core.Maybe [Types.SchemaFacet])
bgoirSchemaFacets = Lens.field @"schemaFacets"
{-# DEPRECATED bgoirSchemaFacets "Use generic-lens or generic-optics with 'schemaFacets' instead." #-}

instance Core.FromJSON BatchGetObjectInformationResponse where
  parseJSON =
    Core.withObject "BatchGetObjectInformationResponse" Core.$
      \x ->
        BatchGetObjectInformationResponse'
          Core.<$> (x Core..:? "ObjectIdentifier")
          Core.<*> (x Core..:? "SchemaFacets")
