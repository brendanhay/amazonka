{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchCreateObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchCreateObject
  ( BatchCreateObject (..),

    -- * Smart constructor
    mkBatchCreateObject,

    -- * Lenses
    bcoSchemaFacet,
    bcoObjectAttributeList,
    bcoBatchReferenceName,
    bcoLinkName,
    bcoParentReference,
  )
where

import qualified Network.AWS.CloudDirectory.Types.AttributeKeyAndValue as Types
import qualified Network.AWS.CloudDirectory.Types.BatchReferenceName as Types
import qualified Network.AWS.CloudDirectory.Types.LinkName as Types
import qualified Network.AWS.CloudDirectory.Types.ObjectReference as Types
import qualified Network.AWS.CloudDirectory.Types.SchemaFacet as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a 'CreateObject' operation.
--
-- /See:/ 'mkBatchCreateObject' smart constructor.
data BatchCreateObject = BatchCreateObject'
  { -- | A list of @FacetArns@ that will be associated with the object. For more information, see 'arns' .
    schemaFacet :: [Types.SchemaFacet],
    -- | An attribute map, which contains an attribute ARN as the key and attribute value as the map value.
    objectAttributeList :: [Types.AttributeKeyAndValue],
    -- | The batch reference name. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/transaction_support.html Transaction Support> for more information.
    batchReferenceName :: Core.Maybe Types.BatchReferenceName,
    -- | The name of the link.
    linkName :: Core.Maybe Types.LinkName,
    -- | If specified, the parent reference to which this object will be attached.
    parentReference :: Core.Maybe Types.ObjectReference
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BatchCreateObject' value with any optional fields omitted.
mkBatchCreateObject ::
  BatchCreateObject
mkBatchCreateObject =
  BatchCreateObject'
    { schemaFacet = Core.mempty,
      objectAttributeList = Core.mempty,
      batchReferenceName = Core.Nothing,
      linkName = Core.Nothing,
      parentReference = Core.Nothing
    }

-- | A list of @FacetArns@ that will be associated with the object. For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaFacet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcoSchemaFacet :: Lens.Lens' BatchCreateObject [Types.SchemaFacet]
bcoSchemaFacet = Lens.field @"schemaFacet"
{-# DEPRECATED bcoSchemaFacet "Use generic-lens or generic-optics with 'schemaFacet' instead." #-}

-- | An attribute map, which contains an attribute ARN as the key and attribute value as the map value.
--
-- /Note:/ Consider using 'objectAttributeList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcoObjectAttributeList :: Lens.Lens' BatchCreateObject [Types.AttributeKeyAndValue]
bcoObjectAttributeList = Lens.field @"objectAttributeList"
{-# DEPRECATED bcoObjectAttributeList "Use generic-lens or generic-optics with 'objectAttributeList' instead." #-}

-- | The batch reference name. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/transaction_support.html Transaction Support> for more information.
--
-- /Note:/ Consider using 'batchReferenceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcoBatchReferenceName :: Lens.Lens' BatchCreateObject (Core.Maybe Types.BatchReferenceName)
bcoBatchReferenceName = Lens.field @"batchReferenceName"
{-# DEPRECATED bcoBatchReferenceName "Use generic-lens or generic-optics with 'batchReferenceName' instead." #-}

-- | The name of the link.
--
-- /Note:/ Consider using 'linkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcoLinkName :: Lens.Lens' BatchCreateObject (Core.Maybe Types.LinkName)
bcoLinkName = Lens.field @"linkName"
{-# DEPRECATED bcoLinkName "Use generic-lens or generic-optics with 'linkName' instead." #-}

-- | If specified, the parent reference to which this object will be attached.
--
-- /Note:/ Consider using 'parentReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcoParentReference :: Lens.Lens' BatchCreateObject (Core.Maybe Types.ObjectReference)
bcoParentReference = Lens.field @"parentReference"
{-# DEPRECATED bcoParentReference "Use generic-lens or generic-optics with 'parentReference' instead." #-}

instance Core.FromJSON BatchCreateObject where
  toJSON BatchCreateObject {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SchemaFacet" Core..= schemaFacet),
            Core.Just ("ObjectAttributeList" Core..= objectAttributeList),
            ("BatchReferenceName" Core..=) Core.<$> batchReferenceName,
            ("LinkName" Core..=) Core.<$> linkName,
            ("ParentReference" Core..=) Core.<$> parentReference
          ]
      )
