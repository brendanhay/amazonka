{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchCreateIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchCreateIndex
  ( BatchCreateIndex (..),

    -- * Smart constructor
    mkBatchCreateIndex,

    -- * Lenses
    bciOrderedIndexedAttributeList,
    bciIsUnique,
    bciBatchReferenceName,
    bciLinkName,
    bciParentReference,
  )
where

import qualified Network.AWS.CloudDirectory.Types.AttributeKey as Types
import qualified Network.AWS.CloudDirectory.Types.BatchReferenceName as Types
import qualified Network.AWS.CloudDirectory.Types.LinkName as Types
import qualified Network.AWS.CloudDirectory.Types.ObjectReference as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Creates an index object inside of a 'BatchRead' operation. For more information, see 'CreateIndex' and 'BatchReadRequest$Operations' .
--
-- /See:/ 'mkBatchCreateIndex' smart constructor.
data BatchCreateIndex = BatchCreateIndex'
  { -- | Specifies the attributes that should be indexed on. Currently only a single attribute is supported.
    orderedIndexedAttributeList :: [Types.AttributeKey],
    -- | Indicates whether the attribute that is being indexed has unique values or not.
    isUnique :: Core.Bool,
    -- | The batch reference name. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/transaction_support.html Transaction Support> for more information.
    batchReferenceName :: Core.Maybe Types.BatchReferenceName,
    -- | The name of the link between the parent object and the index object.
    linkName :: Core.Maybe Types.LinkName,
    -- | A reference to the parent object that contains the index object.
    parentReference :: Core.Maybe Types.ObjectReference
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchCreateIndex' value with any optional fields omitted.
mkBatchCreateIndex ::
  -- | 'isUnique'
  Core.Bool ->
  BatchCreateIndex
mkBatchCreateIndex isUnique =
  BatchCreateIndex'
    { orderedIndexedAttributeList = Core.mempty,
      isUnique,
      batchReferenceName = Core.Nothing,
      linkName = Core.Nothing,
      parentReference = Core.Nothing
    }

-- | Specifies the attributes that should be indexed on. Currently only a single attribute is supported.
--
-- /Note:/ Consider using 'orderedIndexedAttributeList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciOrderedIndexedAttributeList :: Lens.Lens' BatchCreateIndex [Types.AttributeKey]
bciOrderedIndexedAttributeList = Lens.field @"orderedIndexedAttributeList"
{-# DEPRECATED bciOrderedIndexedAttributeList "Use generic-lens or generic-optics with 'orderedIndexedAttributeList' instead." #-}

-- | Indicates whether the attribute that is being indexed has unique values or not.
--
-- /Note:/ Consider using 'isUnique' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciIsUnique :: Lens.Lens' BatchCreateIndex Core.Bool
bciIsUnique = Lens.field @"isUnique"
{-# DEPRECATED bciIsUnique "Use generic-lens or generic-optics with 'isUnique' instead." #-}

-- | The batch reference name. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/transaction_support.html Transaction Support> for more information.
--
-- /Note:/ Consider using 'batchReferenceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciBatchReferenceName :: Lens.Lens' BatchCreateIndex (Core.Maybe Types.BatchReferenceName)
bciBatchReferenceName = Lens.field @"batchReferenceName"
{-# DEPRECATED bciBatchReferenceName "Use generic-lens or generic-optics with 'batchReferenceName' instead." #-}

-- | The name of the link between the parent object and the index object.
--
-- /Note:/ Consider using 'linkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciLinkName :: Lens.Lens' BatchCreateIndex (Core.Maybe Types.LinkName)
bciLinkName = Lens.field @"linkName"
{-# DEPRECATED bciLinkName "Use generic-lens or generic-optics with 'linkName' instead." #-}

-- | A reference to the parent object that contains the index object.
--
-- /Note:/ Consider using 'parentReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciParentReference :: Lens.Lens' BatchCreateIndex (Core.Maybe Types.ObjectReference)
bciParentReference = Lens.field @"parentReference"
{-# DEPRECATED bciParentReference "Use generic-lens or generic-optics with 'parentReference' instead." #-}

instance Core.FromJSON BatchCreateIndex where
  toJSON BatchCreateIndex {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "OrderedIndexedAttributeList"
                  Core..= orderedIndexedAttributeList
              ),
            Core.Just ("IsUnique" Core..= isUnique),
            ("BatchReferenceName" Core..=) Core.<$> batchReferenceName,
            ("LinkName" Core..=) Core.<$> linkName,
            ("ParentReference" Core..=) Core.<$> parentReference
          ]
      )
