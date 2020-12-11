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
    bciParentReference,
    bciLinkName,
    bciBatchReferenceName,
    bciOrderedIndexedAttributeList,
    bciIsUnique,
  )
where

import Network.AWS.CloudDirectory.Types.AttributeKey
import Network.AWS.CloudDirectory.Types.ObjectReference
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Creates an index object inside of a 'BatchRead' operation. For more information, see 'CreateIndex' and 'BatchReadRequest$Operations' .
--
-- /See:/ 'mkBatchCreateIndex' smart constructor.
data BatchCreateIndex = BatchCreateIndex'
  { parentReference ::
      Lude.Maybe ObjectReference,
    linkName :: Lude.Maybe Lude.Text,
    batchReferenceName :: Lude.Maybe Lude.Text,
    orderedIndexedAttributeList :: [AttributeKey],
    isUnique :: Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchCreateIndex' with the minimum fields required to make a request.
--
-- * 'batchReferenceName' - The batch reference name. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/transaction_support.html Transaction Support> for more information.
-- * 'isUnique' - Indicates whether the attribute that is being indexed has unique values or not.
-- * 'linkName' - The name of the link between the parent object and the index object.
-- * 'orderedIndexedAttributeList' - Specifies the attributes that should be indexed on. Currently only a single attribute is supported.
-- * 'parentReference' - A reference to the parent object that contains the index object.
mkBatchCreateIndex ::
  -- | 'isUnique'
  Lude.Bool ->
  BatchCreateIndex
mkBatchCreateIndex pIsUnique_ =
  BatchCreateIndex'
    { parentReference = Lude.Nothing,
      linkName = Lude.Nothing,
      batchReferenceName = Lude.Nothing,
      orderedIndexedAttributeList = Lude.mempty,
      isUnique = pIsUnique_
    }

-- | A reference to the parent object that contains the index object.
--
-- /Note:/ Consider using 'parentReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciParentReference :: Lens.Lens' BatchCreateIndex (Lude.Maybe ObjectReference)
bciParentReference = Lens.lens (parentReference :: BatchCreateIndex -> Lude.Maybe ObjectReference) (\s a -> s {parentReference = a} :: BatchCreateIndex)
{-# DEPRECATED bciParentReference "Use generic-lens or generic-optics with 'parentReference' instead." #-}

-- | The name of the link between the parent object and the index object.
--
-- /Note:/ Consider using 'linkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciLinkName :: Lens.Lens' BatchCreateIndex (Lude.Maybe Lude.Text)
bciLinkName = Lens.lens (linkName :: BatchCreateIndex -> Lude.Maybe Lude.Text) (\s a -> s {linkName = a} :: BatchCreateIndex)
{-# DEPRECATED bciLinkName "Use generic-lens or generic-optics with 'linkName' instead." #-}

-- | The batch reference name. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/transaction_support.html Transaction Support> for more information.
--
-- /Note:/ Consider using 'batchReferenceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciBatchReferenceName :: Lens.Lens' BatchCreateIndex (Lude.Maybe Lude.Text)
bciBatchReferenceName = Lens.lens (batchReferenceName :: BatchCreateIndex -> Lude.Maybe Lude.Text) (\s a -> s {batchReferenceName = a} :: BatchCreateIndex)
{-# DEPRECATED bciBatchReferenceName "Use generic-lens or generic-optics with 'batchReferenceName' instead." #-}

-- | Specifies the attributes that should be indexed on. Currently only a single attribute is supported.
--
-- /Note:/ Consider using 'orderedIndexedAttributeList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciOrderedIndexedAttributeList :: Lens.Lens' BatchCreateIndex [AttributeKey]
bciOrderedIndexedAttributeList = Lens.lens (orderedIndexedAttributeList :: BatchCreateIndex -> [AttributeKey]) (\s a -> s {orderedIndexedAttributeList = a} :: BatchCreateIndex)
{-# DEPRECATED bciOrderedIndexedAttributeList "Use generic-lens or generic-optics with 'orderedIndexedAttributeList' instead." #-}

-- | Indicates whether the attribute that is being indexed has unique values or not.
--
-- /Note:/ Consider using 'isUnique' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bciIsUnique :: Lens.Lens' BatchCreateIndex Lude.Bool
bciIsUnique = Lens.lens (isUnique :: BatchCreateIndex -> Lude.Bool) (\s a -> s {isUnique = a} :: BatchCreateIndex)
{-# DEPRECATED bciIsUnique "Use generic-lens or generic-optics with 'isUnique' instead." #-}

instance Lude.ToJSON BatchCreateIndex where
  toJSON BatchCreateIndex' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ParentReference" Lude..=) Lude.<$> parentReference,
            ("LinkName" Lude..=) Lude.<$> linkName,
            ("BatchReferenceName" Lude..=) Lude.<$> batchReferenceName,
            Lude.Just
              ( "OrderedIndexedAttributeList"
                  Lude..= orderedIndexedAttributeList
              ),
            Lude.Just ("IsUnique" Lude..= isUnique)
          ]
      )
