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
    bcoParentReference,
    bcoLinkName,
    bcoBatchReferenceName,
    bcoSchemaFacet,
    bcoObjectAttributeList,
  )
where

import Network.AWS.CloudDirectory.Types.AttributeKeyAndValue
import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.CloudDirectory.Types.SchemaFacet
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a 'CreateObject' operation.
--
-- /See:/ 'mkBatchCreateObject' smart constructor.
data BatchCreateObject = BatchCreateObject'
  { parentReference ::
      Lude.Maybe ObjectReference,
    linkName :: Lude.Maybe Lude.Text,
    batchReferenceName :: Lude.Maybe Lude.Text,
    schemaFacet :: [SchemaFacet],
    objectAttributeList :: [AttributeKeyAndValue]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchCreateObject' with the minimum fields required to make a request.
--
-- * 'batchReferenceName' - The batch reference name. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/transaction_support.html Transaction Support> for more information.
-- * 'linkName' - The name of the link.
-- * 'objectAttributeList' - An attribute map, which contains an attribute ARN as the key and attribute value as the map value.
-- * 'parentReference' - If specified, the parent reference to which this object will be attached.
-- * 'schemaFacet' - A list of @FacetArns@ that will be associated with the object. For more information, see 'arns' .
mkBatchCreateObject ::
  BatchCreateObject
mkBatchCreateObject =
  BatchCreateObject'
    { parentReference = Lude.Nothing,
      linkName = Lude.Nothing,
      batchReferenceName = Lude.Nothing,
      schemaFacet = Lude.mempty,
      objectAttributeList = Lude.mempty
    }

-- | If specified, the parent reference to which this object will be attached.
--
-- /Note:/ Consider using 'parentReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcoParentReference :: Lens.Lens' BatchCreateObject (Lude.Maybe ObjectReference)
bcoParentReference = Lens.lens (parentReference :: BatchCreateObject -> Lude.Maybe ObjectReference) (\s a -> s {parentReference = a} :: BatchCreateObject)
{-# DEPRECATED bcoParentReference "Use generic-lens or generic-optics with 'parentReference' instead." #-}

-- | The name of the link.
--
-- /Note:/ Consider using 'linkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcoLinkName :: Lens.Lens' BatchCreateObject (Lude.Maybe Lude.Text)
bcoLinkName = Lens.lens (linkName :: BatchCreateObject -> Lude.Maybe Lude.Text) (\s a -> s {linkName = a} :: BatchCreateObject)
{-# DEPRECATED bcoLinkName "Use generic-lens or generic-optics with 'linkName' instead." #-}

-- | The batch reference name. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/transaction_support.html Transaction Support> for more information.
--
-- /Note:/ Consider using 'batchReferenceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcoBatchReferenceName :: Lens.Lens' BatchCreateObject (Lude.Maybe Lude.Text)
bcoBatchReferenceName = Lens.lens (batchReferenceName :: BatchCreateObject -> Lude.Maybe Lude.Text) (\s a -> s {batchReferenceName = a} :: BatchCreateObject)
{-# DEPRECATED bcoBatchReferenceName "Use generic-lens or generic-optics with 'batchReferenceName' instead." #-}

-- | A list of @FacetArns@ that will be associated with the object. For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaFacet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcoSchemaFacet :: Lens.Lens' BatchCreateObject [SchemaFacet]
bcoSchemaFacet = Lens.lens (schemaFacet :: BatchCreateObject -> [SchemaFacet]) (\s a -> s {schemaFacet = a} :: BatchCreateObject)
{-# DEPRECATED bcoSchemaFacet "Use generic-lens or generic-optics with 'schemaFacet' instead." #-}

-- | An attribute map, which contains an attribute ARN as the key and attribute value as the map value.
--
-- /Note:/ Consider using 'objectAttributeList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcoObjectAttributeList :: Lens.Lens' BatchCreateObject [AttributeKeyAndValue]
bcoObjectAttributeList = Lens.lens (objectAttributeList :: BatchCreateObject -> [AttributeKeyAndValue]) (\s a -> s {objectAttributeList = a} :: BatchCreateObject)
{-# DEPRECATED bcoObjectAttributeList "Use generic-lens or generic-optics with 'objectAttributeList' instead." #-}

instance Lude.ToJSON BatchCreateObject where
  toJSON BatchCreateObject' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ParentReference" Lude..=) Lude.<$> parentReference,
            ("LinkName" Lude..=) Lude.<$> linkName,
            ("BatchReferenceName" Lude..=) Lude.<$> batchReferenceName,
            Lude.Just ("SchemaFacet" Lude..= schemaFacet),
            Lude.Just ("ObjectAttributeList" Lude..= objectAttributeList)
          ]
      )
