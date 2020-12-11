-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBSnapshotAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBSnapshotAttribute
  ( DBSnapshotAttribute (..),

    -- * Smart constructor
    mkDBSnapshotAttribute,

    -- * Lenses
    dsaAttributeValues,
    dsaAttributeName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the name and values of a manual DB snapshot attribute
--
-- Manual DB snapshot attributes are used to authorize other AWS accounts to restore a manual DB snapshot. For more information, see the @ModifyDBSnapshotAttribute@ API.
--
-- /See:/ 'mkDBSnapshotAttribute' smart constructor.
data DBSnapshotAttribute = DBSnapshotAttribute'
  { attributeValues ::
      Lude.Maybe [Lude.Text],
    attributeName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DBSnapshotAttribute' with the minimum fields required to make a request.
--
-- * 'attributeName' - The name of the manual DB snapshot attribute.
--
-- The attribute named @restore@ refers to the list of AWS accounts that have permission to copy or restore the manual DB cluster snapshot. For more information, see the @ModifyDBSnapshotAttribute@ API action.
-- * 'attributeValues' - The value or values for the manual DB snapshot attribute.
--
-- If the @AttributeName@ field is set to @restore@ , then this element returns a list of IDs of the AWS accounts that are authorized to copy or restore the manual DB snapshot. If a value of @all@ is in the list, then the manual DB snapshot is public and available for any AWS account to copy or restore.
mkDBSnapshotAttribute ::
  DBSnapshotAttribute
mkDBSnapshotAttribute =
  DBSnapshotAttribute'
    { attributeValues = Lude.Nothing,
      attributeName = Lude.Nothing
    }

-- | The value or values for the manual DB snapshot attribute.
--
-- If the @AttributeName@ field is set to @restore@ , then this element returns a list of IDs of the AWS accounts that are authorized to copy or restore the manual DB snapshot. If a value of @all@ is in the list, then the manual DB snapshot is public and available for any AWS account to copy or restore.
--
-- /Note:/ Consider using 'attributeValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaAttributeValues :: Lens.Lens' DBSnapshotAttribute (Lude.Maybe [Lude.Text])
dsaAttributeValues = Lens.lens (attributeValues :: DBSnapshotAttribute -> Lude.Maybe [Lude.Text]) (\s a -> s {attributeValues = a} :: DBSnapshotAttribute)
{-# DEPRECATED dsaAttributeValues "Use generic-lens or generic-optics with 'attributeValues' instead." #-}

-- | The name of the manual DB snapshot attribute.
--
-- The attribute named @restore@ refers to the list of AWS accounts that have permission to copy or restore the manual DB cluster snapshot. For more information, see the @ModifyDBSnapshotAttribute@ API action.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaAttributeName :: Lens.Lens' DBSnapshotAttribute (Lude.Maybe Lude.Text)
dsaAttributeName = Lens.lens (attributeName :: DBSnapshotAttribute -> Lude.Maybe Lude.Text) (\s a -> s {attributeName = a} :: DBSnapshotAttribute)
{-# DEPRECATED dsaAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

instance Lude.FromXML DBSnapshotAttribute where
  parseXML x =
    DBSnapshotAttribute'
      Lude.<$> ( x Lude..@? "AttributeValues" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "AttributeValue")
               )
      Lude.<*> (x Lude..@? "AttributeName")
