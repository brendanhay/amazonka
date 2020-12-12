{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBClusterSnapshotAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBClusterSnapshotAttribute
  ( DBClusterSnapshotAttribute (..),

    -- * Smart constructor
    mkDBClusterSnapshotAttribute,

    -- * Lenses
    dcsaAttributeValues,
    dcsaAttributeName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the name and values of a manual DB cluster snapshot attribute.
--
-- Manual DB cluster snapshot attributes are used to authorize other AWS accounts to restore a manual DB cluster snapshot. For more information, see the @ModifyDBClusterSnapshotAttribute@ API action.
--
-- /See:/ 'mkDBClusterSnapshotAttribute' smart constructor.
data DBClusterSnapshotAttribute = DBClusterSnapshotAttribute'
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

-- | Creates a value of 'DBClusterSnapshotAttribute' with the minimum fields required to make a request.
--
-- * 'attributeName' - The name of the manual DB cluster snapshot attribute.
--
-- The attribute named @restore@ refers to the list of AWS accounts that have permission to copy or restore the manual DB cluster snapshot. For more information, see the @ModifyDBClusterSnapshotAttribute@ API action.
-- * 'attributeValues' - The value(s) for the manual DB cluster snapshot attribute.
--
-- If the @AttributeName@ field is set to @restore@ , then this element returns a list of IDs of the AWS accounts that are authorized to copy or restore the manual DB cluster snapshot. If a value of @all@ is in the list, then the manual DB cluster snapshot is public and available for any AWS account to copy or restore.
mkDBClusterSnapshotAttribute ::
  DBClusterSnapshotAttribute
mkDBClusterSnapshotAttribute =
  DBClusterSnapshotAttribute'
    { attributeValues = Lude.Nothing,
      attributeName = Lude.Nothing
    }

-- | The value(s) for the manual DB cluster snapshot attribute.
--
-- If the @AttributeName@ field is set to @restore@ , then this element returns a list of IDs of the AWS accounts that are authorized to copy or restore the manual DB cluster snapshot. If a value of @all@ is in the list, then the manual DB cluster snapshot is public and available for any AWS account to copy or restore.
--
-- /Note:/ Consider using 'attributeValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsaAttributeValues :: Lens.Lens' DBClusterSnapshotAttribute (Lude.Maybe [Lude.Text])
dcsaAttributeValues = Lens.lens (attributeValues :: DBClusterSnapshotAttribute -> Lude.Maybe [Lude.Text]) (\s a -> s {attributeValues = a} :: DBClusterSnapshotAttribute)
{-# DEPRECATED dcsaAttributeValues "Use generic-lens or generic-optics with 'attributeValues' instead." #-}

-- | The name of the manual DB cluster snapshot attribute.
--
-- The attribute named @restore@ refers to the list of AWS accounts that have permission to copy or restore the manual DB cluster snapshot. For more information, see the @ModifyDBClusterSnapshotAttribute@ API action.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsaAttributeName :: Lens.Lens' DBClusterSnapshotAttribute (Lude.Maybe Lude.Text)
dcsaAttributeName = Lens.lens (attributeName :: DBClusterSnapshotAttribute -> Lude.Maybe Lude.Text) (\s a -> s {attributeName = a} :: DBClusterSnapshotAttribute)
{-# DEPRECATED dcsaAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

instance Lude.FromXML DBClusterSnapshotAttribute where
  parseXML x =
    DBClusterSnapshotAttribute'
      Lude.<$> ( x Lude..@? "AttributeValues" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "AttributeValue")
               )
      Lude.<*> (x Lude..@? "AttributeName")
