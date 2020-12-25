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
    dbcsaAttributeName,
    dbcsaAttributeValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.AttributeName as Types
import qualified Network.AWS.RDS.Types.String as Types

-- | Contains the name and values of a manual DB cluster snapshot attribute.
--
-- Manual DB cluster snapshot attributes are used to authorize other AWS accounts to restore a manual DB cluster snapshot. For more information, see the @ModifyDBClusterSnapshotAttribute@ API action.
--
-- /See:/ 'mkDBClusterSnapshotAttribute' smart constructor.
data DBClusterSnapshotAttribute = DBClusterSnapshotAttribute'
  { -- | The name of the manual DB cluster snapshot attribute.
    --
    -- The attribute named @restore@ refers to the list of AWS accounts that have permission to copy or restore the manual DB cluster snapshot. For more information, see the @ModifyDBClusterSnapshotAttribute@ API action.
    attributeName :: Core.Maybe Types.AttributeName,
    -- | The value(s) for the manual DB cluster snapshot attribute.
    --
    -- If the @AttributeName@ field is set to @restore@ , then this element returns a list of IDs of the AWS accounts that are authorized to copy or restore the manual DB cluster snapshot. If a value of @all@ is in the list, then the manual DB cluster snapshot is public and available for any AWS account to copy or restore.
    attributeValues :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DBClusterSnapshotAttribute' value with any optional fields omitted.
mkDBClusterSnapshotAttribute ::
  DBClusterSnapshotAttribute
mkDBClusterSnapshotAttribute =
  DBClusterSnapshotAttribute'
    { attributeName = Core.Nothing,
      attributeValues = Core.Nothing
    }

-- | The name of the manual DB cluster snapshot attribute.
--
-- The attribute named @restore@ refers to the list of AWS accounts that have permission to copy or restore the manual DB cluster snapshot. For more information, see the @ModifyDBClusterSnapshotAttribute@ API action.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsaAttributeName :: Lens.Lens' DBClusterSnapshotAttribute (Core.Maybe Types.AttributeName)
dbcsaAttributeName = Lens.field @"attributeName"
{-# DEPRECATED dbcsaAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

-- | The value(s) for the manual DB cluster snapshot attribute.
--
-- If the @AttributeName@ field is set to @restore@ , then this element returns a list of IDs of the AWS accounts that are authorized to copy or restore the manual DB cluster snapshot. If a value of @all@ is in the list, then the manual DB cluster snapshot is public and available for any AWS account to copy or restore.
--
-- /Note:/ Consider using 'attributeValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcsaAttributeValues :: Lens.Lens' DBClusterSnapshotAttribute (Core.Maybe [Types.String])
dbcsaAttributeValues = Lens.field @"attributeValues"
{-# DEPRECATED dbcsaAttributeValues "Use generic-lens or generic-optics with 'attributeValues' instead." #-}

instance Core.FromXML DBClusterSnapshotAttribute where
  parseXML x =
    DBClusterSnapshotAttribute'
      Core.<$> (x Core..@? "AttributeName")
      Core.<*> ( x Core..@? "AttributeValues"
                   Core..<@> Core.parseXMLList "AttributeValue"
               )
