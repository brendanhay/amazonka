{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBSnapshotAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.DBSnapshotAttribute
  ( DBSnapshotAttribute (..)
  -- * Smart constructor
  , mkDBSnapshotAttribute
  -- * Lenses
  , dbsaAttributeName
  , dbsaAttributeValues
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the name and values of a manual DB snapshot attribute
--
-- Manual DB snapshot attributes are used to authorize other AWS accounts to restore a manual DB snapshot. For more information, see the @ModifyDBSnapshotAttribute@ API.
--
-- /See:/ 'mkDBSnapshotAttribute' smart constructor.
data DBSnapshotAttribute = DBSnapshotAttribute'
  { attributeName :: Core.Maybe Core.Text
    -- ^ The name of the manual DB snapshot attribute.
--
-- The attribute named @restore@ refers to the list of AWS accounts that have permission to copy or restore the manual DB cluster snapshot. For more information, see the @ModifyDBSnapshotAttribute@ API action.
  , attributeValues :: Core.Maybe [Core.Text]
    -- ^ The value or values for the manual DB snapshot attribute.
--
-- If the @AttributeName@ field is set to @restore@ , then this element returns a list of IDs of the AWS accounts that are authorized to copy or restore the manual DB snapshot. If a value of @all@ is in the list, then the manual DB snapshot is public and available for any AWS account to copy or restore.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DBSnapshotAttribute' value with any optional fields omitted.
mkDBSnapshotAttribute
    :: DBSnapshotAttribute
mkDBSnapshotAttribute
  = DBSnapshotAttribute'{attributeName = Core.Nothing,
                         attributeValues = Core.Nothing}

-- | The name of the manual DB snapshot attribute.
--
-- The attribute named @restore@ refers to the list of AWS accounts that have permission to copy or restore the manual DB cluster snapshot. For more information, see the @ModifyDBSnapshotAttribute@ API action.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsaAttributeName :: Lens.Lens' DBSnapshotAttribute (Core.Maybe Core.Text)
dbsaAttributeName = Lens.field @"attributeName"
{-# INLINEABLE dbsaAttributeName #-}
{-# DEPRECATED attributeName "Use generic-lens or generic-optics with 'attributeName' instead"  #-}

-- | The value or values for the manual DB snapshot attribute.
--
-- If the @AttributeName@ field is set to @restore@ , then this element returns a list of IDs of the AWS accounts that are authorized to copy or restore the manual DB snapshot. If a value of @all@ is in the list, then the manual DB snapshot is public and available for any AWS account to copy or restore.
--
-- /Note:/ Consider using 'attributeValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsaAttributeValues :: Lens.Lens' DBSnapshotAttribute (Core.Maybe [Core.Text])
dbsaAttributeValues = Lens.field @"attributeValues"
{-# INLINEABLE dbsaAttributeValues #-}
{-# DEPRECATED attributeValues "Use generic-lens or generic-optics with 'attributeValues' instead"  #-}

instance Core.FromXML DBSnapshotAttribute where
        parseXML x
          = DBSnapshotAttribute' Core.<$>
              (x Core..@? "AttributeName") Core.<*>
                x Core..@? "AttributeValues" Core..<@>
                  Core.parseXMLList "AttributeValue"
