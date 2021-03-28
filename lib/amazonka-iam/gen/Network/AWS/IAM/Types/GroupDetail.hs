{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.GroupDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.GroupDetail
  ( GroupDetail (..)
  -- * Smart constructor
  , mkGroupDetail
  -- * Lenses
  , gdArn
  , gdAttachedManagedPolicies
  , gdCreateDate
  , gdGroupId
  , gdGroupName
  , gdGroupPolicyList
  , gdPath
  ) where

import qualified Network.AWS.IAM.Types.Arn as Types
import qualified Network.AWS.IAM.Types.AttachedPolicy as Types
import qualified Network.AWS.IAM.Types.GroupNameType as Types
import qualified Network.AWS.IAM.Types.IdType as Types
import qualified Network.AWS.IAM.Types.Path as Types
import qualified Network.AWS.IAM.Types.PolicyDetail as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about an IAM group, including all of the group's policies.
--
-- This data type is used as a response element in the 'GetAccountAuthorizationDetails' operation.
--
-- /See:/ 'mkGroupDetail' smart constructor.
data GroupDetail = GroupDetail'
  { arn :: Core.Maybe Types.Arn
  , attachedManagedPolicies :: Core.Maybe [Types.AttachedPolicy]
    -- ^ A list of the managed policies attached to the group.
  , createDate :: Core.Maybe Core.UTCTime
    -- ^ The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the group was created.
  , groupId :: Core.Maybe Types.IdType
    -- ^ The stable and unique string identifying the group. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
  , groupName :: Core.Maybe Types.GroupNameType
    -- ^ The friendly name that identifies the group.
  , groupPolicyList :: Core.Maybe [Types.PolicyDetail]
    -- ^ A list of the inline policies embedded in the group.
  , path :: Core.Maybe Types.Path
    -- ^ The path to the group. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GroupDetail' value with any optional fields omitted.
mkGroupDetail
    :: GroupDetail
mkGroupDetail
  = GroupDetail'{arn = Core.Nothing,
                 attachedManagedPolicies = Core.Nothing, createDate = Core.Nothing,
                 groupId = Core.Nothing, groupName = Core.Nothing,
                 groupPolicyList = Core.Nothing, path = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdArn :: Lens.Lens' GroupDetail (Core.Maybe Types.Arn)
gdArn = Lens.field @"arn"
{-# INLINEABLE gdArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | A list of the managed policies attached to the group.
--
-- /Note:/ Consider using 'attachedManagedPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdAttachedManagedPolicies :: Lens.Lens' GroupDetail (Core.Maybe [Types.AttachedPolicy])
gdAttachedManagedPolicies = Lens.field @"attachedManagedPolicies"
{-# INLINEABLE gdAttachedManagedPolicies #-}
{-# DEPRECATED attachedManagedPolicies "Use generic-lens or generic-optics with 'attachedManagedPolicies' instead"  #-}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the group was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdCreateDate :: Lens.Lens' GroupDetail (Core.Maybe Core.UTCTime)
gdCreateDate = Lens.field @"createDate"
{-# INLINEABLE gdCreateDate #-}
{-# DEPRECATED createDate "Use generic-lens or generic-optics with 'createDate' instead"  #-}

-- | The stable and unique string identifying the group. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdGroupId :: Lens.Lens' GroupDetail (Core.Maybe Types.IdType)
gdGroupId = Lens.field @"groupId"
{-# INLINEABLE gdGroupId #-}
{-# DEPRECATED groupId "Use generic-lens or generic-optics with 'groupId' instead"  #-}

-- | The friendly name that identifies the group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdGroupName :: Lens.Lens' GroupDetail (Core.Maybe Types.GroupNameType)
gdGroupName = Lens.field @"groupName"
{-# INLINEABLE gdGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

-- | A list of the inline policies embedded in the group.
--
-- /Note:/ Consider using 'groupPolicyList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdGroupPolicyList :: Lens.Lens' GroupDetail (Core.Maybe [Types.PolicyDetail])
gdGroupPolicyList = Lens.field @"groupPolicyList"
{-# INLINEABLE gdGroupPolicyList #-}
{-# DEPRECATED groupPolicyList "Use generic-lens or generic-optics with 'groupPolicyList' instead"  #-}

-- | The path to the group. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdPath :: Lens.Lens' GroupDetail (Core.Maybe Types.Path)
gdPath = Lens.field @"path"
{-# INLINEABLE gdPath #-}
{-# DEPRECATED path "Use generic-lens or generic-optics with 'path' instead"  #-}

instance Core.FromXML GroupDetail where
        parseXML x
          = GroupDetail' Core.<$>
              (x Core..@? "Arn") Core.<*>
                x Core..@? "AttachedManagedPolicies" Core..<@>
                  Core.parseXMLList "member"
                Core.<*> x Core..@? "CreateDate"
                Core.<*> x Core..@? "GroupId"
                Core.<*> x Core..@? "GroupName"
                Core.<*>
                x Core..@? "GroupPolicyList" Core..<@> Core.parseXMLList "member"
                Core.<*> x Core..@? "Path"
