{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.GroupDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.GroupDetail
  ( GroupDetail (..),

    -- * Smart constructor
    mkGroupDetail,

    -- * Lenses
    gdARN,
    gdPath,
    gdCreateDate,
    gdGroupId,
    gdGroupPolicyList,
    gdGroupName,
    gdAttachedManagedPolicies,
  )
where

import Network.AWS.IAM.Types.AttachedPolicy
import Network.AWS.IAM.Types.PolicyDetail
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about an IAM group, including all of the group's policies.
--
-- This data type is used as a response element in the 'GetAccountAuthorizationDetails' operation.
--
-- /See:/ 'mkGroupDetail' smart constructor.
data GroupDetail = GroupDetail'
  { arn :: Lude.Maybe Lude.Text,
    path :: Lude.Maybe Lude.Text,
    createDate :: Lude.Maybe Lude.DateTime,
    groupId :: Lude.Maybe Lude.Text,
    groupPolicyList :: Lude.Maybe [PolicyDetail],
    groupName :: Lude.Maybe Lude.Text,
    attachedManagedPolicies :: Lude.Maybe [AttachedPolicy]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GroupDetail' with the minimum fields required to make a request.
--
-- * 'arn' - Undocumented field.
-- * 'attachedManagedPolicies' - A list of the managed policies attached to the group.
-- * 'createDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the group was created.
-- * 'groupId' - The stable and unique string identifying the group. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
-- * 'groupName' - The friendly name that identifies the group.
-- * 'groupPolicyList' - A list of the inline policies embedded in the group.
-- * 'path' - The path to the group. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
mkGroupDetail ::
  GroupDetail
mkGroupDetail =
  GroupDetail'
    { arn = Lude.Nothing,
      path = Lude.Nothing,
      createDate = Lude.Nothing,
      groupId = Lude.Nothing,
      groupPolicyList = Lude.Nothing,
      groupName = Lude.Nothing,
      attachedManagedPolicies = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdARN :: Lens.Lens' GroupDetail (Lude.Maybe Lude.Text)
gdARN = Lens.lens (arn :: GroupDetail -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: GroupDetail)
{-# DEPRECATED gdARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The path to the group. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdPath :: Lens.Lens' GroupDetail (Lude.Maybe Lude.Text)
gdPath = Lens.lens (path :: GroupDetail -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: GroupDetail)
{-# DEPRECATED gdPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the group was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdCreateDate :: Lens.Lens' GroupDetail (Lude.Maybe Lude.DateTime)
gdCreateDate = Lens.lens (createDate :: GroupDetail -> Lude.Maybe Lude.DateTime) (\s a -> s {createDate = a} :: GroupDetail)
{-# DEPRECATED gdCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | The stable and unique string identifying the group. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdGroupId :: Lens.Lens' GroupDetail (Lude.Maybe Lude.Text)
gdGroupId = Lens.lens (groupId :: GroupDetail -> Lude.Maybe Lude.Text) (\s a -> s {groupId = a} :: GroupDetail)
{-# DEPRECATED gdGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | A list of the inline policies embedded in the group.
--
-- /Note:/ Consider using 'groupPolicyList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdGroupPolicyList :: Lens.Lens' GroupDetail (Lude.Maybe [PolicyDetail])
gdGroupPolicyList = Lens.lens (groupPolicyList :: GroupDetail -> Lude.Maybe [PolicyDetail]) (\s a -> s {groupPolicyList = a} :: GroupDetail)
{-# DEPRECATED gdGroupPolicyList "Use generic-lens or generic-optics with 'groupPolicyList' instead." #-}

-- | The friendly name that identifies the group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdGroupName :: Lens.Lens' GroupDetail (Lude.Maybe Lude.Text)
gdGroupName = Lens.lens (groupName :: GroupDetail -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: GroupDetail)
{-# DEPRECATED gdGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | A list of the managed policies attached to the group.
--
-- /Note:/ Consider using 'attachedManagedPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdAttachedManagedPolicies :: Lens.Lens' GroupDetail (Lude.Maybe [AttachedPolicy])
gdAttachedManagedPolicies = Lens.lens (attachedManagedPolicies :: GroupDetail -> Lude.Maybe [AttachedPolicy]) (\s a -> s {attachedManagedPolicies = a} :: GroupDetail)
{-# DEPRECATED gdAttachedManagedPolicies "Use generic-lens or generic-optics with 'attachedManagedPolicies' instead." #-}

instance Lude.FromXML GroupDetail where
  parseXML x =
    GroupDetail'
      Lude.<$> (x Lude..@? "Arn")
      Lude.<*> (x Lude..@? "Path")
      Lude.<*> (x Lude..@? "CreateDate")
      Lude.<*> (x Lude..@? "GroupId")
      Lude.<*> ( x Lude..@? "GroupPolicyList" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "GroupName")
      Lude.<*> ( x Lude..@? "AttachedManagedPolicies" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
