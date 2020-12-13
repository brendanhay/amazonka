{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PolicyGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PolicyGroup
  ( PolicyGroup (..),

    -- * Smart constructor
    mkPolicyGroup,

    -- * Lenses
    pgGroupId,
    pgGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a group that a managed policy is attached to.
--
-- This data type is used as a response element in the 'ListEntitiesForPolicy' operation.
-- For more information about managed policies, refer to <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
-- /See:/ 'mkPolicyGroup' smart constructor.
data PolicyGroup = PolicyGroup'
  { -- | The stable and unique string identifying the group. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the /IAM User Guide/ .
    groupId :: Lude.Maybe Lude.Text,
    -- | The name (friendly name, not ARN) identifying the group.
    groupName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PolicyGroup' with the minimum fields required to make a request.
--
-- * 'groupId' - The stable and unique string identifying the group. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the /IAM User Guide/ .
-- * 'groupName' - The name (friendly name, not ARN) identifying the group.
mkPolicyGroup ::
  PolicyGroup
mkPolicyGroup =
  PolicyGroup' {groupId = Lude.Nothing, groupName = Lude.Nothing}

-- | The stable and unique string identifying the group. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgGroupId :: Lens.Lens' PolicyGroup (Lude.Maybe Lude.Text)
pgGroupId = Lens.lens (groupId :: PolicyGroup -> Lude.Maybe Lude.Text) (\s a -> s {groupId = a} :: PolicyGroup)
{-# DEPRECATED pgGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The name (friendly name, not ARN) identifying the group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgGroupName :: Lens.Lens' PolicyGroup (Lude.Maybe Lude.Text)
pgGroupName = Lens.lens (groupName :: PolicyGroup -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: PolicyGroup)
{-# DEPRECATED pgGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.FromXML PolicyGroup where
  parseXML x =
    PolicyGroup'
      Lude.<$> (x Lude..@? "GroupId") Lude.<*> (x Lude..@? "GroupName")
