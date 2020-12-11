-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspacesIPGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspacesIPGroup
  ( WorkspacesIPGroup (..),

    -- * Smart constructor
    mkWorkspacesIPGroup,

    -- * Lenses
    wigGroupDesc,
    wigUserRules,
    wigGroupId,
    wigGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkSpaces.Types.IPRuleItem

-- | Describes an IP access control group.
--
-- /See:/ 'mkWorkspacesIPGroup' smart constructor.
data WorkspacesIPGroup = WorkspacesIPGroup'
  { groupDesc ::
      Lude.Maybe Lude.Text,
    userRules :: Lude.Maybe [IPRuleItem],
    groupId :: Lude.Maybe Lude.Text,
    groupName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WorkspacesIPGroup' with the minimum fields required to make a request.
--
-- * 'groupDesc' - The description of the group.
-- * 'groupId' - The identifier of the group.
-- * 'groupName' - The name of the group.
-- * 'userRules' - The rules.
mkWorkspacesIPGroup ::
  WorkspacesIPGroup
mkWorkspacesIPGroup =
  WorkspacesIPGroup'
    { groupDesc = Lude.Nothing,
      userRules = Lude.Nothing,
      groupId = Lude.Nothing,
      groupName = Lude.Nothing
    }

-- | The description of the group.
--
-- /Note:/ Consider using 'groupDesc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wigGroupDesc :: Lens.Lens' WorkspacesIPGroup (Lude.Maybe Lude.Text)
wigGroupDesc = Lens.lens (groupDesc :: WorkspacesIPGroup -> Lude.Maybe Lude.Text) (\s a -> s {groupDesc = a} :: WorkspacesIPGroup)
{-# DEPRECATED wigGroupDesc "Use generic-lens or generic-optics with 'groupDesc' instead." #-}

-- | The rules.
--
-- /Note:/ Consider using 'userRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wigUserRules :: Lens.Lens' WorkspacesIPGroup (Lude.Maybe [IPRuleItem])
wigUserRules = Lens.lens (userRules :: WorkspacesIPGroup -> Lude.Maybe [IPRuleItem]) (\s a -> s {userRules = a} :: WorkspacesIPGroup)
{-# DEPRECATED wigUserRules "Use generic-lens or generic-optics with 'userRules' instead." #-}

-- | The identifier of the group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wigGroupId :: Lens.Lens' WorkspacesIPGroup (Lude.Maybe Lude.Text)
wigGroupId = Lens.lens (groupId :: WorkspacesIPGroup -> Lude.Maybe Lude.Text) (\s a -> s {groupId = a} :: WorkspacesIPGroup)
{-# DEPRECATED wigGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The name of the group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wigGroupName :: Lens.Lens' WorkspacesIPGroup (Lude.Maybe Lude.Text)
wigGroupName = Lens.lens (groupName :: WorkspacesIPGroup -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: WorkspacesIPGroup)
{-# DEPRECATED wigGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.FromJSON WorkspacesIPGroup where
  parseJSON =
    Lude.withObject
      "WorkspacesIPGroup"
      ( \x ->
          WorkspacesIPGroup'
            Lude.<$> (x Lude..:? "groupDesc")
            Lude.<*> (x Lude..:? "userRules" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "groupId")
            Lude.<*> (x Lude..:? "groupName")
      )
