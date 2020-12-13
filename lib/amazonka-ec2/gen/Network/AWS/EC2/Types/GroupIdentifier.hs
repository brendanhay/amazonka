{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.GroupIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.GroupIdentifier
  ( GroupIdentifier (..),

    -- * Smart constructor
    mkGroupIdentifier,

    -- * Lenses
    giGroupId,
    giGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a security group.
--
-- /See:/ 'mkGroupIdentifier' smart constructor.
data GroupIdentifier = GroupIdentifier'
  { -- | The ID of the security group.
    groupId :: Lude.Maybe Lude.Text,
    -- | The name of the security group.
    groupName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GroupIdentifier' with the minimum fields required to make a request.
--
-- * 'groupId' - The ID of the security group.
-- * 'groupName' - The name of the security group.
mkGroupIdentifier ::
  GroupIdentifier
mkGroupIdentifier =
  GroupIdentifier'
    { groupId = Lude.Nothing,
      groupName = Lude.Nothing
    }

-- | The ID of the security group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giGroupId :: Lens.Lens' GroupIdentifier (Lude.Maybe Lude.Text)
giGroupId = Lens.lens (groupId :: GroupIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {groupId = a} :: GroupIdentifier)
{-# DEPRECATED giGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The name of the security group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giGroupName :: Lens.Lens' GroupIdentifier (Lude.Maybe Lude.Text)
giGroupName = Lens.lens (groupName :: GroupIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: GroupIdentifier)
{-# DEPRECATED giGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.FromXML GroupIdentifier where
  parseXML x =
    GroupIdentifier'
      Lude.<$> (x Lude..@? "groupId") Lude.<*> (x Lude..@? "groupName")

instance Lude.ToQuery GroupIdentifier where
  toQuery GroupIdentifier' {..} =
    Lude.mconcat
      ["GroupId" Lude.=: groupId, "GroupName" Lude.=: groupName]
