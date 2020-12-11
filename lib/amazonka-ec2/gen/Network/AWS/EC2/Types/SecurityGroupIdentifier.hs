-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SecurityGroupIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SecurityGroupIdentifier
  ( SecurityGroupIdentifier (..),

    -- * Smart constructor
    mkSecurityGroupIdentifier,

    -- * Lenses
    sgiGroupId,
    sgiGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a security group.
--
-- /See:/ 'mkSecurityGroupIdentifier' smart constructor.
data SecurityGroupIdentifier = SecurityGroupIdentifier'
  { groupId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'SecurityGroupIdentifier' with the minimum fields required to make a request.
--
-- * 'groupId' - The ID of the security group.
-- * 'groupName' - The name of the security group.
mkSecurityGroupIdentifier ::
  SecurityGroupIdentifier
mkSecurityGroupIdentifier =
  SecurityGroupIdentifier'
    { groupId = Lude.Nothing,
      groupName = Lude.Nothing
    }

-- | The ID of the security group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgiGroupId :: Lens.Lens' SecurityGroupIdentifier (Lude.Maybe Lude.Text)
sgiGroupId = Lens.lens (groupId :: SecurityGroupIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {groupId = a} :: SecurityGroupIdentifier)
{-# DEPRECATED sgiGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The name of the security group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgiGroupName :: Lens.Lens' SecurityGroupIdentifier (Lude.Maybe Lude.Text)
sgiGroupName = Lens.lens (groupName :: SecurityGroupIdentifier -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: SecurityGroupIdentifier)
{-# DEPRECATED sgiGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.FromXML SecurityGroupIdentifier where
  parseXML x =
    SecurityGroupIdentifier'
      Lude.<$> (x Lude..@? "groupId") Lude.<*> (x Lude..@? "groupName")
