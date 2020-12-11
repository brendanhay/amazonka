-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.SecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.SecurityGroup
  ( SecurityGroup (..),

    -- * Smart constructor
    mkSecurityGroup,

    -- * Lenses
    sgGroupId,
    sgGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a security group associated with a network interface. This data type is used as one of the elements of the 'NetworkInterface' data type.
--
-- /See:/ 'mkSecurityGroup' smart constructor.
data SecurityGroup = SecurityGroup'
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

-- | Creates a value of 'SecurityGroup' with the minimum fields required to make a request.
--
-- * 'groupId' - The ID of the security group.
-- * 'groupName' - The name of the security group.
mkSecurityGroup ::
  SecurityGroup
mkSecurityGroup =
  SecurityGroup' {groupId = Lude.Nothing, groupName = Lude.Nothing}

-- | The ID of the security group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgGroupId :: Lens.Lens' SecurityGroup (Lude.Maybe Lude.Text)
sgGroupId = Lens.lens (groupId :: SecurityGroup -> Lude.Maybe Lude.Text) (\s a -> s {groupId = a} :: SecurityGroup)
{-# DEPRECATED sgGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The name of the security group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgGroupName :: Lens.Lens' SecurityGroup (Lude.Maybe Lude.Text)
sgGroupName = Lens.lens (groupName :: SecurityGroup -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: SecurityGroup)
{-# DEPRECATED sgGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.FromJSON SecurityGroup where
  parseJSON =
    Lude.withObject
      "SecurityGroup"
      ( \x ->
          SecurityGroup'
            Lude.<$> (x Lude..:? "groupId") Lude.<*> (x Lude..:? "groupName")
      )
