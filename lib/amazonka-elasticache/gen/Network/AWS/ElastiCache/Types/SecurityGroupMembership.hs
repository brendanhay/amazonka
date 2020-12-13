{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.SecurityGroupMembership
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.SecurityGroupMembership
  ( SecurityGroupMembership (..),

    -- * Smart constructor
    mkSecurityGroupMembership,

    -- * Lenses
    sgmStatus,
    sgmSecurityGroupId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a single cache security group and its status.
--
-- /See:/ 'mkSecurityGroupMembership' smart constructor.
data SecurityGroupMembership = SecurityGroupMembership'
  { -- | The status of the cache security group membership. The status changes whenever a cache security group is modified, or when the cache security groups assigned to a cluster are modified.
    status :: Lude.Maybe Lude.Text,
    -- | The identifier of the cache security group.
    securityGroupId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SecurityGroupMembership' with the minimum fields required to make a request.
--
-- * 'status' - The status of the cache security group membership. The status changes whenever a cache security group is modified, or when the cache security groups assigned to a cluster are modified.
-- * 'securityGroupId' - The identifier of the cache security group.
mkSecurityGroupMembership ::
  SecurityGroupMembership
mkSecurityGroupMembership =
  SecurityGroupMembership'
    { status = Lude.Nothing,
      securityGroupId = Lude.Nothing
    }

-- | The status of the cache security group membership. The status changes whenever a cache security group is modified, or when the cache security groups assigned to a cluster are modified.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgmStatus :: Lens.Lens' SecurityGroupMembership (Lude.Maybe Lude.Text)
sgmStatus = Lens.lens (status :: SecurityGroupMembership -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: SecurityGroupMembership)
{-# DEPRECATED sgmStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The identifier of the cache security group.
--
-- /Note:/ Consider using 'securityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgmSecurityGroupId :: Lens.Lens' SecurityGroupMembership (Lude.Maybe Lude.Text)
sgmSecurityGroupId = Lens.lens (securityGroupId :: SecurityGroupMembership -> Lude.Maybe Lude.Text) (\s a -> s {securityGroupId = a} :: SecurityGroupMembership)
{-# DEPRECATED sgmSecurityGroupId "Use generic-lens or generic-optics with 'securityGroupId' instead." #-}

instance Lude.FromXML SecurityGroupMembership where
  parseXML x =
    SecurityGroupMembership'
      Lude.<$> (x Lude..@? "Status") Lude.<*> (x Lude..@? "SecurityGroupId")
