-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.SecurityGroupMembership
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.SecurityGroupMembership
  ( SecurityGroupMembership (..),

    -- * Smart constructor
    mkSecurityGroupMembership,

    -- * Lenses
    sgmStatus,
    sgmSecurityGroupIdentifier,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An individual VPC security group and its status.
--
-- /See:/ 'mkSecurityGroupMembership' smart constructor.
data SecurityGroupMembership = SecurityGroupMembership'
  { status ::
      Lude.Maybe Lude.Text,
    securityGroupIdentifier ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SecurityGroupMembership' with the minimum fields required to make a request.
--
-- * 'securityGroupIdentifier' - The unique ID for this security group.
-- * 'status' - The status of this security group.
mkSecurityGroupMembership ::
  SecurityGroupMembership
mkSecurityGroupMembership =
  SecurityGroupMembership'
    { status = Lude.Nothing,
      securityGroupIdentifier = Lude.Nothing
    }

-- | The status of this security group.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgmStatus :: Lens.Lens' SecurityGroupMembership (Lude.Maybe Lude.Text)
sgmStatus = Lens.lens (status :: SecurityGroupMembership -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: SecurityGroupMembership)
{-# DEPRECATED sgmStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The unique ID for this security group.
--
-- /Note:/ Consider using 'securityGroupIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgmSecurityGroupIdentifier :: Lens.Lens' SecurityGroupMembership (Lude.Maybe Lude.Text)
sgmSecurityGroupIdentifier = Lens.lens (securityGroupIdentifier :: SecurityGroupMembership -> Lude.Maybe Lude.Text) (\s a -> s {securityGroupIdentifier = a} :: SecurityGroupMembership)
{-# DEPRECATED sgmSecurityGroupIdentifier "Use generic-lens or generic-optics with 'securityGroupIdentifier' instead." #-}

instance Lude.FromJSON SecurityGroupMembership where
  parseJSON =
    Lude.withObject
      "SecurityGroupMembership"
      ( \x ->
          SecurityGroupMembership'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "SecurityGroupIdentifier")
      )
