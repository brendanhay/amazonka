{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.PhysicalConnectionRequirements
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.PhysicalConnectionRequirements
  ( PhysicalConnectionRequirements (..),

    -- * Smart constructor
    mkPhysicalConnectionRequirements,

    -- * Lenses
    pcrSecurityGroupIdList,
    pcrSubnetId,
    pcrAvailabilityZone,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the physical requirements for a connection.
--
-- /See:/ 'mkPhysicalConnectionRequirements' smart constructor.
data PhysicalConnectionRequirements = PhysicalConnectionRequirements'
  { securityGroupIdList ::
      Lude.Maybe [Lude.Text],
    subnetId ::
      Lude.Maybe Lude.Text,
    availabilityZone ::
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

-- | Creates a value of 'PhysicalConnectionRequirements' with the minimum fields required to make a request.
--
-- * 'availabilityZone' - The connection's Availability Zone. This field is redundant because the specified subnet implies the Availability Zone to be used. Currently the field must be populated, but it will be deprecated in the future.
-- * 'securityGroupIdList' - The security group ID list used by the connection.
-- * 'subnetId' - The subnet ID used by the connection.
mkPhysicalConnectionRequirements ::
  PhysicalConnectionRequirements
mkPhysicalConnectionRequirements =
  PhysicalConnectionRequirements'
    { securityGroupIdList =
        Lude.Nothing,
      subnetId = Lude.Nothing,
      availabilityZone = Lude.Nothing
    }

-- | The security group ID list used by the connection.
--
-- /Note:/ Consider using 'securityGroupIdList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrSecurityGroupIdList :: Lens.Lens' PhysicalConnectionRequirements (Lude.Maybe [Lude.Text])
pcrSecurityGroupIdList = Lens.lens (securityGroupIdList :: PhysicalConnectionRequirements -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroupIdList = a} :: PhysicalConnectionRequirements)
{-# DEPRECATED pcrSecurityGroupIdList "Use generic-lens or generic-optics with 'securityGroupIdList' instead." #-}

-- | The subnet ID used by the connection.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrSubnetId :: Lens.Lens' PhysicalConnectionRequirements (Lude.Maybe Lude.Text)
pcrSubnetId = Lens.lens (subnetId :: PhysicalConnectionRequirements -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: PhysicalConnectionRequirements)
{-# DEPRECATED pcrSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The connection's Availability Zone. This field is redundant because the specified subnet implies the Availability Zone to be used. Currently the field must be populated, but it will be deprecated in the future.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcrAvailabilityZone :: Lens.Lens' PhysicalConnectionRequirements (Lude.Maybe Lude.Text)
pcrAvailabilityZone = Lens.lens (availabilityZone :: PhysicalConnectionRequirements -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: PhysicalConnectionRequirements)
{-# DEPRECATED pcrAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

instance Lude.FromJSON PhysicalConnectionRequirements where
  parseJSON =
    Lude.withObject
      "PhysicalConnectionRequirements"
      ( \x ->
          PhysicalConnectionRequirements'
            Lude.<$> (x Lude..:? "SecurityGroupIdList" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "SubnetId")
            Lude.<*> (x Lude..:? "AvailabilityZone")
      )

instance Lude.ToJSON PhysicalConnectionRequirements where
  toJSON PhysicalConnectionRequirements' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SecurityGroupIdList" Lude..=) Lude.<$> securityGroupIdList,
            ("SubnetId" Lude..=) Lude.<$> subnetId,
            ("AvailabilityZone" Lude..=) Lude.<$> availabilityZone
          ]
      )
