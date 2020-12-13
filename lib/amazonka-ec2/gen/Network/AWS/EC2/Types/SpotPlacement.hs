{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotPlacement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotPlacement
  ( SpotPlacement (..),

    -- * Smart constructor
    mkSpotPlacement,

    -- * Lenses
    spAvailabilityZone,
    spTenancy,
    spGroupName,
  )
where

import Network.AWS.EC2.Types.Tenancy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes Spot Instance placement.
--
-- /See:/ 'mkSpotPlacement' smart constructor.
data SpotPlacement = SpotPlacement'
  { -- | The Availability Zone.
    --
    -- [Spot Fleet only] To specify multiple Availability Zones, separate them using commas; for example, "us-west-2a, us-west-2b".
    availabilityZone :: Lude.Maybe Lude.Text,
    -- | The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of @dedicated@ runs on single-tenant hardware. The @host@ tenancy is not supported for Spot Instances.
    tenancy :: Lude.Maybe Tenancy,
    -- | The name of the placement group.
    groupName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SpotPlacement' with the minimum fields required to make a request.
--
-- * 'availabilityZone' - The Availability Zone.
--
-- [Spot Fleet only] To specify multiple Availability Zones, separate them using commas; for example, "us-west-2a, us-west-2b".
-- * 'tenancy' - The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of @dedicated@ runs on single-tenant hardware. The @host@ tenancy is not supported for Spot Instances.
-- * 'groupName' - The name of the placement group.
mkSpotPlacement ::
  SpotPlacement
mkSpotPlacement =
  SpotPlacement'
    { availabilityZone = Lude.Nothing,
      tenancy = Lude.Nothing,
      groupName = Lude.Nothing
    }

-- | The Availability Zone.
--
-- [Spot Fleet only] To specify multiple Availability Zones, separate them using commas; for example, "us-west-2a, us-west-2b".
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spAvailabilityZone :: Lens.Lens' SpotPlacement (Lude.Maybe Lude.Text)
spAvailabilityZone = Lens.lens (availabilityZone :: SpotPlacement -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: SpotPlacement)
{-# DEPRECATED spAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The tenancy of the instance (if the instance is running in a VPC). An instance with a tenancy of @dedicated@ runs on single-tenant hardware. The @host@ tenancy is not supported for Spot Instances.
--
-- /Note:/ Consider using 'tenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spTenancy :: Lens.Lens' SpotPlacement (Lude.Maybe Tenancy)
spTenancy = Lens.lens (tenancy :: SpotPlacement -> Lude.Maybe Tenancy) (\s a -> s {tenancy = a} :: SpotPlacement)
{-# DEPRECATED spTenancy "Use generic-lens or generic-optics with 'tenancy' instead." #-}

-- | The name of the placement group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spGroupName :: Lens.Lens' SpotPlacement (Lude.Maybe Lude.Text)
spGroupName = Lens.lens (groupName :: SpotPlacement -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: SpotPlacement)
{-# DEPRECATED spGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.FromXML SpotPlacement where
  parseXML x =
    SpotPlacement'
      Lude.<$> (x Lude..@? "availabilityZone")
      Lude.<*> (x Lude..@? "tenancy")
      Lude.<*> (x Lude..@? "groupName")

instance Lude.ToQuery SpotPlacement where
  toQuery SpotPlacement' {..} =
    Lude.mconcat
      [ "AvailabilityZone" Lude.=: availabilityZone,
        "Tenancy" Lude.=: tenancy,
        "GroupName" Lude.=: groupName
      ]
