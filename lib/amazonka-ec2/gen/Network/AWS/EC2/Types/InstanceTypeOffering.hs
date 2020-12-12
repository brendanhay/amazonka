{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceTypeOffering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceTypeOffering
  ( InstanceTypeOffering (..),

    -- * Smart constructor
    mkInstanceTypeOffering,

    -- * Lenses
    itoLocation,
    itoInstanceType,
    itoLocationType,
  )
where

import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.LocationType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The instance types offered.
--
-- /See:/ 'mkInstanceTypeOffering' smart constructor.
data InstanceTypeOffering = InstanceTypeOffering'
  { location ::
      Lude.Maybe Lude.Text,
    instanceType :: Lude.Maybe InstanceType,
    locationType :: Lude.Maybe LocationType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceTypeOffering' with the minimum fields required to make a request.
--
-- * 'instanceType' - The instance type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
-- * 'location' - The identifier for the location. This depends on the location type. For example, if the location type is @region@ , the location is the Region code (for example, @us-east-2@ .)
-- * 'locationType' - The location type.
mkInstanceTypeOffering ::
  InstanceTypeOffering
mkInstanceTypeOffering =
  InstanceTypeOffering'
    { location = Lude.Nothing,
      instanceType = Lude.Nothing,
      locationType = Lude.Nothing
    }

-- | The identifier for the location. This depends on the location type. For example, if the location type is @region@ , the location is the Region code (for example, @us-east-2@ .)
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itoLocation :: Lens.Lens' InstanceTypeOffering (Lude.Maybe Lude.Text)
itoLocation = Lens.lens (location :: InstanceTypeOffering -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: InstanceTypeOffering)
{-# DEPRECATED itoLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The instance type. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance Types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itoInstanceType :: Lens.Lens' InstanceTypeOffering (Lude.Maybe InstanceType)
itoInstanceType = Lens.lens (instanceType :: InstanceTypeOffering -> Lude.Maybe InstanceType) (\s a -> s {instanceType = a} :: InstanceTypeOffering)
{-# DEPRECATED itoInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The location type.
--
-- /Note:/ Consider using 'locationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itoLocationType :: Lens.Lens' InstanceTypeOffering (Lude.Maybe LocationType)
itoLocationType = Lens.lens (locationType :: InstanceTypeOffering -> Lude.Maybe LocationType) (\s a -> s {locationType = a} :: InstanceTypeOffering)
{-# DEPRECATED itoLocationType "Use generic-lens or generic-optics with 'locationType' instead." #-}

instance Lude.FromXML InstanceTypeOffering where
  parseXML x =
    InstanceTypeOffering'
      Lude.<$> (x Lude..@? "location")
      Lude.<*> (x Lude..@? "instanceType")
      Lude.<*> (x Lude..@? "locationType")
