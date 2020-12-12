{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.AvailabilityZoneDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.AvailabilityZoneDetail
  ( AvailabilityZoneDetail (..),

    -- * Smart constructor
    mkAvailabilityZoneDetail,

    -- * Lenses
    azdName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A list of Availability Zones corresponding to the segments in a trace.
--
-- /See:/ 'mkAvailabilityZoneDetail' smart constructor.
newtype AvailabilityZoneDetail = AvailabilityZoneDetail'
  { name ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AvailabilityZoneDetail' with the minimum fields required to make a request.
--
-- * 'name' - The name of a corresponding Availability Zone.
mkAvailabilityZoneDetail ::
  AvailabilityZoneDetail
mkAvailabilityZoneDetail =
  AvailabilityZoneDetail' {name = Lude.Nothing}

-- | The name of a corresponding Availability Zone.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azdName :: Lens.Lens' AvailabilityZoneDetail (Lude.Maybe Lude.Text)
azdName = Lens.lens (name :: AvailabilityZoneDetail -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: AvailabilityZoneDetail)
{-# DEPRECATED azdName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON AvailabilityZoneDetail where
  parseJSON =
    Lude.withObject
      "AvailabilityZoneDetail"
      (\x -> AvailabilityZoneDetail' Lude.<$> (x Lude..:? "Name"))
