-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.AvailabilityZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.AvailabilityZone
  ( AvailabilityZone (..),

    -- * Smart constructor
    mkAvailabilityZone,

    -- * Lenses
    azState,
    azZoneName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an Availability Zone.
--
-- /See:/ 'mkAvailabilityZone' smart constructor.
data AvailabilityZone = AvailabilityZone'
  { state ::
      Lude.Maybe Lude.Text,
    zoneName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AvailabilityZone' with the minimum fields required to make a request.
--
-- * 'state' - The state of the Availability Zone.
-- * 'zoneName' - The name of the Availability Zone. The format is @us-east-2a@ (case-sensitive).
mkAvailabilityZone ::
  AvailabilityZone
mkAvailabilityZone =
  AvailabilityZone' {state = Lude.Nothing, zoneName = Lude.Nothing}

-- | The state of the Availability Zone.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azState :: Lens.Lens' AvailabilityZone (Lude.Maybe Lude.Text)
azState = Lens.lens (state :: AvailabilityZone -> Lude.Maybe Lude.Text) (\s a -> s {state = a} :: AvailabilityZone)
{-# DEPRECATED azState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The name of the Availability Zone. The format is @us-east-2a@ (case-sensitive).
--
-- /Note:/ Consider using 'zoneName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azZoneName :: Lens.Lens' AvailabilityZone (Lude.Maybe Lude.Text)
azZoneName = Lens.lens (zoneName :: AvailabilityZone -> Lude.Maybe Lude.Text) (\s a -> s {zoneName = a} :: AvailabilityZone)
{-# DEPRECATED azZoneName "Use generic-lens or generic-optics with 'zoneName' instead." #-}

instance Lude.FromJSON AvailabilityZone where
  parseJSON =
    Lude.withObject
      "AvailabilityZone"
      ( \x ->
          AvailabilityZone'
            Lude.<$> (x Lude..:? "state") Lude.<*> (x Lude..:? "zoneName")
      )
