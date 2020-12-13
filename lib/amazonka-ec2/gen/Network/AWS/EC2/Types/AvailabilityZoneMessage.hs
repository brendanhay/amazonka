{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AvailabilityZoneMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AvailabilityZoneMessage
  ( AvailabilityZoneMessage (..),

    -- * Smart constructor
    mkAvailabilityZoneMessage,

    -- * Lenses
    azmMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a message about an Availability Zone, Local Zone, or Wavelength Zone.
--
-- /See:/ 'mkAvailabilityZoneMessage' smart constructor.
newtype AvailabilityZoneMessage = AvailabilityZoneMessage'
  { -- | The message about the Availability Zone, Local Zone, or Wavelength Zone.
    message :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AvailabilityZoneMessage' with the minimum fields required to make a request.
--
-- * 'message' - The message about the Availability Zone, Local Zone, or Wavelength Zone.
mkAvailabilityZoneMessage ::
  AvailabilityZoneMessage
mkAvailabilityZoneMessage =
  AvailabilityZoneMessage' {message = Lude.Nothing}

-- | The message about the Availability Zone, Local Zone, or Wavelength Zone.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azmMessage :: Lens.Lens' AvailabilityZoneMessage (Lude.Maybe Lude.Text)
azmMessage = Lens.lens (message :: AvailabilityZoneMessage -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: AvailabilityZoneMessage)
{-# DEPRECATED azmMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromXML AvailabilityZoneMessage where
  parseXML x =
    AvailabilityZoneMessage' Lude.<$> (x Lude..@? "message")
