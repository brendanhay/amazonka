{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Destination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Destination
  ( Destination (..),

    -- * Smart constructor
    mkDestination,

    -- * Lenses
    dStatus,
    dDestinationType,
    dDestinationId,
  )
where

import Network.AWS.GuardDuty.Types.DestinationType
import Network.AWS.GuardDuty.Types.PublishingStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the publishing destination, including the ID, type, and status.
--
-- /See:/ 'mkDestination' smart constructor.
data Destination = Destination'
  { -- | The status of the publishing destination.
    status :: PublishingStatus,
    -- | The type of resource used for the publishing destination. Currently, only Amazon S3 buckets are supported.
    destinationType :: DestinationType,
    -- | The unique ID of the publishing destination.
    destinationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Destination' with the minimum fields required to make a request.
--
-- * 'status' - The status of the publishing destination.
-- * 'destinationType' - The type of resource used for the publishing destination. Currently, only Amazon S3 buckets are supported.
-- * 'destinationId' - The unique ID of the publishing destination.
mkDestination ::
  -- | 'status'
  PublishingStatus ->
  -- | 'destinationType'
  DestinationType ->
  -- | 'destinationId'
  Lude.Text ->
  Destination
mkDestination pStatus_ pDestinationType_ pDestinationId_ =
  Destination'
    { status = pStatus_,
      destinationType = pDestinationType_,
      destinationId = pDestinationId_
    }

-- | The status of the publishing destination.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStatus :: Lens.Lens' Destination PublishingStatus
dStatus = Lens.lens (status :: Destination -> PublishingStatus) (\s a -> s {status = a} :: Destination)
{-# DEPRECATED dStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The type of resource used for the publishing destination. Currently, only Amazon S3 buckets are supported.
--
-- /Note:/ Consider using 'destinationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDestinationType :: Lens.Lens' Destination DestinationType
dDestinationType = Lens.lens (destinationType :: Destination -> DestinationType) (\s a -> s {destinationType = a} :: Destination)
{-# DEPRECATED dDestinationType "Use generic-lens or generic-optics with 'destinationType' instead." #-}

-- | The unique ID of the publishing destination.
--
-- /Note:/ Consider using 'destinationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDestinationId :: Lens.Lens' Destination Lude.Text
dDestinationId = Lens.lens (destinationId :: Destination -> Lude.Text) (\s a -> s {destinationId = a} :: Destination)
{-# DEPRECATED dDestinationId "Use generic-lens or generic-optics with 'destinationId' instead." #-}

instance Lude.FromJSON Destination where
  parseJSON =
    Lude.withObject
      "Destination"
      ( \x ->
          Destination'
            Lude.<$> (x Lude..: "status")
            Lude.<*> (x Lude..: "destinationType")
            Lude.<*> (x Lude..: "destinationId")
      )
