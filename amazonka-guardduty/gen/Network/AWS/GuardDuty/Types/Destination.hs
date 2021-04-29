{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Destination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Destination where

import Network.AWS.GuardDuty.Types.DestinationType
import Network.AWS.GuardDuty.Types.PublishingStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the publishing destination, including the ID,
-- type, and status.
--
-- /See:/ 'newDestination' smart constructor.
data Destination = Destination'
  { -- | The unique ID of the publishing destination.
    destinationId :: Prelude.Text,
    -- | The type of resource used for the publishing destination. Currently,
    -- only Amazon S3 buckets are supported.
    destinationType :: DestinationType,
    -- | The status of the publishing destination.
    status :: PublishingStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Destination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationId', 'destination_destinationId' - The unique ID of the publishing destination.
--
-- 'destinationType', 'destination_destinationType' - The type of resource used for the publishing destination. Currently,
-- only Amazon S3 buckets are supported.
--
-- 'status', 'destination_status' - The status of the publishing destination.
newDestination ::
  -- | 'destinationId'
  Prelude.Text ->
  -- | 'destinationType'
  DestinationType ->
  -- | 'status'
  PublishingStatus ->
  Destination
newDestination
  pDestinationId_
  pDestinationType_
  pStatus_ =
    Destination'
      { destinationId = pDestinationId_,
        destinationType = pDestinationType_,
        status = pStatus_
      }

-- | The unique ID of the publishing destination.
destination_destinationId :: Lens.Lens' Destination Prelude.Text
destination_destinationId = Lens.lens (\Destination' {destinationId} -> destinationId) (\s@Destination' {} a -> s {destinationId = a} :: Destination)

-- | The type of resource used for the publishing destination. Currently,
-- only Amazon S3 buckets are supported.
destination_destinationType :: Lens.Lens' Destination DestinationType
destination_destinationType = Lens.lens (\Destination' {destinationType} -> destinationType) (\s@Destination' {} a -> s {destinationType = a} :: Destination)

-- | The status of the publishing destination.
destination_status :: Lens.Lens' Destination PublishingStatus
destination_status = Lens.lens (\Destination' {status} -> status) (\s@Destination' {} a -> s {status = a} :: Destination)

instance Prelude.FromJSON Destination where
  parseJSON =
    Prelude.withObject
      "Destination"
      ( \x ->
          Destination'
            Prelude.<$> (x Prelude..: "destinationId")
            Prelude.<*> (x Prelude..: "destinationType")
            Prelude.<*> (x Prelude..: "status")
      )

instance Prelude.Hashable Destination

instance Prelude.NFData Destination
