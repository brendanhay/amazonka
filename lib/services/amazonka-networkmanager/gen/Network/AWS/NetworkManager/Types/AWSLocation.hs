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
-- Module      : Network.AWS.NetworkManager.Types.AWSLocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.NetworkManager.Types.AWSLocation where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies a location in AWS.
--
-- /See:/ 'newAWSLocation' smart constructor.
data AWSLocation = AWSLocation'
  { -- | The Zone the device is located in. This can be the ID of an Availability
    -- Zone, Local Zone, Wavelength Zone, or an Outpost.
    zone :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the subnet the device is located in.
    subnetArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AWSLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'zone', 'aWSLocation_zone' - The Zone the device is located in. This can be the ID of an Availability
-- Zone, Local Zone, Wavelength Zone, or an Outpost.
--
-- 'subnetArn', 'aWSLocation_subnetArn' - The Amazon Resource Name (ARN) of the subnet the device is located in.
newAWSLocation ::
  AWSLocation
newAWSLocation =
  AWSLocation'
    { zone = Prelude.Nothing,
      subnetArn = Prelude.Nothing
    }

-- | The Zone the device is located in. This can be the ID of an Availability
-- Zone, Local Zone, Wavelength Zone, or an Outpost.
aWSLocation_zone :: Lens.Lens' AWSLocation (Prelude.Maybe Prelude.Text)
aWSLocation_zone = Lens.lens (\AWSLocation' {zone} -> zone) (\s@AWSLocation' {} a -> s {zone = a} :: AWSLocation)

-- | The Amazon Resource Name (ARN) of the subnet the device is located in.
aWSLocation_subnetArn :: Lens.Lens' AWSLocation (Prelude.Maybe Prelude.Text)
aWSLocation_subnetArn = Lens.lens (\AWSLocation' {subnetArn} -> subnetArn) (\s@AWSLocation' {} a -> s {subnetArn = a} :: AWSLocation)

instance Core.FromJSON AWSLocation where
  parseJSON =
    Core.withObject
      "AWSLocation"
      ( \x ->
          AWSLocation'
            Prelude.<$> (x Core..:? "Zone")
            Prelude.<*> (x Core..:? "SubnetArn")
      )

instance Prelude.Hashable AWSLocation

instance Prelude.NFData AWSLocation

instance Core.ToJSON AWSLocation where
  toJSON AWSLocation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Zone" Core..=) Prelude.<$> zone,
            ("SubnetArn" Core..=) Prelude.<$> subnetArn
          ]
      )
