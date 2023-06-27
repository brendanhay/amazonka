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
-- Module      : Amazonka.NetworkManager.Types.AWSLocation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.AWSLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a location in Amazon Web Services.
--
-- /See:/ 'newAWSLocation' smart constructor.
data AWSLocation = AWSLocation'
  { -- | The Amazon Resource Name (ARN) of the subnet that the device is located
    -- in.
    subnetArn :: Prelude.Maybe Prelude.Text,
    -- | The Zone that the device is located in. Specify the ID of an
    -- Availability Zone, Local Zone, Wavelength Zone, or an Outpost.
    zone :: Prelude.Maybe Prelude.Text
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
-- 'subnetArn', 'aWSLocation_subnetArn' - The Amazon Resource Name (ARN) of the subnet that the device is located
-- in.
--
-- 'zone', 'aWSLocation_zone' - The Zone that the device is located in. Specify the ID of an
-- Availability Zone, Local Zone, Wavelength Zone, or an Outpost.
newAWSLocation ::
  AWSLocation
newAWSLocation =
  AWSLocation'
    { subnetArn = Prelude.Nothing,
      zone = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the subnet that the device is located
-- in.
aWSLocation_subnetArn :: Lens.Lens' AWSLocation (Prelude.Maybe Prelude.Text)
aWSLocation_subnetArn = Lens.lens (\AWSLocation' {subnetArn} -> subnetArn) (\s@AWSLocation' {} a -> s {subnetArn = a} :: AWSLocation)

-- | The Zone that the device is located in. Specify the ID of an
-- Availability Zone, Local Zone, Wavelength Zone, or an Outpost.
aWSLocation_zone :: Lens.Lens' AWSLocation (Prelude.Maybe Prelude.Text)
aWSLocation_zone = Lens.lens (\AWSLocation' {zone} -> zone) (\s@AWSLocation' {} a -> s {zone = a} :: AWSLocation)

instance Data.FromJSON AWSLocation where
  parseJSON =
    Data.withObject
      "AWSLocation"
      ( \x ->
          AWSLocation'
            Prelude.<$> (x Data..:? "SubnetArn")
            Prelude.<*> (x Data..:? "Zone")
      )

instance Prelude.Hashable AWSLocation where
  hashWithSalt _salt AWSLocation' {..} =
    _salt
      `Prelude.hashWithSalt` subnetArn
      `Prelude.hashWithSalt` zone

instance Prelude.NFData AWSLocation where
  rnf AWSLocation' {..} =
    Prelude.rnf subnetArn
      `Prelude.seq` Prelude.rnf zone

instance Data.ToJSON AWSLocation where
  toJSON AWSLocation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SubnetArn" Data..=) Prelude.<$> subnetArn,
            ("Zone" Data..=) Prelude.<$> zone
          ]
      )
