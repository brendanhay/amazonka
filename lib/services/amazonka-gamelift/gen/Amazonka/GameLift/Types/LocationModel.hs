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
-- Module      : Amazonka.GameLift.Types.LocationModel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.LocationModel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Properties of a location
--
-- /See:/ 'newLocationModel' smart constructor.
data LocationModel = LocationModel'
  { -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- that is assigned to a Amazon GameLift location resource and uniquely
    -- identifies it. ARNs are unique across all Regions. Format is
    -- @arn:aws:gamelift:\<region>::location\/location-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
    locationArn :: Prelude.Maybe Prelude.Text,
    -- | The location\'s name.
    locationName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LocationModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locationArn', 'locationModel_locationArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a Amazon GameLift location resource and uniquely
-- identifies it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::location\/location-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
--
-- 'locationName', 'locationModel_locationName' - The location\'s name.
newLocationModel ::
  LocationModel
newLocationModel =
  LocationModel'
    { locationArn = Prelude.Nothing,
      locationName = Prelude.Nothing
    }

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a Amazon GameLift location resource and uniquely
-- identifies it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::location\/location-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
locationModel_locationArn :: Lens.Lens' LocationModel (Prelude.Maybe Prelude.Text)
locationModel_locationArn = Lens.lens (\LocationModel' {locationArn} -> locationArn) (\s@LocationModel' {} a -> s {locationArn = a} :: LocationModel)

-- | The location\'s name.
locationModel_locationName :: Lens.Lens' LocationModel (Prelude.Maybe Prelude.Text)
locationModel_locationName = Lens.lens (\LocationModel' {locationName} -> locationName) (\s@LocationModel' {} a -> s {locationName = a} :: LocationModel)

instance Data.FromJSON LocationModel where
  parseJSON =
    Data.withObject
      "LocationModel"
      ( \x ->
          LocationModel'
            Prelude.<$> (x Data..:? "LocationArn")
            Prelude.<*> (x Data..:? "LocationName")
      )

instance Prelude.Hashable LocationModel where
  hashWithSalt _salt LocationModel' {..} =
    _salt
      `Prelude.hashWithSalt` locationArn
      `Prelude.hashWithSalt` locationName

instance Prelude.NFData LocationModel where
  rnf LocationModel' {..} =
    Prelude.rnf locationArn
      `Prelude.seq` Prelude.rnf locationName
