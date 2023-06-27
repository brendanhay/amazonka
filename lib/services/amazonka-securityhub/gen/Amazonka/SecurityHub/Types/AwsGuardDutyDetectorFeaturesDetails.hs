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
-- Module      : Amazonka.SecurityHub.Types.AwsGuardDutyDetectorFeaturesDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsGuardDutyDetectorFeaturesDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes which features are activated for the detector.
--
-- /See:/ 'newAwsGuardDutyDetectorFeaturesDetails' smart constructor.
data AwsGuardDutyDetectorFeaturesDetails = AwsGuardDutyDetectorFeaturesDetails'
  { -- | Indicates the name of the feature that is activated for the detector.
    name :: Prelude.Maybe Prelude.Text,
    -- | Indicates the status of the feature that is activated for the detector.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsGuardDutyDetectorFeaturesDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'awsGuardDutyDetectorFeaturesDetails_name' - Indicates the name of the feature that is activated for the detector.
--
-- 'status', 'awsGuardDutyDetectorFeaturesDetails_status' - Indicates the status of the feature that is activated for the detector.
newAwsGuardDutyDetectorFeaturesDetails ::
  AwsGuardDutyDetectorFeaturesDetails
newAwsGuardDutyDetectorFeaturesDetails =
  AwsGuardDutyDetectorFeaturesDetails'
    { name =
        Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Indicates the name of the feature that is activated for the detector.
awsGuardDutyDetectorFeaturesDetails_name :: Lens.Lens' AwsGuardDutyDetectorFeaturesDetails (Prelude.Maybe Prelude.Text)
awsGuardDutyDetectorFeaturesDetails_name = Lens.lens (\AwsGuardDutyDetectorFeaturesDetails' {name} -> name) (\s@AwsGuardDutyDetectorFeaturesDetails' {} a -> s {name = a} :: AwsGuardDutyDetectorFeaturesDetails)

-- | Indicates the status of the feature that is activated for the detector.
awsGuardDutyDetectorFeaturesDetails_status :: Lens.Lens' AwsGuardDutyDetectorFeaturesDetails (Prelude.Maybe Prelude.Text)
awsGuardDutyDetectorFeaturesDetails_status = Lens.lens (\AwsGuardDutyDetectorFeaturesDetails' {status} -> status) (\s@AwsGuardDutyDetectorFeaturesDetails' {} a -> s {status = a} :: AwsGuardDutyDetectorFeaturesDetails)

instance
  Data.FromJSON
    AwsGuardDutyDetectorFeaturesDetails
  where
  parseJSON =
    Data.withObject
      "AwsGuardDutyDetectorFeaturesDetails"
      ( \x ->
          AwsGuardDutyDetectorFeaturesDetails'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Status")
      )

instance
  Prelude.Hashable
    AwsGuardDutyDetectorFeaturesDetails
  where
  hashWithSalt
    _salt
    AwsGuardDutyDetectorFeaturesDetails' {..} =
      _salt
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    AwsGuardDutyDetectorFeaturesDetails
  where
  rnf AwsGuardDutyDetectorFeaturesDetails' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf status

instance
  Data.ToJSON
    AwsGuardDutyDetectorFeaturesDetails
  where
  toJSON AwsGuardDutyDetectorFeaturesDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("Status" Data..=) Prelude.<$> status
          ]
      )
