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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataMaintenanceOptionsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataMaintenanceOptionsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The maintenance options of an Amazon EC2 instance.
--
-- /See:/ 'newAwsEc2LaunchTemplateDataMaintenanceOptionsDetails' smart constructor.
data AwsEc2LaunchTemplateDataMaintenanceOptionsDetails = AwsEc2LaunchTemplateDataMaintenanceOptionsDetails'
  { -- | Disables the automatic recovery behavior of your instance or sets it to
    -- default.
    autoRecovery :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDataMaintenanceOptionsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoRecovery', 'awsEc2LaunchTemplateDataMaintenanceOptionsDetails_autoRecovery' - Disables the automatic recovery behavior of your instance or sets it to
-- default.
newAwsEc2LaunchTemplateDataMaintenanceOptionsDetails ::
  AwsEc2LaunchTemplateDataMaintenanceOptionsDetails
newAwsEc2LaunchTemplateDataMaintenanceOptionsDetails =
  AwsEc2LaunchTemplateDataMaintenanceOptionsDetails'
    { autoRecovery =
        Prelude.Nothing
    }

-- | Disables the automatic recovery behavior of your instance or sets it to
-- default.
awsEc2LaunchTemplateDataMaintenanceOptionsDetails_autoRecovery :: Lens.Lens' AwsEc2LaunchTemplateDataMaintenanceOptionsDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataMaintenanceOptionsDetails_autoRecovery = Lens.lens (\AwsEc2LaunchTemplateDataMaintenanceOptionsDetails' {autoRecovery} -> autoRecovery) (\s@AwsEc2LaunchTemplateDataMaintenanceOptionsDetails' {} a -> s {autoRecovery = a} :: AwsEc2LaunchTemplateDataMaintenanceOptionsDetails)

instance
  Data.FromJSON
    AwsEc2LaunchTemplateDataMaintenanceOptionsDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDataMaintenanceOptionsDetails"
      ( \x ->
          AwsEc2LaunchTemplateDataMaintenanceOptionsDetails'
            Prelude.<$> (x Data..:? "AutoRecovery")
      )

instance
  Prelude.Hashable
    AwsEc2LaunchTemplateDataMaintenanceOptionsDetails
  where
  hashWithSalt
    _salt
    AwsEc2LaunchTemplateDataMaintenanceOptionsDetails' {..} =
      _salt `Prelude.hashWithSalt` autoRecovery

instance
  Prelude.NFData
    AwsEc2LaunchTemplateDataMaintenanceOptionsDetails
  where
  rnf
    AwsEc2LaunchTemplateDataMaintenanceOptionsDetails' {..} =
      Prelude.rnf autoRecovery

instance
  Data.ToJSON
    AwsEc2LaunchTemplateDataMaintenanceOptionsDetails
  where
  toJSON
    AwsEc2LaunchTemplateDataMaintenanceOptionsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [("AutoRecovery" Data..=) Prelude.<$> autoRecovery]
        )
