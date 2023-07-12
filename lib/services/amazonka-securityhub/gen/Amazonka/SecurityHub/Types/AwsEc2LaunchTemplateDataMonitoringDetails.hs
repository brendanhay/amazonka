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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataMonitoringDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataMonitoringDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The monitoring for an Amazon EC2 instance.
--
-- /See:/ 'newAwsEc2LaunchTemplateDataMonitoringDetails' smart constructor.
data AwsEc2LaunchTemplateDataMonitoringDetails = AwsEc2LaunchTemplateDataMonitoringDetails'
  { -- | Enables detailed monitoring when @true@ is specified. Otherwise, basic
    -- monitoring is enabled. For more information about detailed monitoring,
    -- see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-cloudwatch-new.html Enable or turn off detailed monitoring for your instances>
    -- in the /Amazon EC2 User Guide/.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDataMonitoringDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'awsEc2LaunchTemplateDataMonitoringDetails_enabled' - Enables detailed monitoring when @true@ is specified. Otherwise, basic
-- monitoring is enabled. For more information about detailed monitoring,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-cloudwatch-new.html Enable or turn off detailed monitoring for your instances>
-- in the /Amazon EC2 User Guide/.
newAwsEc2LaunchTemplateDataMonitoringDetails ::
  AwsEc2LaunchTemplateDataMonitoringDetails
newAwsEc2LaunchTemplateDataMonitoringDetails =
  AwsEc2LaunchTemplateDataMonitoringDetails'
    { enabled =
        Prelude.Nothing
    }

-- | Enables detailed monitoring when @true@ is specified. Otherwise, basic
-- monitoring is enabled. For more information about detailed monitoring,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-cloudwatch-new.html Enable or turn off detailed monitoring for your instances>
-- in the /Amazon EC2 User Guide/.
awsEc2LaunchTemplateDataMonitoringDetails_enabled :: Lens.Lens' AwsEc2LaunchTemplateDataMonitoringDetails (Prelude.Maybe Prelude.Bool)
awsEc2LaunchTemplateDataMonitoringDetails_enabled = Lens.lens (\AwsEc2LaunchTemplateDataMonitoringDetails' {enabled} -> enabled) (\s@AwsEc2LaunchTemplateDataMonitoringDetails' {} a -> s {enabled = a} :: AwsEc2LaunchTemplateDataMonitoringDetails)

instance
  Data.FromJSON
    AwsEc2LaunchTemplateDataMonitoringDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDataMonitoringDetails"
      ( \x ->
          AwsEc2LaunchTemplateDataMonitoringDetails'
            Prelude.<$> (x Data..:? "Enabled")
      )

instance
  Prelude.Hashable
    AwsEc2LaunchTemplateDataMonitoringDetails
  where
  hashWithSalt
    _salt
    AwsEc2LaunchTemplateDataMonitoringDetails' {..} =
      _salt `Prelude.hashWithSalt` enabled

instance
  Prelude.NFData
    AwsEc2LaunchTemplateDataMonitoringDetails
  where
  rnf AwsEc2LaunchTemplateDataMonitoringDetails' {..} =
    Prelude.rnf enabled

instance
  Data.ToJSON
    AwsEc2LaunchTemplateDataMonitoringDetails
  where
  toJSON AwsEc2LaunchTemplateDataMonitoringDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Enabled" Data..=) Prelude.<$> enabled]
      )
