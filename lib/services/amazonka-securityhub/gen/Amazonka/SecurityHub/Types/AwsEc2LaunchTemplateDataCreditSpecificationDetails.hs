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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataCreditSpecificationDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataCreditSpecificationDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the credit option for CPU usage of a T2, T3, or T3a Amazon EC2
-- instance.
--
-- /See:/ 'newAwsEc2LaunchTemplateDataCreditSpecificationDetails' smart constructor.
data AwsEc2LaunchTemplateDataCreditSpecificationDetails = AwsEc2LaunchTemplateDataCreditSpecificationDetails'
  { -- | The credit option for CPU usage of a T instance.
    cpuCredits :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDataCreditSpecificationDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cpuCredits', 'awsEc2LaunchTemplateDataCreditSpecificationDetails_cpuCredits' - The credit option for CPU usage of a T instance.
newAwsEc2LaunchTemplateDataCreditSpecificationDetails ::
  AwsEc2LaunchTemplateDataCreditSpecificationDetails
newAwsEc2LaunchTemplateDataCreditSpecificationDetails =
  AwsEc2LaunchTemplateDataCreditSpecificationDetails'
    { cpuCredits =
        Prelude.Nothing
    }

-- | The credit option for CPU usage of a T instance.
awsEc2LaunchTemplateDataCreditSpecificationDetails_cpuCredits :: Lens.Lens' AwsEc2LaunchTemplateDataCreditSpecificationDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataCreditSpecificationDetails_cpuCredits = Lens.lens (\AwsEc2LaunchTemplateDataCreditSpecificationDetails' {cpuCredits} -> cpuCredits) (\s@AwsEc2LaunchTemplateDataCreditSpecificationDetails' {} a -> s {cpuCredits = a} :: AwsEc2LaunchTemplateDataCreditSpecificationDetails)

instance
  Data.FromJSON
    AwsEc2LaunchTemplateDataCreditSpecificationDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDataCreditSpecificationDetails"
      ( \x ->
          AwsEc2LaunchTemplateDataCreditSpecificationDetails'
            Prelude.<$> (x Data..:? "CpuCredits")
      )

instance
  Prelude.Hashable
    AwsEc2LaunchTemplateDataCreditSpecificationDetails
  where
  hashWithSalt
    _salt
    AwsEc2LaunchTemplateDataCreditSpecificationDetails' {..} =
      _salt `Prelude.hashWithSalt` cpuCredits

instance
  Prelude.NFData
    AwsEc2LaunchTemplateDataCreditSpecificationDetails
  where
  rnf
    AwsEc2LaunchTemplateDataCreditSpecificationDetails' {..} =
      Prelude.rnf cpuCredits

instance
  Data.ToJSON
    AwsEc2LaunchTemplateDataCreditSpecificationDetails
  where
  toJSON
    AwsEc2LaunchTemplateDataCreditSpecificationDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [("CpuCredits" Data..=) Prelude.<$> cpuCredits]
        )
