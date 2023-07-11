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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataEnclaveOptionsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataEnclaveOptionsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Indicates whether the instance is enabled for Amazon Web Services Nitro
-- Enclaves.
--
-- /See:/ 'newAwsEc2LaunchTemplateDataEnclaveOptionsDetails' smart constructor.
data AwsEc2LaunchTemplateDataEnclaveOptionsDetails = AwsEc2LaunchTemplateDataEnclaveOptionsDetails'
  { -- | If this parameter is set to @true@, the instance is enabled for Amazon
    -- Web Services Nitro Enclaves.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDataEnclaveOptionsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'awsEc2LaunchTemplateDataEnclaveOptionsDetails_enabled' - If this parameter is set to @true@, the instance is enabled for Amazon
-- Web Services Nitro Enclaves.
newAwsEc2LaunchTemplateDataEnclaveOptionsDetails ::
  AwsEc2LaunchTemplateDataEnclaveOptionsDetails
newAwsEc2LaunchTemplateDataEnclaveOptionsDetails =
  AwsEc2LaunchTemplateDataEnclaveOptionsDetails'
    { enabled =
        Prelude.Nothing
    }

-- | If this parameter is set to @true@, the instance is enabled for Amazon
-- Web Services Nitro Enclaves.
awsEc2LaunchTemplateDataEnclaveOptionsDetails_enabled :: Lens.Lens' AwsEc2LaunchTemplateDataEnclaveOptionsDetails (Prelude.Maybe Prelude.Bool)
awsEc2LaunchTemplateDataEnclaveOptionsDetails_enabled = Lens.lens (\AwsEc2LaunchTemplateDataEnclaveOptionsDetails' {enabled} -> enabled) (\s@AwsEc2LaunchTemplateDataEnclaveOptionsDetails' {} a -> s {enabled = a} :: AwsEc2LaunchTemplateDataEnclaveOptionsDetails)

instance
  Data.FromJSON
    AwsEc2LaunchTemplateDataEnclaveOptionsDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDataEnclaveOptionsDetails"
      ( \x ->
          AwsEc2LaunchTemplateDataEnclaveOptionsDetails'
            Prelude.<$> (x Data..:? "Enabled")
      )

instance
  Prelude.Hashable
    AwsEc2LaunchTemplateDataEnclaveOptionsDetails
  where
  hashWithSalt
    _salt
    AwsEc2LaunchTemplateDataEnclaveOptionsDetails' {..} =
      _salt `Prelude.hashWithSalt` enabled

instance
  Prelude.NFData
    AwsEc2LaunchTemplateDataEnclaveOptionsDetails
  where
  rnf
    AwsEc2LaunchTemplateDataEnclaveOptionsDetails' {..} =
      Prelude.rnf enabled

instance
  Data.ToJSON
    AwsEc2LaunchTemplateDataEnclaveOptionsDetails
  where
  toJSON
    AwsEc2LaunchTemplateDataEnclaveOptionsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [("Enabled" Data..=) Prelude.<$> enabled]
        )
