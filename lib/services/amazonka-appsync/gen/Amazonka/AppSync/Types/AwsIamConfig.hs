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
-- Module      : Amazonka.AppSync.Types.AwsIamConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.AwsIamConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Identity and Access Management (IAM) configuration.
--
-- /See:/ 'newAwsIamConfig' smart constructor.
data AwsIamConfig = AwsIamConfig'
  { -- | The signing Amazon Web Services Region for IAM authorization.
    signingRegion :: Prelude.Maybe Prelude.Text,
    -- | The signing service name for IAM authorization.
    signingServiceName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsIamConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'signingRegion', 'awsIamConfig_signingRegion' - The signing Amazon Web Services Region for IAM authorization.
--
-- 'signingServiceName', 'awsIamConfig_signingServiceName' - The signing service name for IAM authorization.
newAwsIamConfig ::
  AwsIamConfig
newAwsIamConfig =
  AwsIamConfig'
    { signingRegion = Prelude.Nothing,
      signingServiceName = Prelude.Nothing
    }

-- | The signing Amazon Web Services Region for IAM authorization.
awsIamConfig_signingRegion :: Lens.Lens' AwsIamConfig (Prelude.Maybe Prelude.Text)
awsIamConfig_signingRegion = Lens.lens (\AwsIamConfig' {signingRegion} -> signingRegion) (\s@AwsIamConfig' {} a -> s {signingRegion = a} :: AwsIamConfig)

-- | The signing service name for IAM authorization.
awsIamConfig_signingServiceName :: Lens.Lens' AwsIamConfig (Prelude.Maybe Prelude.Text)
awsIamConfig_signingServiceName = Lens.lens (\AwsIamConfig' {signingServiceName} -> signingServiceName) (\s@AwsIamConfig' {} a -> s {signingServiceName = a} :: AwsIamConfig)

instance Data.FromJSON AwsIamConfig where
  parseJSON =
    Data.withObject
      "AwsIamConfig"
      ( \x ->
          AwsIamConfig'
            Prelude.<$> (x Data..:? "signingRegion")
            Prelude.<*> (x Data..:? "signingServiceName")
      )

instance Prelude.Hashable AwsIamConfig where
  hashWithSalt _salt AwsIamConfig' {..} =
    _salt
      `Prelude.hashWithSalt` signingRegion
      `Prelude.hashWithSalt` signingServiceName

instance Prelude.NFData AwsIamConfig where
  rnf AwsIamConfig' {..} =
    Prelude.rnf signingRegion
      `Prelude.seq` Prelude.rnf signingServiceName

instance Data.ToJSON AwsIamConfig where
  toJSON AwsIamConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("signingRegion" Data..=) Prelude.<$> signingRegion,
            ("signingServiceName" Data..=)
              Prelude.<$> signingServiceName
          ]
      )
