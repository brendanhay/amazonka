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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsClusterConfigurationExecuteCommandConfigurationDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsClusterConfigurationExecuteCommandConfigurationDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails

-- | Contains the run command configuration for the cluster.
--
-- /See:/ 'newAwsEcsClusterConfigurationExecuteCommandConfigurationDetails' smart constructor.
data AwsEcsClusterConfigurationExecuteCommandConfigurationDetails = AwsEcsClusterConfigurationExecuteCommandConfigurationDetails'
  { -- | The identifier of the KMS key that is used to encrypt the data between
    -- the local client and the container.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The log configuration for the results of the run command actions.
    -- Required if @Logging@ is @NONE@.
    logConfiguration :: Prelude.Maybe AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails,
    -- | The log setting to use for redirecting logs for run command results.
    logging :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsClusterConfigurationExecuteCommandConfigurationDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'awsEcsClusterConfigurationExecuteCommandConfigurationDetails_kmsKeyId' - The identifier of the KMS key that is used to encrypt the data between
-- the local client and the container.
--
-- 'logConfiguration', 'awsEcsClusterConfigurationExecuteCommandConfigurationDetails_logConfiguration' - The log configuration for the results of the run command actions.
-- Required if @Logging@ is @NONE@.
--
-- 'logging', 'awsEcsClusterConfigurationExecuteCommandConfigurationDetails_logging' - The log setting to use for redirecting logs for run command results.
newAwsEcsClusterConfigurationExecuteCommandConfigurationDetails ::
  AwsEcsClusterConfigurationExecuteCommandConfigurationDetails
newAwsEcsClusterConfigurationExecuteCommandConfigurationDetails =
  AwsEcsClusterConfigurationExecuteCommandConfigurationDetails'
    { kmsKeyId =
        Prelude.Nothing,
      logConfiguration =
        Prelude.Nothing,
      logging =
        Prelude.Nothing
    }

-- | The identifier of the KMS key that is used to encrypt the data between
-- the local client and the container.
awsEcsClusterConfigurationExecuteCommandConfigurationDetails_kmsKeyId :: Lens.Lens' AwsEcsClusterConfigurationExecuteCommandConfigurationDetails (Prelude.Maybe Prelude.Text)
awsEcsClusterConfigurationExecuteCommandConfigurationDetails_kmsKeyId = Lens.lens (\AwsEcsClusterConfigurationExecuteCommandConfigurationDetails' {kmsKeyId} -> kmsKeyId) (\s@AwsEcsClusterConfigurationExecuteCommandConfigurationDetails' {} a -> s {kmsKeyId = a} :: AwsEcsClusterConfigurationExecuteCommandConfigurationDetails)

-- | The log configuration for the results of the run command actions.
-- Required if @Logging@ is @NONE@.
awsEcsClusterConfigurationExecuteCommandConfigurationDetails_logConfiguration :: Lens.Lens' AwsEcsClusterConfigurationExecuteCommandConfigurationDetails (Prelude.Maybe AwsEcsClusterConfigurationExecuteCommandConfigurationLogConfigurationDetails)
awsEcsClusterConfigurationExecuteCommandConfigurationDetails_logConfiguration = Lens.lens (\AwsEcsClusterConfigurationExecuteCommandConfigurationDetails' {logConfiguration} -> logConfiguration) (\s@AwsEcsClusterConfigurationExecuteCommandConfigurationDetails' {} a -> s {logConfiguration = a} :: AwsEcsClusterConfigurationExecuteCommandConfigurationDetails)

-- | The log setting to use for redirecting logs for run command results.
awsEcsClusterConfigurationExecuteCommandConfigurationDetails_logging :: Lens.Lens' AwsEcsClusterConfigurationExecuteCommandConfigurationDetails (Prelude.Maybe Prelude.Text)
awsEcsClusterConfigurationExecuteCommandConfigurationDetails_logging = Lens.lens (\AwsEcsClusterConfigurationExecuteCommandConfigurationDetails' {logging} -> logging) (\s@AwsEcsClusterConfigurationExecuteCommandConfigurationDetails' {} a -> s {logging = a} :: AwsEcsClusterConfigurationExecuteCommandConfigurationDetails)

instance
  Data.FromJSON
    AwsEcsClusterConfigurationExecuteCommandConfigurationDetails
  where
  parseJSON =
    Data.withObject
      "AwsEcsClusterConfigurationExecuteCommandConfigurationDetails"
      ( \x ->
          AwsEcsClusterConfigurationExecuteCommandConfigurationDetails'
            Prelude.<$> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..:? "LogConfiguration")
            Prelude.<*> (x Data..:? "Logging")
      )

instance
  Prelude.Hashable
    AwsEcsClusterConfigurationExecuteCommandConfigurationDetails
  where
  hashWithSalt
    _salt
    AwsEcsClusterConfigurationExecuteCommandConfigurationDetails' {..} =
      _salt
        `Prelude.hashWithSalt` kmsKeyId
        `Prelude.hashWithSalt` logConfiguration
        `Prelude.hashWithSalt` logging

instance
  Prelude.NFData
    AwsEcsClusterConfigurationExecuteCommandConfigurationDetails
  where
  rnf
    AwsEcsClusterConfigurationExecuteCommandConfigurationDetails' {..} =
      Prelude.rnf kmsKeyId
        `Prelude.seq` Prelude.rnf logConfiguration
        `Prelude.seq` Prelude.rnf logging

instance
  Data.ToJSON
    AwsEcsClusterConfigurationExecuteCommandConfigurationDetails
  where
  toJSON
    AwsEcsClusterConfigurationExecuteCommandConfigurationDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
              ("LogConfiguration" Data..=)
                Prelude.<$> logConfiguration,
              ("Logging" Data..=) Prelude.<$> logging
            ]
        )
