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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.DeployAsApplicationConfigurationDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.DeployAsApplicationConfigurationDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types.S3ContentBaseLocationDescription
import qualified Amazonka.Prelude as Prelude

-- | The configuration information required to deploy an Amazon Data
-- Analytics Studio notebook as an application with durable state.
--
-- /See:/ 'newDeployAsApplicationConfigurationDescription' smart constructor.
data DeployAsApplicationConfigurationDescription = DeployAsApplicationConfigurationDescription'
  { -- | The location that holds the data required to specify an Amazon Data
    -- Analytics application.
    s3ContentLocationDescription :: S3ContentBaseLocationDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeployAsApplicationConfigurationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3ContentLocationDescription', 'deployAsApplicationConfigurationDescription_s3ContentLocationDescription' - The location that holds the data required to specify an Amazon Data
-- Analytics application.
newDeployAsApplicationConfigurationDescription ::
  -- | 's3ContentLocationDescription'
  S3ContentBaseLocationDescription ->
  DeployAsApplicationConfigurationDescription
newDeployAsApplicationConfigurationDescription
  pS3ContentLocationDescription_ =
    DeployAsApplicationConfigurationDescription'
      { s3ContentLocationDescription =
          pS3ContentLocationDescription_
      }

-- | The location that holds the data required to specify an Amazon Data
-- Analytics application.
deployAsApplicationConfigurationDescription_s3ContentLocationDescription :: Lens.Lens' DeployAsApplicationConfigurationDescription S3ContentBaseLocationDescription
deployAsApplicationConfigurationDescription_s3ContentLocationDescription = Lens.lens (\DeployAsApplicationConfigurationDescription' {s3ContentLocationDescription} -> s3ContentLocationDescription) (\s@DeployAsApplicationConfigurationDescription' {} a -> s {s3ContentLocationDescription = a} :: DeployAsApplicationConfigurationDescription)

instance
  Data.FromJSON
    DeployAsApplicationConfigurationDescription
  where
  parseJSON =
    Data.withObject
      "DeployAsApplicationConfigurationDescription"
      ( \x ->
          DeployAsApplicationConfigurationDescription'
            Prelude.<$> (x Data..: "S3ContentLocationDescription")
      )

instance
  Prelude.Hashable
    DeployAsApplicationConfigurationDescription
  where
  hashWithSalt
    _salt
    DeployAsApplicationConfigurationDescription' {..} =
      _salt
        `Prelude.hashWithSalt` s3ContentLocationDescription

instance
  Prelude.NFData
    DeployAsApplicationConfigurationDescription
  where
  rnf DeployAsApplicationConfigurationDescription' {..} =
    Prelude.rnf s3ContentLocationDescription
