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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.DeployAsApplicationConfigurationUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.DeployAsApplicationConfigurationUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types.S3ContentBaseLocationUpdate
import qualified Amazonka.Prelude as Prelude

-- | Updates to the configuration information required to deploy an Amazon
-- Data Analytics Studio notebook as an application with durable state.
--
-- /See:/ 'newDeployAsApplicationConfigurationUpdate' smart constructor.
data DeployAsApplicationConfigurationUpdate = DeployAsApplicationConfigurationUpdate'
  { -- | Updates to the location that holds the data required to specify an
    -- Amazon Data Analytics application.
    s3ContentLocationUpdate :: Prelude.Maybe S3ContentBaseLocationUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeployAsApplicationConfigurationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3ContentLocationUpdate', 'deployAsApplicationConfigurationUpdate_s3ContentLocationUpdate' - Updates to the location that holds the data required to specify an
-- Amazon Data Analytics application.
newDeployAsApplicationConfigurationUpdate ::
  DeployAsApplicationConfigurationUpdate
newDeployAsApplicationConfigurationUpdate =
  DeployAsApplicationConfigurationUpdate'
    { s3ContentLocationUpdate =
        Prelude.Nothing
    }

-- | Updates to the location that holds the data required to specify an
-- Amazon Data Analytics application.
deployAsApplicationConfigurationUpdate_s3ContentLocationUpdate :: Lens.Lens' DeployAsApplicationConfigurationUpdate (Prelude.Maybe S3ContentBaseLocationUpdate)
deployAsApplicationConfigurationUpdate_s3ContentLocationUpdate = Lens.lens (\DeployAsApplicationConfigurationUpdate' {s3ContentLocationUpdate} -> s3ContentLocationUpdate) (\s@DeployAsApplicationConfigurationUpdate' {} a -> s {s3ContentLocationUpdate = a} :: DeployAsApplicationConfigurationUpdate)

instance
  Prelude.Hashable
    DeployAsApplicationConfigurationUpdate
  where
  hashWithSalt
    _salt
    DeployAsApplicationConfigurationUpdate' {..} =
      _salt
        `Prelude.hashWithSalt` s3ContentLocationUpdate

instance
  Prelude.NFData
    DeployAsApplicationConfigurationUpdate
  where
  rnf DeployAsApplicationConfigurationUpdate' {..} =
    Prelude.rnf s3ContentLocationUpdate

instance
  Data.ToJSON
    DeployAsApplicationConfigurationUpdate
  where
  toJSON DeployAsApplicationConfigurationUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("S3ContentLocationUpdate" Data..=)
              Prelude.<$> s3ContentLocationUpdate
          ]
      )
