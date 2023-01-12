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
-- Module      : Amazonka.EMRContainers.Types.ParametricS3MonitoringConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRContainers.Types.ParametricS3MonitoringConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Amazon S3 configuration for monitoring log publishing. You can configure
-- your jobs to send log information to Amazon S3. This data type allows
-- job template parameters to be specified within.
--
-- /See:/ 'newParametricS3MonitoringConfiguration' smart constructor.
data ParametricS3MonitoringConfiguration = ParametricS3MonitoringConfiguration'
  { -- | Amazon S3 destination URI for log publishing.
    logUri :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParametricS3MonitoringConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logUri', 'parametricS3MonitoringConfiguration_logUri' - Amazon S3 destination URI for log publishing.
newParametricS3MonitoringConfiguration ::
  ParametricS3MonitoringConfiguration
newParametricS3MonitoringConfiguration =
  ParametricS3MonitoringConfiguration'
    { logUri =
        Prelude.Nothing
    }

-- | Amazon S3 destination URI for log publishing.
parametricS3MonitoringConfiguration_logUri :: Lens.Lens' ParametricS3MonitoringConfiguration (Prelude.Maybe Prelude.Text)
parametricS3MonitoringConfiguration_logUri = Lens.lens (\ParametricS3MonitoringConfiguration' {logUri} -> logUri) (\s@ParametricS3MonitoringConfiguration' {} a -> s {logUri = a} :: ParametricS3MonitoringConfiguration)

instance
  Data.FromJSON
    ParametricS3MonitoringConfiguration
  where
  parseJSON =
    Data.withObject
      "ParametricS3MonitoringConfiguration"
      ( \x ->
          ParametricS3MonitoringConfiguration'
            Prelude.<$> (x Data..:? "logUri")
      )

instance
  Prelude.Hashable
    ParametricS3MonitoringConfiguration
  where
  hashWithSalt
    _salt
    ParametricS3MonitoringConfiguration' {..} =
      _salt `Prelude.hashWithSalt` logUri

instance
  Prelude.NFData
    ParametricS3MonitoringConfiguration
  where
  rnf ParametricS3MonitoringConfiguration' {..} =
    Prelude.rnf logUri

instance
  Data.ToJSON
    ParametricS3MonitoringConfiguration
  where
  toJSON ParametricS3MonitoringConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("logUri" Data..=) Prelude.<$> logUri]
      )
