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
-- Module      : Amazonka.EMRContainers.Types.S3MonitoringConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRContainers.Types.S3MonitoringConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Amazon S3 configuration for monitoring log publishing. You can configure
-- your jobs to send log information to Amazon S3.
--
-- /See:/ 'newS3MonitoringConfiguration' smart constructor.
data S3MonitoringConfiguration = S3MonitoringConfiguration'
  { -- | Amazon S3 destination URI for log publishing.
    logUri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3MonitoringConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logUri', 's3MonitoringConfiguration_logUri' - Amazon S3 destination URI for log publishing.
newS3MonitoringConfiguration ::
  -- | 'logUri'
  Prelude.Text ->
  S3MonitoringConfiguration
newS3MonitoringConfiguration pLogUri_ =
  S3MonitoringConfiguration' {logUri = pLogUri_}

-- | Amazon S3 destination URI for log publishing.
s3MonitoringConfiguration_logUri :: Lens.Lens' S3MonitoringConfiguration Prelude.Text
s3MonitoringConfiguration_logUri = Lens.lens (\S3MonitoringConfiguration' {logUri} -> logUri) (\s@S3MonitoringConfiguration' {} a -> s {logUri = a} :: S3MonitoringConfiguration)

instance Data.FromJSON S3MonitoringConfiguration where
  parseJSON =
    Data.withObject
      "S3MonitoringConfiguration"
      ( \x ->
          S3MonitoringConfiguration'
            Prelude.<$> (x Data..: "logUri")
      )

instance Prelude.Hashable S3MonitoringConfiguration where
  hashWithSalt _salt S3MonitoringConfiguration' {..} =
    _salt `Prelude.hashWithSalt` logUri

instance Prelude.NFData S3MonitoringConfiguration where
  rnf S3MonitoringConfiguration' {..} =
    Prelude.rnf logUri

instance Data.ToJSON S3MonitoringConfiguration where
  toJSON S3MonitoringConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("logUri" Data..= logUri)]
      )
