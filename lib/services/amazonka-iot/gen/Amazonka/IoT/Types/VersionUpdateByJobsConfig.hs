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
-- Module      : Amazonka.IoT.Types.VersionUpdateByJobsConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.VersionUpdateByJobsConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration to manage IoT Job\'s package version reporting. If
-- configured, Jobs updates the thing\'s reserved named shadow with the
-- package version information up on successful job completion.
--
-- __Note:__ For each job, the destinationPackageVersions attribute has to
-- be set with the correct data for Jobs to report to the thing shadow.
--
-- /See:/ 'newVersionUpdateByJobsConfig' smart constructor.
data VersionUpdateByJobsConfig = VersionUpdateByJobsConfig'
  { -- | Indicates whether the Job is enabled or not.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the role that grants permission to the
    -- IoT jobs service to update the reserved named shadow when the job
    -- successfully completes.
    roleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VersionUpdateByJobsConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'versionUpdateByJobsConfig_enabled' - Indicates whether the Job is enabled or not.
--
-- 'roleArn', 'versionUpdateByJobsConfig_roleArn' - The Amazon Resource Name (ARN) of the role that grants permission to the
-- IoT jobs service to update the reserved named shadow when the job
-- successfully completes.
newVersionUpdateByJobsConfig ::
  VersionUpdateByJobsConfig
newVersionUpdateByJobsConfig =
  VersionUpdateByJobsConfig'
    { enabled =
        Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | Indicates whether the Job is enabled or not.
versionUpdateByJobsConfig_enabled :: Lens.Lens' VersionUpdateByJobsConfig (Prelude.Maybe Prelude.Bool)
versionUpdateByJobsConfig_enabled = Lens.lens (\VersionUpdateByJobsConfig' {enabled} -> enabled) (\s@VersionUpdateByJobsConfig' {} a -> s {enabled = a} :: VersionUpdateByJobsConfig)

-- | The Amazon Resource Name (ARN) of the role that grants permission to the
-- IoT jobs service to update the reserved named shadow when the job
-- successfully completes.
versionUpdateByJobsConfig_roleArn :: Lens.Lens' VersionUpdateByJobsConfig (Prelude.Maybe Prelude.Text)
versionUpdateByJobsConfig_roleArn = Lens.lens (\VersionUpdateByJobsConfig' {roleArn} -> roleArn) (\s@VersionUpdateByJobsConfig' {} a -> s {roleArn = a} :: VersionUpdateByJobsConfig)

instance Data.FromJSON VersionUpdateByJobsConfig where
  parseJSON =
    Data.withObject
      "VersionUpdateByJobsConfig"
      ( \x ->
          VersionUpdateByJobsConfig'
            Prelude.<$> (x Data..:? "enabled")
            Prelude.<*> (x Data..:? "roleArn")
      )

instance Prelude.Hashable VersionUpdateByJobsConfig where
  hashWithSalt _salt VersionUpdateByJobsConfig' {..} =
    _salt
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData VersionUpdateByJobsConfig where
  rnf VersionUpdateByJobsConfig' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToJSON VersionUpdateByJobsConfig where
  toJSON VersionUpdateByJobsConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("enabled" Data..=) Prelude.<$> enabled,
            ("roleArn" Data..=) Prelude.<$> roleArn
          ]
      )
