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
-- Module      : Amazonka.CustomerProfiles.Types.MatchingResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.MatchingResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types.AutoMerging
import Amazonka.CustomerProfiles.Types.ExportingConfig
import Amazonka.CustomerProfiles.Types.JobSchedule
import qualified Amazonka.Prelude as Prelude

-- | The flag that enables the matching process of duplicate profiles.
--
-- /See:/ 'newMatchingResponse' smart constructor.
data MatchingResponse = MatchingResponse'
  { -- | The day and time when do you want to start the Identity Resolution Job
    -- every week.
    jobSchedule :: Prelude.Maybe JobSchedule,
    -- | Configuration information about the auto-merging process.
    autoMerging :: Prelude.Maybe AutoMerging,
    -- | The flag that enables the matching process of duplicate profiles.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | Configuration information for exporting Identity Resolution results, for
    -- example, to an S3 bucket.
    exportingConfig :: Prelude.Maybe ExportingConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MatchingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobSchedule', 'matchingResponse_jobSchedule' - The day and time when do you want to start the Identity Resolution Job
-- every week.
--
-- 'autoMerging', 'matchingResponse_autoMerging' - Configuration information about the auto-merging process.
--
-- 'enabled', 'matchingResponse_enabled' - The flag that enables the matching process of duplicate profiles.
--
-- 'exportingConfig', 'matchingResponse_exportingConfig' - Configuration information for exporting Identity Resolution results, for
-- example, to an S3 bucket.
newMatchingResponse ::
  MatchingResponse
newMatchingResponse =
  MatchingResponse'
    { jobSchedule = Prelude.Nothing,
      autoMerging = Prelude.Nothing,
      enabled = Prelude.Nothing,
      exportingConfig = Prelude.Nothing
    }

-- | The day and time when do you want to start the Identity Resolution Job
-- every week.
matchingResponse_jobSchedule :: Lens.Lens' MatchingResponse (Prelude.Maybe JobSchedule)
matchingResponse_jobSchedule = Lens.lens (\MatchingResponse' {jobSchedule} -> jobSchedule) (\s@MatchingResponse' {} a -> s {jobSchedule = a} :: MatchingResponse)

-- | Configuration information about the auto-merging process.
matchingResponse_autoMerging :: Lens.Lens' MatchingResponse (Prelude.Maybe AutoMerging)
matchingResponse_autoMerging = Lens.lens (\MatchingResponse' {autoMerging} -> autoMerging) (\s@MatchingResponse' {} a -> s {autoMerging = a} :: MatchingResponse)

-- | The flag that enables the matching process of duplicate profiles.
matchingResponse_enabled :: Lens.Lens' MatchingResponse (Prelude.Maybe Prelude.Bool)
matchingResponse_enabled = Lens.lens (\MatchingResponse' {enabled} -> enabled) (\s@MatchingResponse' {} a -> s {enabled = a} :: MatchingResponse)

-- | Configuration information for exporting Identity Resolution results, for
-- example, to an S3 bucket.
matchingResponse_exportingConfig :: Lens.Lens' MatchingResponse (Prelude.Maybe ExportingConfig)
matchingResponse_exportingConfig = Lens.lens (\MatchingResponse' {exportingConfig} -> exportingConfig) (\s@MatchingResponse' {} a -> s {exportingConfig = a} :: MatchingResponse)

instance Core.FromJSON MatchingResponse where
  parseJSON =
    Core.withObject
      "MatchingResponse"
      ( \x ->
          MatchingResponse'
            Prelude.<$> (x Core..:? "JobSchedule")
            Prelude.<*> (x Core..:? "AutoMerging")
            Prelude.<*> (x Core..:? "Enabled")
            Prelude.<*> (x Core..:? "ExportingConfig")
      )

instance Prelude.Hashable MatchingResponse where
  hashWithSalt _salt MatchingResponse' {..} =
    _salt `Prelude.hashWithSalt` jobSchedule
      `Prelude.hashWithSalt` autoMerging
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` exportingConfig

instance Prelude.NFData MatchingResponse where
  rnf MatchingResponse' {..} =
    Prelude.rnf jobSchedule
      `Prelude.seq` Prelude.rnf autoMerging
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf exportingConfig
