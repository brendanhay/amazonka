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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The flag that enables the matching process of duplicate profiles.
--
-- /See:/ 'newMatchingResponse' smart constructor.
data MatchingResponse = MatchingResponse'
  { -- | Configuration information about the auto-merging process.
    autoMerging :: Prelude.Maybe AutoMerging,
    -- | The flag that enables the matching process of duplicate profiles.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | Configuration information for exporting Identity Resolution results, for
    -- example, to an S3 bucket.
    exportingConfig :: Prelude.Maybe ExportingConfig,
    -- | The day and time when do you want to start the Identity Resolution Job
    -- every week.
    jobSchedule :: Prelude.Maybe JobSchedule
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
-- 'autoMerging', 'matchingResponse_autoMerging' - Configuration information about the auto-merging process.
--
-- 'enabled', 'matchingResponse_enabled' - The flag that enables the matching process of duplicate profiles.
--
-- 'exportingConfig', 'matchingResponse_exportingConfig' - Configuration information for exporting Identity Resolution results, for
-- example, to an S3 bucket.
--
-- 'jobSchedule', 'matchingResponse_jobSchedule' - The day and time when do you want to start the Identity Resolution Job
-- every week.
newMatchingResponse ::
  MatchingResponse
newMatchingResponse =
  MatchingResponse'
    { autoMerging = Prelude.Nothing,
      enabled = Prelude.Nothing,
      exportingConfig = Prelude.Nothing,
      jobSchedule = Prelude.Nothing
    }

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

-- | The day and time when do you want to start the Identity Resolution Job
-- every week.
matchingResponse_jobSchedule :: Lens.Lens' MatchingResponse (Prelude.Maybe JobSchedule)
matchingResponse_jobSchedule = Lens.lens (\MatchingResponse' {jobSchedule} -> jobSchedule) (\s@MatchingResponse' {} a -> s {jobSchedule = a} :: MatchingResponse)

instance Data.FromJSON MatchingResponse where
  parseJSON =
    Data.withObject
      "MatchingResponse"
      ( \x ->
          MatchingResponse'
            Prelude.<$> (x Data..:? "AutoMerging")
            Prelude.<*> (x Data..:? "Enabled")
            Prelude.<*> (x Data..:? "ExportingConfig")
            Prelude.<*> (x Data..:? "JobSchedule")
      )

instance Prelude.Hashable MatchingResponse where
  hashWithSalt _salt MatchingResponse' {..} =
    _salt `Prelude.hashWithSalt` autoMerging
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` exportingConfig
      `Prelude.hashWithSalt` jobSchedule

instance Prelude.NFData MatchingResponse where
  rnf MatchingResponse' {..} =
    Prelude.rnf autoMerging
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf exportingConfig
      `Prelude.seq` Prelude.rnf jobSchedule
