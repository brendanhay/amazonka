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
-- Module      : Amazonka.CustomerProfiles.Types.MatchingRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.MatchingRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types.AutoMerging
import Amazonka.CustomerProfiles.Types.ExportingConfig
import Amazonka.CustomerProfiles.Types.JobSchedule
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The flag that enables the matching process of duplicate profiles.
--
-- /See:/ 'newMatchingRequest' smart constructor.
data MatchingRequest = MatchingRequest'
  { -- | Configuration information about the auto-merging process.
    autoMerging :: Prelude.Maybe AutoMerging,
    -- | Configuration information for exporting Identity Resolution results, for
    -- example, to an S3 bucket.
    exportingConfig :: Prelude.Maybe ExportingConfig,
    -- | The day and time when do you want to start the Identity Resolution Job
    -- every week.
    jobSchedule :: Prelude.Maybe JobSchedule,
    -- | The flag that enables the matching process of duplicate profiles.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MatchingRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoMerging', 'matchingRequest_autoMerging' - Configuration information about the auto-merging process.
--
-- 'exportingConfig', 'matchingRequest_exportingConfig' - Configuration information for exporting Identity Resolution results, for
-- example, to an S3 bucket.
--
-- 'jobSchedule', 'matchingRequest_jobSchedule' - The day and time when do you want to start the Identity Resolution Job
-- every week.
--
-- 'enabled', 'matchingRequest_enabled' - The flag that enables the matching process of duplicate profiles.
newMatchingRequest ::
  -- | 'enabled'
  Prelude.Bool ->
  MatchingRequest
newMatchingRequest pEnabled_ =
  MatchingRequest'
    { autoMerging = Prelude.Nothing,
      exportingConfig = Prelude.Nothing,
      jobSchedule = Prelude.Nothing,
      enabled = pEnabled_
    }

-- | Configuration information about the auto-merging process.
matchingRequest_autoMerging :: Lens.Lens' MatchingRequest (Prelude.Maybe AutoMerging)
matchingRequest_autoMerging = Lens.lens (\MatchingRequest' {autoMerging} -> autoMerging) (\s@MatchingRequest' {} a -> s {autoMerging = a} :: MatchingRequest)

-- | Configuration information for exporting Identity Resolution results, for
-- example, to an S3 bucket.
matchingRequest_exportingConfig :: Lens.Lens' MatchingRequest (Prelude.Maybe ExportingConfig)
matchingRequest_exportingConfig = Lens.lens (\MatchingRequest' {exportingConfig} -> exportingConfig) (\s@MatchingRequest' {} a -> s {exportingConfig = a} :: MatchingRequest)

-- | The day and time when do you want to start the Identity Resolution Job
-- every week.
matchingRequest_jobSchedule :: Lens.Lens' MatchingRequest (Prelude.Maybe JobSchedule)
matchingRequest_jobSchedule = Lens.lens (\MatchingRequest' {jobSchedule} -> jobSchedule) (\s@MatchingRequest' {} a -> s {jobSchedule = a} :: MatchingRequest)

-- | The flag that enables the matching process of duplicate profiles.
matchingRequest_enabled :: Lens.Lens' MatchingRequest Prelude.Bool
matchingRequest_enabled = Lens.lens (\MatchingRequest' {enabled} -> enabled) (\s@MatchingRequest' {} a -> s {enabled = a} :: MatchingRequest)

instance Prelude.Hashable MatchingRequest where
  hashWithSalt _salt MatchingRequest' {..} =
    _salt
      `Prelude.hashWithSalt` autoMerging
      `Prelude.hashWithSalt` exportingConfig
      `Prelude.hashWithSalt` jobSchedule
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData MatchingRequest where
  rnf MatchingRequest' {..} =
    Prelude.rnf autoMerging `Prelude.seq`
      Prelude.rnf exportingConfig `Prelude.seq`
        Prelude.rnf jobSchedule `Prelude.seq`
          Prelude.rnf enabled

instance Data.ToJSON MatchingRequest where
  toJSON MatchingRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AutoMerging" Data..=) Prelude.<$> autoMerging,
            ("ExportingConfig" Data..=)
              Prelude.<$> exportingConfig,
            ("JobSchedule" Data..=) Prelude.<$> jobSchedule,
            Prelude.Just ("Enabled" Data..= enabled)
          ]
      )
