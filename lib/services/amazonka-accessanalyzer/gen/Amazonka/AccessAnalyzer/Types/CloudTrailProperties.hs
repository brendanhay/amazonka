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
-- Module      : Amazonka.AccessAnalyzer.Types.CloudTrailProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.CloudTrailProperties where

import Amazonka.AccessAnalyzer.Types.TrailProperties
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about CloudTrail access.
--
-- /See:/ 'newCloudTrailProperties' smart constructor.
data CloudTrailProperties = CloudTrailProperties'
  { -- | The end of the time range for which IAM Access Analyzer reviews your
    -- CloudTrail events. Events with a timestamp after this time are not
    -- considered to generate a policy. If this is not included in the request,
    -- the default value is the current time.
    endTime :: Core.POSIX,
    -- | The start of the time range for which IAM Access Analyzer reviews your
    -- CloudTrail events. Events with a timestamp before this time are not
    -- considered to generate a policy.
    startTime :: Core.POSIX,
    -- | A @TrailProperties@ object that contains settings for trail properties.
    trailProperties :: [TrailProperties]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudTrailProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'cloudTrailProperties_endTime' - The end of the time range for which IAM Access Analyzer reviews your
-- CloudTrail events. Events with a timestamp after this time are not
-- considered to generate a policy. If this is not included in the request,
-- the default value is the current time.
--
-- 'startTime', 'cloudTrailProperties_startTime' - The start of the time range for which IAM Access Analyzer reviews your
-- CloudTrail events. Events with a timestamp before this time are not
-- considered to generate a policy.
--
-- 'trailProperties', 'cloudTrailProperties_trailProperties' - A @TrailProperties@ object that contains settings for trail properties.
newCloudTrailProperties ::
  -- | 'endTime'
  Prelude.UTCTime ->
  -- | 'startTime'
  Prelude.UTCTime ->
  CloudTrailProperties
newCloudTrailProperties pEndTime_ pStartTime_ =
  CloudTrailProperties'
    { endTime =
        Core._Time Lens.# pEndTime_,
      startTime = Core._Time Lens.# pStartTime_,
      trailProperties = Prelude.mempty
    }

-- | The end of the time range for which IAM Access Analyzer reviews your
-- CloudTrail events. Events with a timestamp after this time are not
-- considered to generate a policy. If this is not included in the request,
-- the default value is the current time.
cloudTrailProperties_endTime :: Lens.Lens' CloudTrailProperties Prelude.UTCTime
cloudTrailProperties_endTime = Lens.lens (\CloudTrailProperties' {endTime} -> endTime) (\s@CloudTrailProperties' {} a -> s {endTime = a} :: CloudTrailProperties) Prelude.. Core._Time

-- | The start of the time range for which IAM Access Analyzer reviews your
-- CloudTrail events. Events with a timestamp before this time are not
-- considered to generate a policy.
cloudTrailProperties_startTime :: Lens.Lens' CloudTrailProperties Prelude.UTCTime
cloudTrailProperties_startTime = Lens.lens (\CloudTrailProperties' {startTime} -> startTime) (\s@CloudTrailProperties' {} a -> s {startTime = a} :: CloudTrailProperties) Prelude.. Core._Time

-- | A @TrailProperties@ object that contains settings for trail properties.
cloudTrailProperties_trailProperties :: Lens.Lens' CloudTrailProperties [TrailProperties]
cloudTrailProperties_trailProperties = Lens.lens (\CloudTrailProperties' {trailProperties} -> trailProperties) (\s@CloudTrailProperties' {} a -> s {trailProperties = a} :: CloudTrailProperties) Prelude.. Lens.coerced

instance Core.FromJSON CloudTrailProperties where
  parseJSON =
    Core.withObject
      "CloudTrailProperties"
      ( \x ->
          CloudTrailProperties'
            Prelude.<$> (x Core..: "endTime")
            Prelude.<*> (x Core..: "startTime")
            Prelude.<*> ( x Core..:? "trailProperties"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable CloudTrailProperties where
  hashWithSalt salt' CloudTrailProperties' {..} =
    salt' `Prelude.hashWithSalt` trailProperties
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` endTime

instance Prelude.NFData CloudTrailProperties where
  rnf CloudTrailProperties' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf trailProperties
      `Prelude.seq` Prelude.rnf startTime
