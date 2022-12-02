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
-- Module      : Amazonka.AccessAnalyzer.Types.CloudTrailDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.CloudTrailDetails where

import Amazonka.AccessAnalyzer.Types.Trail
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about CloudTrail access.
--
-- /See:/ 'newCloudTrailDetails' smart constructor.
data CloudTrailDetails = CloudTrailDetails'
  { -- | The end of the time range for which IAM Access Analyzer reviews your
    -- CloudTrail events. Events with a timestamp after this time are not
    -- considered to generate a policy. If this is not included in the request,
    -- the default value is the current time.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | A @Trail@ object that contains settings for a trail.
    trails :: [Trail],
    -- | The ARN of the service role that IAM Access Analyzer uses to access your
    -- CloudTrail trail and service last accessed information.
    accessRole :: Prelude.Text,
    -- | The start of the time range for which IAM Access Analyzer reviews your
    -- CloudTrail events. Events with a timestamp before this time are not
    -- considered to generate a policy.
    startTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudTrailDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'cloudTrailDetails_endTime' - The end of the time range for which IAM Access Analyzer reviews your
-- CloudTrail events. Events with a timestamp after this time are not
-- considered to generate a policy. If this is not included in the request,
-- the default value is the current time.
--
-- 'trails', 'cloudTrailDetails_trails' - A @Trail@ object that contains settings for a trail.
--
-- 'accessRole', 'cloudTrailDetails_accessRole' - The ARN of the service role that IAM Access Analyzer uses to access your
-- CloudTrail trail and service last accessed information.
--
-- 'startTime', 'cloudTrailDetails_startTime' - The start of the time range for which IAM Access Analyzer reviews your
-- CloudTrail events. Events with a timestamp before this time are not
-- considered to generate a policy.
newCloudTrailDetails ::
  -- | 'accessRole'
  Prelude.Text ->
  -- | 'startTime'
  Prelude.UTCTime ->
  CloudTrailDetails
newCloudTrailDetails pAccessRole_ pStartTime_ =
  CloudTrailDetails'
    { endTime = Prelude.Nothing,
      trails = Prelude.mempty,
      accessRole = pAccessRole_,
      startTime = Data._Time Lens.# pStartTime_
    }

-- | The end of the time range for which IAM Access Analyzer reviews your
-- CloudTrail events. Events with a timestamp after this time are not
-- considered to generate a policy. If this is not included in the request,
-- the default value is the current time.
cloudTrailDetails_endTime :: Lens.Lens' CloudTrailDetails (Prelude.Maybe Prelude.UTCTime)
cloudTrailDetails_endTime = Lens.lens (\CloudTrailDetails' {endTime} -> endTime) (\s@CloudTrailDetails' {} a -> s {endTime = a} :: CloudTrailDetails) Prelude.. Lens.mapping Data._Time

-- | A @Trail@ object that contains settings for a trail.
cloudTrailDetails_trails :: Lens.Lens' CloudTrailDetails [Trail]
cloudTrailDetails_trails = Lens.lens (\CloudTrailDetails' {trails} -> trails) (\s@CloudTrailDetails' {} a -> s {trails = a} :: CloudTrailDetails) Prelude.. Lens.coerced

-- | The ARN of the service role that IAM Access Analyzer uses to access your
-- CloudTrail trail and service last accessed information.
cloudTrailDetails_accessRole :: Lens.Lens' CloudTrailDetails Prelude.Text
cloudTrailDetails_accessRole = Lens.lens (\CloudTrailDetails' {accessRole} -> accessRole) (\s@CloudTrailDetails' {} a -> s {accessRole = a} :: CloudTrailDetails)

-- | The start of the time range for which IAM Access Analyzer reviews your
-- CloudTrail events. Events with a timestamp before this time are not
-- considered to generate a policy.
cloudTrailDetails_startTime :: Lens.Lens' CloudTrailDetails Prelude.UTCTime
cloudTrailDetails_startTime = Lens.lens (\CloudTrailDetails' {startTime} -> startTime) (\s@CloudTrailDetails' {} a -> s {startTime = a} :: CloudTrailDetails) Prelude.. Data._Time

instance Prelude.Hashable CloudTrailDetails where
  hashWithSalt _salt CloudTrailDetails' {..} =
    _salt `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` trails
      `Prelude.hashWithSalt` accessRole
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData CloudTrailDetails where
  rnf CloudTrailDetails' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf trails
      `Prelude.seq` Prelude.rnf accessRole
      `Prelude.seq` Prelude.rnf startTime

instance Data.ToJSON CloudTrailDetails where
  toJSON CloudTrailDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("endTime" Data..=) Prelude.<$> endTime,
            Prelude.Just ("trails" Data..= trails),
            Prelude.Just ("accessRole" Data..= accessRole),
            Prelude.Just ("startTime" Data..= startTime)
          ]
      )
