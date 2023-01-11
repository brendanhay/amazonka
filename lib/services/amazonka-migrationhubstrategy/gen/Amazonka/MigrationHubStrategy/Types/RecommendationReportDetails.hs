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
-- Module      : Amazonka.MigrationHubStrategy.Types.RecommendationReportDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.RecommendationReportDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.RecommendationReportStatus
import qualified Amazonka.Prelude as Prelude

-- | Contains detailed information about a recommendation report.
--
-- /See:/ 'newRecommendationReportDetails' smart constructor.
data RecommendationReportDetails = RecommendationReportDetails'
  { -- | The time that the recommendation report generation task completes.
    completionTime :: Prelude.Maybe Data.POSIX,
    -- | The S3 bucket where the report file is located.
    s3Bucket :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 key name of the report file.
    s3Keys :: Prelude.Maybe [Prelude.Text],
    -- | The time that the recommendation report generation task starts.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The status of the recommendation report generation task.
    status :: Prelude.Maybe RecommendationReportStatus,
    -- | The status message for recommendation report generation.
    statusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommendationReportDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completionTime', 'recommendationReportDetails_completionTime' - The time that the recommendation report generation task completes.
--
-- 's3Bucket', 'recommendationReportDetails_s3Bucket' - The S3 bucket where the report file is located.
--
-- 's3Keys', 'recommendationReportDetails_s3Keys' - The Amazon S3 key name of the report file.
--
-- 'startTime', 'recommendationReportDetails_startTime' - The time that the recommendation report generation task starts.
--
-- 'status', 'recommendationReportDetails_status' - The status of the recommendation report generation task.
--
-- 'statusMessage', 'recommendationReportDetails_statusMessage' - The status message for recommendation report generation.
newRecommendationReportDetails ::
  RecommendationReportDetails
newRecommendationReportDetails =
  RecommendationReportDetails'
    { completionTime =
        Prelude.Nothing,
      s3Bucket = Prelude.Nothing,
      s3Keys = Prelude.Nothing,
      startTime = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | The time that the recommendation report generation task completes.
recommendationReportDetails_completionTime :: Lens.Lens' RecommendationReportDetails (Prelude.Maybe Prelude.UTCTime)
recommendationReportDetails_completionTime = Lens.lens (\RecommendationReportDetails' {completionTime} -> completionTime) (\s@RecommendationReportDetails' {} a -> s {completionTime = a} :: RecommendationReportDetails) Prelude.. Lens.mapping Data._Time

-- | The S3 bucket where the report file is located.
recommendationReportDetails_s3Bucket :: Lens.Lens' RecommendationReportDetails (Prelude.Maybe Prelude.Text)
recommendationReportDetails_s3Bucket = Lens.lens (\RecommendationReportDetails' {s3Bucket} -> s3Bucket) (\s@RecommendationReportDetails' {} a -> s {s3Bucket = a} :: RecommendationReportDetails)

-- | The Amazon S3 key name of the report file.
recommendationReportDetails_s3Keys :: Lens.Lens' RecommendationReportDetails (Prelude.Maybe [Prelude.Text])
recommendationReportDetails_s3Keys = Lens.lens (\RecommendationReportDetails' {s3Keys} -> s3Keys) (\s@RecommendationReportDetails' {} a -> s {s3Keys = a} :: RecommendationReportDetails) Prelude.. Lens.mapping Lens.coerced

-- | The time that the recommendation report generation task starts.
recommendationReportDetails_startTime :: Lens.Lens' RecommendationReportDetails (Prelude.Maybe Prelude.UTCTime)
recommendationReportDetails_startTime = Lens.lens (\RecommendationReportDetails' {startTime} -> startTime) (\s@RecommendationReportDetails' {} a -> s {startTime = a} :: RecommendationReportDetails) Prelude.. Lens.mapping Data._Time

-- | The status of the recommendation report generation task.
recommendationReportDetails_status :: Lens.Lens' RecommendationReportDetails (Prelude.Maybe RecommendationReportStatus)
recommendationReportDetails_status = Lens.lens (\RecommendationReportDetails' {status} -> status) (\s@RecommendationReportDetails' {} a -> s {status = a} :: RecommendationReportDetails)

-- | The status message for recommendation report generation.
recommendationReportDetails_statusMessage :: Lens.Lens' RecommendationReportDetails (Prelude.Maybe Prelude.Text)
recommendationReportDetails_statusMessage = Lens.lens (\RecommendationReportDetails' {statusMessage} -> statusMessage) (\s@RecommendationReportDetails' {} a -> s {statusMessage = a} :: RecommendationReportDetails)

instance Data.FromJSON RecommendationReportDetails where
  parseJSON =
    Data.withObject
      "RecommendationReportDetails"
      ( \x ->
          RecommendationReportDetails'
            Prelude.<$> (x Data..:? "completionTime")
            Prelude.<*> (x Data..:? "s3Bucket")
            Prelude.<*> (x Data..:? "s3Keys" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "startTime")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "statusMessage")
      )

instance Prelude.Hashable RecommendationReportDetails where
  hashWithSalt _salt RecommendationReportDetails' {..} =
    _salt `Prelude.hashWithSalt` completionTime
      `Prelude.hashWithSalt` s3Bucket
      `Prelude.hashWithSalt` s3Keys
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData RecommendationReportDetails where
  rnf RecommendationReportDetails' {..} =
    Prelude.rnf completionTime
      `Prelude.seq` Prelude.rnf s3Bucket
      `Prelude.seq` Prelude.rnf s3Keys
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
