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
-- Module      : Amazonka.MacieV2.Types.ClassificationDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.ClassificationDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MacieV2.Types.ClassificationResult
import Amazonka.MacieV2.Types.OriginType
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a sensitive data finding and the details of
-- the finding.
--
-- /See:/ 'newClassificationDetails' smart constructor.
data ClassificationDetails = ClassificationDetails'
  { -- | Specifies how Amazon Macie found the sensitive data that produced the
    -- finding: SENSITIVE_DATA_DISCOVERY_JOB, for a classification job.
    originType :: Prelude.Maybe OriginType,
    -- | The unique identifier for the classification job that produced the
    -- finding.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The path to the folder or file (in Amazon S3) that contains the
    -- corresponding sensitive data discovery result for the finding. If a
    -- finding applies to a large archive or compressed file, this value is the
    -- path to a folder. Otherwise, this value is the path to a file.
    detailedResultsLocation :: Prelude.Maybe Prelude.Text,
    -- | The status and other details of the finding.
    result :: Prelude.Maybe ClassificationResult,
    -- | The Amazon Resource Name (ARN) of the classification job that produced
    -- the finding.
    jobArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClassificationDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'originType', 'classificationDetails_originType' - Specifies how Amazon Macie found the sensitive data that produced the
-- finding: SENSITIVE_DATA_DISCOVERY_JOB, for a classification job.
--
-- 'jobId', 'classificationDetails_jobId' - The unique identifier for the classification job that produced the
-- finding.
--
-- 'detailedResultsLocation', 'classificationDetails_detailedResultsLocation' - The path to the folder or file (in Amazon S3) that contains the
-- corresponding sensitive data discovery result for the finding. If a
-- finding applies to a large archive or compressed file, this value is the
-- path to a folder. Otherwise, this value is the path to a file.
--
-- 'result', 'classificationDetails_result' - The status and other details of the finding.
--
-- 'jobArn', 'classificationDetails_jobArn' - The Amazon Resource Name (ARN) of the classification job that produced
-- the finding.
newClassificationDetails ::
  ClassificationDetails
newClassificationDetails =
  ClassificationDetails'
    { originType =
        Prelude.Nothing,
      jobId = Prelude.Nothing,
      detailedResultsLocation = Prelude.Nothing,
      result = Prelude.Nothing,
      jobArn = Prelude.Nothing
    }

-- | Specifies how Amazon Macie found the sensitive data that produced the
-- finding: SENSITIVE_DATA_DISCOVERY_JOB, for a classification job.
classificationDetails_originType :: Lens.Lens' ClassificationDetails (Prelude.Maybe OriginType)
classificationDetails_originType = Lens.lens (\ClassificationDetails' {originType} -> originType) (\s@ClassificationDetails' {} a -> s {originType = a} :: ClassificationDetails)

-- | The unique identifier for the classification job that produced the
-- finding.
classificationDetails_jobId :: Lens.Lens' ClassificationDetails (Prelude.Maybe Prelude.Text)
classificationDetails_jobId = Lens.lens (\ClassificationDetails' {jobId} -> jobId) (\s@ClassificationDetails' {} a -> s {jobId = a} :: ClassificationDetails)

-- | The path to the folder or file (in Amazon S3) that contains the
-- corresponding sensitive data discovery result for the finding. If a
-- finding applies to a large archive or compressed file, this value is the
-- path to a folder. Otherwise, this value is the path to a file.
classificationDetails_detailedResultsLocation :: Lens.Lens' ClassificationDetails (Prelude.Maybe Prelude.Text)
classificationDetails_detailedResultsLocation = Lens.lens (\ClassificationDetails' {detailedResultsLocation} -> detailedResultsLocation) (\s@ClassificationDetails' {} a -> s {detailedResultsLocation = a} :: ClassificationDetails)

-- | The status and other details of the finding.
classificationDetails_result :: Lens.Lens' ClassificationDetails (Prelude.Maybe ClassificationResult)
classificationDetails_result = Lens.lens (\ClassificationDetails' {result} -> result) (\s@ClassificationDetails' {} a -> s {result = a} :: ClassificationDetails)

-- | The Amazon Resource Name (ARN) of the classification job that produced
-- the finding.
classificationDetails_jobArn :: Lens.Lens' ClassificationDetails (Prelude.Maybe Prelude.Text)
classificationDetails_jobArn = Lens.lens (\ClassificationDetails' {jobArn} -> jobArn) (\s@ClassificationDetails' {} a -> s {jobArn = a} :: ClassificationDetails)

instance Core.FromJSON ClassificationDetails where
  parseJSON =
    Core.withObject
      "ClassificationDetails"
      ( \x ->
          ClassificationDetails'
            Prelude.<$> (x Core..:? "originType")
            Prelude.<*> (x Core..:? "jobId")
            Prelude.<*> (x Core..:? "detailedResultsLocation")
            Prelude.<*> (x Core..:? "result")
            Prelude.<*> (x Core..:? "jobArn")
      )

instance Prelude.Hashable ClassificationDetails where
  hashWithSalt _salt ClassificationDetails' {..} =
    _salt `Prelude.hashWithSalt` originType
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` detailedResultsLocation
      `Prelude.hashWithSalt` result
      `Prelude.hashWithSalt` jobArn

instance Prelude.NFData ClassificationDetails where
  rnf ClassificationDetails' {..} =
    Prelude.rnf originType
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf detailedResultsLocation
      `Prelude.seq` Prelude.rnf result
      `Prelude.seq` Prelude.rnf jobArn
