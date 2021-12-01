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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.ClassificationDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MacieV2.Types.ClassificationResult
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a sensitive data finding, including the
-- classification job that produced the finding.
--
-- /See:/ 'newClassificationDetails' smart constructor.
data ClassificationDetails = ClassificationDetails'
  { -- | The path to the folder or file (in Amazon S3) that contains the
    -- corresponding sensitive data discovery result for the finding. If a
    -- finding applies to a large archive or compressed file, this value is the
    -- path to a folder. Otherwise, this value is the path to a file.
    detailedResultsLocation :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the classification job that produced the
    -- finding.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the classification job that produced
    -- the finding.
    jobArn :: Prelude.Maybe Prelude.Text,
    -- | The status and other details of the finding.
    result :: Prelude.Maybe ClassificationResult
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
-- 'detailedResultsLocation', 'classificationDetails_detailedResultsLocation' - The path to the folder or file (in Amazon S3) that contains the
-- corresponding sensitive data discovery result for the finding. If a
-- finding applies to a large archive or compressed file, this value is the
-- path to a folder. Otherwise, this value is the path to a file.
--
-- 'jobId', 'classificationDetails_jobId' - The unique identifier for the classification job that produced the
-- finding.
--
-- 'jobArn', 'classificationDetails_jobArn' - The Amazon Resource Name (ARN) of the classification job that produced
-- the finding.
--
-- 'result', 'classificationDetails_result' - The status and other details of the finding.
newClassificationDetails ::
  ClassificationDetails
newClassificationDetails =
  ClassificationDetails'
    { detailedResultsLocation =
        Prelude.Nothing,
      jobId = Prelude.Nothing,
      jobArn = Prelude.Nothing,
      result = Prelude.Nothing
    }

-- | The path to the folder or file (in Amazon S3) that contains the
-- corresponding sensitive data discovery result for the finding. If a
-- finding applies to a large archive or compressed file, this value is the
-- path to a folder. Otherwise, this value is the path to a file.
classificationDetails_detailedResultsLocation :: Lens.Lens' ClassificationDetails (Prelude.Maybe Prelude.Text)
classificationDetails_detailedResultsLocation = Lens.lens (\ClassificationDetails' {detailedResultsLocation} -> detailedResultsLocation) (\s@ClassificationDetails' {} a -> s {detailedResultsLocation = a} :: ClassificationDetails)

-- | The unique identifier for the classification job that produced the
-- finding.
classificationDetails_jobId :: Lens.Lens' ClassificationDetails (Prelude.Maybe Prelude.Text)
classificationDetails_jobId = Lens.lens (\ClassificationDetails' {jobId} -> jobId) (\s@ClassificationDetails' {} a -> s {jobId = a} :: ClassificationDetails)

-- | The Amazon Resource Name (ARN) of the classification job that produced
-- the finding.
classificationDetails_jobArn :: Lens.Lens' ClassificationDetails (Prelude.Maybe Prelude.Text)
classificationDetails_jobArn = Lens.lens (\ClassificationDetails' {jobArn} -> jobArn) (\s@ClassificationDetails' {} a -> s {jobArn = a} :: ClassificationDetails)

-- | The status and other details of the finding.
classificationDetails_result :: Lens.Lens' ClassificationDetails (Prelude.Maybe ClassificationResult)
classificationDetails_result = Lens.lens (\ClassificationDetails' {result} -> result) (\s@ClassificationDetails' {} a -> s {result = a} :: ClassificationDetails)

instance Core.FromJSON ClassificationDetails where
  parseJSON =
    Core.withObject
      "ClassificationDetails"
      ( \x ->
          ClassificationDetails'
            Prelude.<$> (x Core..:? "detailedResultsLocation")
            Prelude.<*> (x Core..:? "jobId")
            Prelude.<*> (x Core..:? "jobArn")
            Prelude.<*> (x Core..:? "result")
      )

instance Prelude.Hashable ClassificationDetails where
  hashWithSalt salt' ClassificationDetails' {..} =
    salt' `Prelude.hashWithSalt` result
      `Prelude.hashWithSalt` jobArn
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` detailedResultsLocation

instance Prelude.NFData ClassificationDetails where
  rnf ClassificationDetails' {..} =
    Prelude.rnf detailedResultsLocation
      `Prelude.seq` Prelude.rnf result
      `Prelude.seq` Prelude.rnf jobArn
      `Prelude.seq` Prelude.rnf jobId
