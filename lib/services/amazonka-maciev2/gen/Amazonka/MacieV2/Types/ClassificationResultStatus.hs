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
-- Module      : Amazonka.MacieV2.Types.ClassificationResultStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.ClassificationResultStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the status of a sensitive data finding.
--
-- /See:/ 'newClassificationResultStatus' smart constructor.
data ClassificationResultStatus = ClassificationResultStatus'
  { -- | A brief description of the status of the finding. Amazon Macie uses this
    -- value to notify you of any errors, warnings, or considerations that
    -- might impact your analysis of the finding.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The status of the finding. Possible values are:
    --
    -- -   COMPLETE - Amazon Macie successfully completed its analysis of the
    --     S3 object that the finding applies to.
    --
    -- -   PARTIAL - Macie analyzed only a subset of the data in the S3 object
    --     that the finding applies to. For example, the object is an archive
    --     file that contains files in an unsupported format.
    --
    -- -   SKIPPED - Macie wasn\'t able to analyze the S3 object that the
    --     finding applies to. For example, the object is a file in an
    --     unsupported format.
    code :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClassificationResultStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reason', 'classificationResultStatus_reason' - A brief description of the status of the finding. Amazon Macie uses this
-- value to notify you of any errors, warnings, or considerations that
-- might impact your analysis of the finding.
--
-- 'code', 'classificationResultStatus_code' - The status of the finding. Possible values are:
--
-- -   COMPLETE - Amazon Macie successfully completed its analysis of the
--     S3 object that the finding applies to.
--
-- -   PARTIAL - Macie analyzed only a subset of the data in the S3 object
--     that the finding applies to. For example, the object is an archive
--     file that contains files in an unsupported format.
--
-- -   SKIPPED - Macie wasn\'t able to analyze the S3 object that the
--     finding applies to. For example, the object is a file in an
--     unsupported format.
newClassificationResultStatus ::
  ClassificationResultStatus
newClassificationResultStatus =
  ClassificationResultStatus'
    { reason =
        Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | A brief description of the status of the finding. Amazon Macie uses this
-- value to notify you of any errors, warnings, or considerations that
-- might impact your analysis of the finding.
classificationResultStatus_reason :: Lens.Lens' ClassificationResultStatus (Prelude.Maybe Prelude.Text)
classificationResultStatus_reason = Lens.lens (\ClassificationResultStatus' {reason} -> reason) (\s@ClassificationResultStatus' {} a -> s {reason = a} :: ClassificationResultStatus)

-- | The status of the finding. Possible values are:
--
-- -   COMPLETE - Amazon Macie successfully completed its analysis of the
--     S3 object that the finding applies to.
--
-- -   PARTIAL - Macie analyzed only a subset of the data in the S3 object
--     that the finding applies to. For example, the object is an archive
--     file that contains files in an unsupported format.
--
-- -   SKIPPED - Macie wasn\'t able to analyze the S3 object that the
--     finding applies to. For example, the object is a file in an
--     unsupported format.
classificationResultStatus_code :: Lens.Lens' ClassificationResultStatus (Prelude.Maybe Prelude.Text)
classificationResultStatus_code = Lens.lens (\ClassificationResultStatus' {code} -> code) (\s@ClassificationResultStatus' {} a -> s {code = a} :: ClassificationResultStatus)

instance Core.FromJSON ClassificationResultStatus where
  parseJSON =
    Core.withObject
      "ClassificationResultStatus"
      ( \x ->
          ClassificationResultStatus'
            Prelude.<$> (x Core..:? "reason")
            Prelude.<*> (x Core..:? "code")
      )

instance Prelude.Hashable ClassificationResultStatus where
  hashWithSalt salt' ClassificationResultStatus' {..} =
    salt' `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` reason

instance Prelude.NFData ClassificationResultStatus where
  rnf ClassificationResultStatus' {..} =
    Prelude.rnf reason `Prelude.seq` Prelude.rnf code
