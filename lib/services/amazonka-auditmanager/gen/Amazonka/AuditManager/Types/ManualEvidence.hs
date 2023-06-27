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
-- Module      : Amazonka.AuditManager.Types.ManualEvidence
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.ManualEvidence where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Evidence that\'s manually added to a control in Audit Manager.
-- @manualEvidence@ can be one of the following: @evidenceFileName@,
-- @s3ResourcePath@, or @textResponse@.
--
-- /See:/ 'newManualEvidence' smart constructor.
data ManualEvidence = ManualEvidence'
  { -- | The name of the file that\'s uploaded as manual evidence. This name is
    -- populated using the @evidenceFileName@ value from the
    -- <https://docs.aws.amazon.com/audit-manager/latest/APIReference/API_GetEvidenceFileUploadUrl.html GetEvidenceFileUploadUrl>
    -- API response.
    evidenceFileName :: Prelude.Maybe Prelude.Text,
    -- | The S3 URL of the object that\'s imported as manual evidence.
    s3ResourcePath :: Prelude.Maybe Prelude.Text,
    -- | The plain text response that\'s entered and saved as manual evidence.
    textResponse :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ManualEvidence' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evidenceFileName', 'manualEvidence_evidenceFileName' - The name of the file that\'s uploaded as manual evidence. This name is
-- populated using the @evidenceFileName@ value from the
-- <https://docs.aws.amazon.com/audit-manager/latest/APIReference/API_GetEvidenceFileUploadUrl.html GetEvidenceFileUploadUrl>
-- API response.
--
-- 's3ResourcePath', 'manualEvidence_s3ResourcePath' - The S3 URL of the object that\'s imported as manual evidence.
--
-- 'textResponse', 'manualEvidence_textResponse' - The plain text response that\'s entered and saved as manual evidence.
newManualEvidence ::
  ManualEvidence
newManualEvidence =
  ManualEvidence'
    { evidenceFileName = Prelude.Nothing,
      s3ResourcePath = Prelude.Nothing,
      textResponse = Prelude.Nothing
    }

-- | The name of the file that\'s uploaded as manual evidence. This name is
-- populated using the @evidenceFileName@ value from the
-- <https://docs.aws.amazon.com/audit-manager/latest/APIReference/API_GetEvidenceFileUploadUrl.html GetEvidenceFileUploadUrl>
-- API response.
manualEvidence_evidenceFileName :: Lens.Lens' ManualEvidence (Prelude.Maybe Prelude.Text)
manualEvidence_evidenceFileName = Lens.lens (\ManualEvidence' {evidenceFileName} -> evidenceFileName) (\s@ManualEvidence' {} a -> s {evidenceFileName = a} :: ManualEvidence)

-- | The S3 URL of the object that\'s imported as manual evidence.
manualEvidence_s3ResourcePath :: Lens.Lens' ManualEvidence (Prelude.Maybe Prelude.Text)
manualEvidence_s3ResourcePath = Lens.lens (\ManualEvidence' {s3ResourcePath} -> s3ResourcePath) (\s@ManualEvidence' {} a -> s {s3ResourcePath = a} :: ManualEvidence)

-- | The plain text response that\'s entered and saved as manual evidence.
manualEvidence_textResponse :: Lens.Lens' ManualEvidence (Prelude.Maybe Prelude.Text)
manualEvidence_textResponse = Lens.lens (\ManualEvidence' {textResponse} -> textResponse) (\s@ManualEvidence' {} a -> s {textResponse = a} :: ManualEvidence)

instance Data.FromJSON ManualEvidence where
  parseJSON =
    Data.withObject
      "ManualEvidence"
      ( \x ->
          ManualEvidence'
            Prelude.<$> (x Data..:? "evidenceFileName")
            Prelude.<*> (x Data..:? "s3ResourcePath")
            Prelude.<*> (x Data..:? "textResponse")
      )

instance Prelude.Hashable ManualEvidence where
  hashWithSalt _salt ManualEvidence' {..} =
    _salt
      `Prelude.hashWithSalt` evidenceFileName
      `Prelude.hashWithSalt` s3ResourcePath
      `Prelude.hashWithSalt` textResponse

instance Prelude.NFData ManualEvidence where
  rnf ManualEvidence' {..} =
    Prelude.rnf evidenceFileName
      `Prelude.seq` Prelude.rnf s3ResourcePath
      `Prelude.seq` Prelude.rnf textResponse

instance Data.ToJSON ManualEvidence where
  toJSON ManualEvidence' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("evidenceFileName" Data..=)
              Prelude.<$> evidenceFileName,
            ("s3ResourcePath" Data..=)
              Prelude.<$> s3ResourcePath,
            ("textResponse" Data..=) Prelude.<$> textResponse
          ]
      )
