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
-- Module      : Amazonka.MacieV2.Types.ClassificationResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.ClassificationResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.ClassificationResultStatus
import Amazonka.MacieV2.Types.CustomDataIdentifiers
import Amazonka.MacieV2.Types.SensitiveDataItem
import qualified Amazonka.Prelude as Prelude

-- | Provides the details of a sensitive data finding, including the types,
-- number of occurrences, and locations of the sensitive data that was
-- detected.
--
-- /See:/ 'newClassificationResult' smart constructor.
data ClassificationResult = ClassificationResult'
  { -- | Specifies whether Amazon Macie detected additional occurrences of
    -- sensitive data in the S3 object. A finding includes location data for a
    -- maximum of 15 occurrences of sensitive data.
    --
    -- This value can help you determine whether to investigate additional
    -- occurrences of sensitive data in an object. You can do this by referring
    -- to the corresponding sensitive data discovery result for the finding
    -- (ClassificationDetails.detailedResultsLocation).
    additionalOccurrences :: Prelude.Maybe Prelude.Bool,
    -- | The custom data identifiers that detected the sensitive data and the
    -- number of occurrences of the data that they detected.
    customDataIdentifiers :: Prelude.Maybe CustomDataIdentifiers,
    -- | The type of content, as a MIME type, that the finding applies to. For
    -- example, application\/gzip, for a GNU Gzip compressed archive file, or
    -- application\/pdf, for an Adobe Portable Document Format file.
    mimeType :: Prelude.Maybe Prelude.Text,
    -- | The category, types, and number of occurrences of the sensitive data
    -- that produced the finding.
    sensitiveData :: Prelude.Maybe [SensitiveDataItem],
    -- | The total size, in bytes, of the data that the finding applies to.
    sizeClassified :: Prelude.Maybe Prelude.Integer,
    -- | The status of the finding.
    status :: Prelude.Maybe ClassificationResultStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClassificationResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalOccurrences', 'classificationResult_additionalOccurrences' - Specifies whether Amazon Macie detected additional occurrences of
-- sensitive data in the S3 object. A finding includes location data for a
-- maximum of 15 occurrences of sensitive data.
--
-- This value can help you determine whether to investigate additional
-- occurrences of sensitive data in an object. You can do this by referring
-- to the corresponding sensitive data discovery result for the finding
-- (ClassificationDetails.detailedResultsLocation).
--
-- 'customDataIdentifiers', 'classificationResult_customDataIdentifiers' - The custom data identifiers that detected the sensitive data and the
-- number of occurrences of the data that they detected.
--
-- 'mimeType', 'classificationResult_mimeType' - The type of content, as a MIME type, that the finding applies to. For
-- example, application\/gzip, for a GNU Gzip compressed archive file, or
-- application\/pdf, for an Adobe Portable Document Format file.
--
-- 'sensitiveData', 'classificationResult_sensitiveData' - The category, types, and number of occurrences of the sensitive data
-- that produced the finding.
--
-- 'sizeClassified', 'classificationResult_sizeClassified' - The total size, in bytes, of the data that the finding applies to.
--
-- 'status', 'classificationResult_status' - The status of the finding.
newClassificationResult ::
  ClassificationResult
newClassificationResult =
  ClassificationResult'
    { additionalOccurrences =
        Prelude.Nothing,
      customDataIdentifiers = Prelude.Nothing,
      mimeType = Prelude.Nothing,
      sensitiveData = Prelude.Nothing,
      sizeClassified = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Specifies whether Amazon Macie detected additional occurrences of
-- sensitive data in the S3 object. A finding includes location data for a
-- maximum of 15 occurrences of sensitive data.
--
-- This value can help you determine whether to investigate additional
-- occurrences of sensitive data in an object. You can do this by referring
-- to the corresponding sensitive data discovery result for the finding
-- (ClassificationDetails.detailedResultsLocation).
classificationResult_additionalOccurrences :: Lens.Lens' ClassificationResult (Prelude.Maybe Prelude.Bool)
classificationResult_additionalOccurrences = Lens.lens (\ClassificationResult' {additionalOccurrences} -> additionalOccurrences) (\s@ClassificationResult' {} a -> s {additionalOccurrences = a} :: ClassificationResult)

-- | The custom data identifiers that detected the sensitive data and the
-- number of occurrences of the data that they detected.
classificationResult_customDataIdentifiers :: Lens.Lens' ClassificationResult (Prelude.Maybe CustomDataIdentifiers)
classificationResult_customDataIdentifiers = Lens.lens (\ClassificationResult' {customDataIdentifiers} -> customDataIdentifiers) (\s@ClassificationResult' {} a -> s {customDataIdentifiers = a} :: ClassificationResult)

-- | The type of content, as a MIME type, that the finding applies to. For
-- example, application\/gzip, for a GNU Gzip compressed archive file, or
-- application\/pdf, for an Adobe Portable Document Format file.
classificationResult_mimeType :: Lens.Lens' ClassificationResult (Prelude.Maybe Prelude.Text)
classificationResult_mimeType = Lens.lens (\ClassificationResult' {mimeType} -> mimeType) (\s@ClassificationResult' {} a -> s {mimeType = a} :: ClassificationResult)

-- | The category, types, and number of occurrences of the sensitive data
-- that produced the finding.
classificationResult_sensitiveData :: Lens.Lens' ClassificationResult (Prelude.Maybe [SensitiveDataItem])
classificationResult_sensitiveData = Lens.lens (\ClassificationResult' {sensitiveData} -> sensitiveData) (\s@ClassificationResult' {} a -> s {sensitiveData = a} :: ClassificationResult) Prelude.. Lens.mapping Lens.coerced

-- | The total size, in bytes, of the data that the finding applies to.
classificationResult_sizeClassified :: Lens.Lens' ClassificationResult (Prelude.Maybe Prelude.Integer)
classificationResult_sizeClassified = Lens.lens (\ClassificationResult' {sizeClassified} -> sizeClassified) (\s@ClassificationResult' {} a -> s {sizeClassified = a} :: ClassificationResult)

-- | The status of the finding.
classificationResult_status :: Lens.Lens' ClassificationResult (Prelude.Maybe ClassificationResultStatus)
classificationResult_status = Lens.lens (\ClassificationResult' {status} -> status) (\s@ClassificationResult' {} a -> s {status = a} :: ClassificationResult)

instance Data.FromJSON ClassificationResult where
  parseJSON =
    Data.withObject
      "ClassificationResult"
      ( \x ->
          ClassificationResult'
            Prelude.<$> (x Data..:? "additionalOccurrences")
            Prelude.<*> (x Data..:? "customDataIdentifiers")
            Prelude.<*> (x Data..:? "mimeType")
            Prelude.<*> (x Data..:? "sensitiveData" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "sizeClassified")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable ClassificationResult where
  hashWithSalt _salt ClassificationResult' {..} =
    _salt
      `Prelude.hashWithSalt` additionalOccurrences
      `Prelude.hashWithSalt` customDataIdentifiers
      `Prelude.hashWithSalt` mimeType
      `Prelude.hashWithSalt` sensitiveData
      `Prelude.hashWithSalt` sizeClassified
      `Prelude.hashWithSalt` status

instance Prelude.NFData ClassificationResult where
  rnf ClassificationResult' {..} =
    Prelude.rnf additionalOccurrences `Prelude.seq`
      Prelude.rnf customDataIdentifiers `Prelude.seq`
        Prelude.rnf mimeType `Prelude.seq`
          Prelude.rnf sensitiveData `Prelude.seq`
            Prelude.rnf sizeClassified `Prelude.seq`
              Prelude.rnf status
