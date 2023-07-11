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
-- Module      : Amazonka.SecurityHub.Types.ClassificationResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.ClassificationResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.ClassificationStatus
import Amazonka.SecurityHub.Types.CustomDataIdentifiersResult
import Amazonka.SecurityHub.Types.SensitiveDataResult

-- | Details about the sensitive data that was detected on the resource.
--
-- /See:/ 'newClassificationResult' smart constructor.
data ClassificationResult = ClassificationResult'
  { -- | Indicates whether there are additional occurrences of sensitive data
    -- that are not included in the finding. This occurs when the number of
    -- occurrences exceeds the maximum that can be included.
    additionalOccurrences :: Prelude.Maybe Prelude.Bool,
    -- | Provides details about sensitive data that was identified based on
    -- customer-defined configuration.
    customDataIdentifiers :: Prelude.Maybe CustomDataIdentifiersResult,
    -- | The type of content that the finding applies to.
    mimeType :: Prelude.Maybe Prelude.Text,
    -- | Provides details about sensitive data that was identified based on
    -- built-in configuration.
    sensitiveData :: Prelude.Maybe [SensitiveDataResult],
    -- | The total size in bytes of the affected data.
    sizeClassified :: Prelude.Maybe Prelude.Integer,
    -- | The current status of the sensitive data detection.
    status :: Prelude.Maybe ClassificationStatus
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
-- 'additionalOccurrences', 'classificationResult_additionalOccurrences' - Indicates whether there are additional occurrences of sensitive data
-- that are not included in the finding. This occurs when the number of
-- occurrences exceeds the maximum that can be included.
--
-- 'customDataIdentifiers', 'classificationResult_customDataIdentifiers' - Provides details about sensitive data that was identified based on
-- customer-defined configuration.
--
-- 'mimeType', 'classificationResult_mimeType' - The type of content that the finding applies to.
--
-- 'sensitiveData', 'classificationResult_sensitiveData' - Provides details about sensitive data that was identified based on
-- built-in configuration.
--
-- 'sizeClassified', 'classificationResult_sizeClassified' - The total size in bytes of the affected data.
--
-- 'status', 'classificationResult_status' - The current status of the sensitive data detection.
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

-- | Indicates whether there are additional occurrences of sensitive data
-- that are not included in the finding. This occurs when the number of
-- occurrences exceeds the maximum that can be included.
classificationResult_additionalOccurrences :: Lens.Lens' ClassificationResult (Prelude.Maybe Prelude.Bool)
classificationResult_additionalOccurrences = Lens.lens (\ClassificationResult' {additionalOccurrences} -> additionalOccurrences) (\s@ClassificationResult' {} a -> s {additionalOccurrences = a} :: ClassificationResult)

-- | Provides details about sensitive data that was identified based on
-- customer-defined configuration.
classificationResult_customDataIdentifiers :: Lens.Lens' ClassificationResult (Prelude.Maybe CustomDataIdentifiersResult)
classificationResult_customDataIdentifiers = Lens.lens (\ClassificationResult' {customDataIdentifiers} -> customDataIdentifiers) (\s@ClassificationResult' {} a -> s {customDataIdentifiers = a} :: ClassificationResult)

-- | The type of content that the finding applies to.
classificationResult_mimeType :: Lens.Lens' ClassificationResult (Prelude.Maybe Prelude.Text)
classificationResult_mimeType = Lens.lens (\ClassificationResult' {mimeType} -> mimeType) (\s@ClassificationResult' {} a -> s {mimeType = a} :: ClassificationResult)

-- | Provides details about sensitive data that was identified based on
-- built-in configuration.
classificationResult_sensitiveData :: Lens.Lens' ClassificationResult (Prelude.Maybe [SensitiveDataResult])
classificationResult_sensitiveData = Lens.lens (\ClassificationResult' {sensitiveData} -> sensitiveData) (\s@ClassificationResult' {} a -> s {sensitiveData = a} :: ClassificationResult) Prelude.. Lens.mapping Lens.coerced

-- | The total size in bytes of the affected data.
classificationResult_sizeClassified :: Lens.Lens' ClassificationResult (Prelude.Maybe Prelude.Integer)
classificationResult_sizeClassified = Lens.lens (\ClassificationResult' {sizeClassified} -> sizeClassified) (\s@ClassificationResult' {} a -> s {sizeClassified = a} :: ClassificationResult)

-- | The current status of the sensitive data detection.
classificationResult_status :: Lens.Lens' ClassificationResult (Prelude.Maybe ClassificationStatus)
classificationResult_status = Lens.lens (\ClassificationResult' {status} -> status) (\s@ClassificationResult' {} a -> s {status = a} :: ClassificationResult)

instance Data.FromJSON ClassificationResult where
  parseJSON =
    Data.withObject
      "ClassificationResult"
      ( \x ->
          ClassificationResult'
            Prelude.<$> (x Data..:? "AdditionalOccurrences")
            Prelude.<*> (x Data..:? "CustomDataIdentifiers")
            Prelude.<*> (x Data..:? "MimeType")
            Prelude.<*> (x Data..:? "SensitiveData" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SizeClassified")
            Prelude.<*> (x Data..:? "Status")
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
    Prelude.rnf additionalOccurrences
      `Prelude.seq` Prelude.rnf customDataIdentifiers
      `Prelude.seq` Prelude.rnf mimeType
      `Prelude.seq` Prelude.rnf sensitiveData
      `Prelude.seq` Prelude.rnf sizeClassified
      `Prelude.seq` Prelude.rnf status

instance Data.ToJSON ClassificationResult where
  toJSON ClassificationResult' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdditionalOccurrences" Data..=)
              Prelude.<$> additionalOccurrences,
            ("CustomDataIdentifiers" Data..=)
              Prelude.<$> customDataIdentifiers,
            ("MimeType" Data..=) Prelude.<$> mimeType,
            ("SensitiveData" Data..=) Prelude.<$> sensitiveData,
            ("SizeClassified" Data..=)
              Prelude.<$> sizeClassified,
            ("Status" Data..=) Prelude.<$> status
          ]
      )
