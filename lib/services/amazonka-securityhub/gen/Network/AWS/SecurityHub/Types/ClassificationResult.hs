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
-- Module      : Network.AWS.SecurityHub.Types.ClassificationResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.ClassificationResult where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SecurityHub.Types.ClassificationStatus
import Network.AWS.SecurityHub.Types.CustomDataIdentifiersResult
import Network.AWS.SecurityHub.Types.SensitiveDataResult

-- | Details about the sensitive data that was detected on the resource.
--
-- /See:/ 'newClassificationResult' smart constructor.
data ClassificationResult = ClassificationResult'
  { -- | The current status of the sensitive data detection.
    status :: Prelude.Maybe ClassificationStatus,
    -- | Provides details about sensitive data that was identified based on
    -- built-in configuration.
    sensitiveData :: Prelude.Maybe [SensitiveDataResult],
    -- | The type of content that the finding applies to.
    mimeType :: Prelude.Maybe Prelude.Text,
    -- | The total size in bytes of the affected data.
    sizeClassified :: Prelude.Maybe Prelude.Integer,
    -- | Indicates whether there are additional occurrences of sensitive data
    -- that are not included in the finding. This occurs when the number of
    -- occurrences exceeds the maximum that can be included.
    additionalOccurrences :: Prelude.Maybe Prelude.Bool,
    -- | Provides details about sensitive data that was identified based on
    -- customer-defined configuration.
    customDataIdentifiers :: Prelude.Maybe CustomDataIdentifiersResult
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
-- 'status', 'classificationResult_status' - The current status of the sensitive data detection.
--
-- 'sensitiveData', 'classificationResult_sensitiveData' - Provides details about sensitive data that was identified based on
-- built-in configuration.
--
-- 'mimeType', 'classificationResult_mimeType' - The type of content that the finding applies to.
--
-- 'sizeClassified', 'classificationResult_sizeClassified' - The total size in bytes of the affected data.
--
-- 'additionalOccurrences', 'classificationResult_additionalOccurrences' - Indicates whether there are additional occurrences of sensitive data
-- that are not included in the finding. This occurs when the number of
-- occurrences exceeds the maximum that can be included.
--
-- 'customDataIdentifiers', 'classificationResult_customDataIdentifiers' - Provides details about sensitive data that was identified based on
-- customer-defined configuration.
newClassificationResult ::
  ClassificationResult
newClassificationResult =
  ClassificationResult'
    { status = Prelude.Nothing,
      sensitiveData = Prelude.Nothing,
      mimeType = Prelude.Nothing,
      sizeClassified = Prelude.Nothing,
      additionalOccurrences = Prelude.Nothing,
      customDataIdentifiers = Prelude.Nothing
    }

-- | The current status of the sensitive data detection.
classificationResult_status :: Lens.Lens' ClassificationResult (Prelude.Maybe ClassificationStatus)
classificationResult_status = Lens.lens (\ClassificationResult' {status} -> status) (\s@ClassificationResult' {} a -> s {status = a} :: ClassificationResult)

-- | Provides details about sensitive data that was identified based on
-- built-in configuration.
classificationResult_sensitiveData :: Lens.Lens' ClassificationResult (Prelude.Maybe [SensitiveDataResult])
classificationResult_sensitiveData = Lens.lens (\ClassificationResult' {sensitiveData} -> sensitiveData) (\s@ClassificationResult' {} a -> s {sensitiveData = a} :: ClassificationResult) Prelude.. Lens.mapping Lens.coerced

-- | The type of content that the finding applies to.
classificationResult_mimeType :: Lens.Lens' ClassificationResult (Prelude.Maybe Prelude.Text)
classificationResult_mimeType = Lens.lens (\ClassificationResult' {mimeType} -> mimeType) (\s@ClassificationResult' {} a -> s {mimeType = a} :: ClassificationResult)

-- | The total size in bytes of the affected data.
classificationResult_sizeClassified :: Lens.Lens' ClassificationResult (Prelude.Maybe Prelude.Integer)
classificationResult_sizeClassified = Lens.lens (\ClassificationResult' {sizeClassified} -> sizeClassified) (\s@ClassificationResult' {} a -> s {sizeClassified = a} :: ClassificationResult)

-- | Indicates whether there are additional occurrences of sensitive data
-- that are not included in the finding. This occurs when the number of
-- occurrences exceeds the maximum that can be included.
classificationResult_additionalOccurrences :: Lens.Lens' ClassificationResult (Prelude.Maybe Prelude.Bool)
classificationResult_additionalOccurrences = Lens.lens (\ClassificationResult' {additionalOccurrences} -> additionalOccurrences) (\s@ClassificationResult' {} a -> s {additionalOccurrences = a} :: ClassificationResult)

-- | Provides details about sensitive data that was identified based on
-- customer-defined configuration.
classificationResult_customDataIdentifiers :: Lens.Lens' ClassificationResult (Prelude.Maybe CustomDataIdentifiersResult)
classificationResult_customDataIdentifiers = Lens.lens (\ClassificationResult' {customDataIdentifiers} -> customDataIdentifiers) (\s@ClassificationResult' {} a -> s {customDataIdentifiers = a} :: ClassificationResult)

instance Core.FromJSON ClassificationResult where
  parseJSON =
    Core.withObject
      "ClassificationResult"
      ( \x ->
          ClassificationResult'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "SensitiveData" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "MimeType")
            Prelude.<*> (x Core..:? "SizeClassified")
            Prelude.<*> (x Core..:? "AdditionalOccurrences")
            Prelude.<*> (x Core..:? "CustomDataIdentifiers")
      )

instance Prelude.Hashable ClassificationResult

instance Prelude.NFData ClassificationResult

instance Core.ToJSON ClassificationResult where
  toJSON ClassificationResult' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Status" Core..=) Prelude.<$> status,
            ("SensitiveData" Core..=) Prelude.<$> sensitiveData,
            ("MimeType" Core..=) Prelude.<$> mimeType,
            ("SizeClassified" Core..=)
              Prelude.<$> sizeClassified,
            ("AdditionalOccurrences" Core..=)
              Prelude.<$> additionalOccurrences,
            ("CustomDataIdentifiers" Core..=)
              Prelude.<$> customDataIdentifiers
          ]
      )
