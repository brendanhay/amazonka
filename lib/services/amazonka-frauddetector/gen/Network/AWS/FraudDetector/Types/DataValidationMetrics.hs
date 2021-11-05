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
-- Module      : Network.AWS.FraudDetector.Types.DataValidationMetrics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FraudDetector.Types.DataValidationMetrics where

import qualified Network.AWS.Core as Core
import Network.AWS.FraudDetector.Types.FieldValidationMessage
import Network.AWS.FraudDetector.Types.FileValidationMessage
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The model training validation messages.
--
-- /See:/ 'newDataValidationMetrics' smart constructor.
data DataValidationMetrics = DataValidationMetrics'
  { -- | The field-specific model training validation messages.
    fieldLevelMessages :: Prelude.Maybe [FieldValidationMessage],
    -- | The file-specific model training validation messages.
    fileLevelMessages :: Prelude.Maybe [FileValidationMessage]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataValidationMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldLevelMessages', 'dataValidationMetrics_fieldLevelMessages' - The field-specific model training validation messages.
--
-- 'fileLevelMessages', 'dataValidationMetrics_fileLevelMessages' - The file-specific model training validation messages.
newDataValidationMetrics ::
  DataValidationMetrics
newDataValidationMetrics =
  DataValidationMetrics'
    { fieldLevelMessages =
        Prelude.Nothing,
      fileLevelMessages = Prelude.Nothing
    }

-- | The field-specific model training validation messages.
dataValidationMetrics_fieldLevelMessages :: Lens.Lens' DataValidationMetrics (Prelude.Maybe [FieldValidationMessage])
dataValidationMetrics_fieldLevelMessages = Lens.lens (\DataValidationMetrics' {fieldLevelMessages} -> fieldLevelMessages) (\s@DataValidationMetrics' {} a -> s {fieldLevelMessages = a} :: DataValidationMetrics) Prelude.. Lens.mapping Lens.coerced

-- | The file-specific model training validation messages.
dataValidationMetrics_fileLevelMessages :: Lens.Lens' DataValidationMetrics (Prelude.Maybe [FileValidationMessage])
dataValidationMetrics_fileLevelMessages = Lens.lens (\DataValidationMetrics' {fileLevelMessages} -> fileLevelMessages) (\s@DataValidationMetrics' {} a -> s {fileLevelMessages = a} :: DataValidationMetrics) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON DataValidationMetrics where
  parseJSON =
    Core.withObject
      "DataValidationMetrics"
      ( \x ->
          DataValidationMetrics'
            Prelude.<$> ( x Core..:? "fieldLevelMessages"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "fileLevelMessages"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable DataValidationMetrics

instance Prelude.NFData DataValidationMetrics
