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
-- Module      : Amazonka.FraudDetector.Types.DataValidationMetrics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.DataValidationMetrics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types.FieldValidationMessage
import Amazonka.FraudDetector.Types.FileValidationMessage
import qualified Amazonka.Prelude as Prelude

-- | The model training data validation metrics.
--
-- /See:/ 'newDataValidationMetrics' smart constructor.
data DataValidationMetrics = DataValidationMetrics'
  { -- | The field-specific model training validation messages.
    fieldLevelMessages :: Prelude.Maybe [FieldValidationMessage],
    -- | The file-specific model training data validation messages.
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
-- 'fileLevelMessages', 'dataValidationMetrics_fileLevelMessages' - The file-specific model training data validation messages.
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

-- | The file-specific model training data validation messages.
dataValidationMetrics_fileLevelMessages :: Lens.Lens' DataValidationMetrics (Prelude.Maybe [FileValidationMessage])
dataValidationMetrics_fileLevelMessages = Lens.lens (\DataValidationMetrics' {fileLevelMessages} -> fileLevelMessages) (\s@DataValidationMetrics' {} a -> s {fileLevelMessages = a} :: DataValidationMetrics) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DataValidationMetrics where
  parseJSON =
    Data.withObject
      "DataValidationMetrics"
      ( \x ->
          DataValidationMetrics'
            Prelude.<$> ( x Data..:? "fieldLevelMessages"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "fileLevelMessages"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable DataValidationMetrics where
  hashWithSalt _salt DataValidationMetrics' {..} =
    _salt `Prelude.hashWithSalt` fieldLevelMessages
      `Prelude.hashWithSalt` fileLevelMessages

instance Prelude.NFData DataValidationMetrics where
  rnf DataValidationMetrics' {..} =
    Prelude.rnf fieldLevelMessages
      `Prelude.seq` Prelude.rnf fileLevelMessages
