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
-- Module      : Amazonka.DataPipeline.Types.ValidationWarning
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataPipeline.Types.ValidationWarning where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines a validation warning. Validation warnings do not prevent
-- pipeline activation. The set of validation warnings that can be returned
-- are defined by AWS Data Pipeline.
--
-- /See:/ 'newValidationWarning' smart constructor.
data ValidationWarning = ValidationWarning'
  { -- | The identifier of the object that contains the validation warning.
    id :: Prelude.Maybe Prelude.Text,
    -- | A description of the validation warning.
    warnings :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidationWarning' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'validationWarning_id' - The identifier of the object that contains the validation warning.
--
-- 'warnings', 'validationWarning_warnings' - A description of the validation warning.
newValidationWarning ::
  ValidationWarning
newValidationWarning =
  ValidationWarning'
    { id = Prelude.Nothing,
      warnings = Prelude.Nothing
    }

-- | The identifier of the object that contains the validation warning.
validationWarning_id :: Lens.Lens' ValidationWarning (Prelude.Maybe Prelude.Text)
validationWarning_id = Lens.lens (\ValidationWarning' {id} -> id) (\s@ValidationWarning' {} a -> s {id = a} :: ValidationWarning)

-- | A description of the validation warning.
validationWarning_warnings :: Lens.Lens' ValidationWarning (Prelude.Maybe [Prelude.Text])
validationWarning_warnings = Lens.lens (\ValidationWarning' {warnings} -> warnings) (\s@ValidationWarning' {} a -> s {warnings = a} :: ValidationWarning) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ValidationWarning where
  parseJSON =
    Data.withObject
      "ValidationWarning"
      ( \x ->
          ValidationWarning'
            Prelude.<$> (x Data..:? "id")
            Prelude.<*> (x Data..:? "warnings" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ValidationWarning where
  hashWithSalt _salt ValidationWarning' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` warnings

instance Prelude.NFData ValidationWarning where
  rnf ValidationWarning' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf warnings
