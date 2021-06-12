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
-- Module      : Network.AWS.DataPipeline.Types.ValidationWarning
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.ValidationWarning where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Defines a validation warning. Validation warnings do not prevent
-- pipeline activation. The set of validation warnings that can be returned
-- are defined by AWS Data Pipeline.
--
-- /See:/ 'newValidationWarning' smart constructor.
data ValidationWarning = ValidationWarning'
  { -- | A description of the validation warning.
    warnings :: Core.Maybe [Core.Text],
    -- | The identifier of the object that contains the validation warning.
    id :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ValidationWarning' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'warnings', 'validationWarning_warnings' - A description of the validation warning.
--
-- 'id', 'validationWarning_id' - The identifier of the object that contains the validation warning.
newValidationWarning ::
  ValidationWarning
newValidationWarning =
  ValidationWarning'
    { warnings = Core.Nothing,
      id = Core.Nothing
    }

-- | A description of the validation warning.
validationWarning_warnings :: Lens.Lens' ValidationWarning (Core.Maybe [Core.Text])
validationWarning_warnings = Lens.lens (\ValidationWarning' {warnings} -> warnings) (\s@ValidationWarning' {} a -> s {warnings = a} :: ValidationWarning) Core.. Lens.mapping Lens._Coerce

-- | The identifier of the object that contains the validation warning.
validationWarning_id :: Lens.Lens' ValidationWarning (Core.Maybe Core.Text)
validationWarning_id = Lens.lens (\ValidationWarning' {id} -> id) (\s@ValidationWarning' {} a -> s {id = a} :: ValidationWarning)

instance Core.FromJSON ValidationWarning where
  parseJSON =
    Core.withObject
      "ValidationWarning"
      ( \x ->
          ValidationWarning'
            Core.<$> (x Core..:? "warnings" Core..!= Core.mempty)
            Core.<*> (x Core..:? "id")
      )

instance Core.Hashable ValidationWarning

instance Core.NFData ValidationWarning
