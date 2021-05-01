{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DataPipeline.Types.ValidationError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.ValidationError where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Defines a validation error. Validation errors prevent pipeline
-- activation. The set of validation errors that can be returned are
-- defined by AWS Data Pipeline.
--
-- /See:/ 'newValidationError' smart constructor.
data ValidationError = ValidationError'
  { -- | The identifier of the object that contains the validation error.
    id :: Prelude.Maybe Prelude.Text,
    -- | A description of the validation error.
    errors :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ValidationError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'validationError_id' - The identifier of the object that contains the validation error.
--
-- 'errors', 'validationError_errors' - A description of the validation error.
newValidationError ::
  ValidationError
newValidationError =
  ValidationError'
    { id = Prelude.Nothing,
      errors = Prelude.Nothing
    }

-- | The identifier of the object that contains the validation error.
validationError_id :: Lens.Lens' ValidationError (Prelude.Maybe Prelude.Text)
validationError_id = Lens.lens (\ValidationError' {id} -> id) (\s@ValidationError' {} a -> s {id = a} :: ValidationError)

-- | A description of the validation error.
validationError_errors :: Lens.Lens' ValidationError (Prelude.Maybe [Prelude.Text])
validationError_errors = Lens.lens (\ValidationError' {errors} -> errors) (\s@ValidationError' {} a -> s {errors = a} :: ValidationError) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON ValidationError where
  parseJSON =
    Prelude.withObject
      "ValidationError"
      ( \x ->
          ValidationError'
            Prelude.<$> (x Prelude..:? "id")
            Prelude.<*> (x Prelude..:? "errors" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable ValidationError

instance Prelude.NFData ValidationError
