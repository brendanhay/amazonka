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
-- Module      : Network.AWS.SecretsManager.Types.ValidationErrorsEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecretsManager.Types.ValidationErrorsEntry where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Displays errors that occurred during validation of the resource policy.
--
-- /See:/ 'newValidationErrorsEntry' smart constructor.
data ValidationErrorsEntry = ValidationErrorsEntry'
  { -- | Checks the name of the policy.
    checkName :: Prelude.Maybe Prelude.Text,
    -- | Displays error messages if validation encounters problems during
    -- validation of the resource policy.
    errorMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ValidationErrorsEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checkName', 'validationErrorsEntry_checkName' - Checks the name of the policy.
--
-- 'errorMessage', 'validationErrorsEntry_errorMessage' - Displays error messages if validation encounters problems during
-- validation of the resource policy.
newValidationErrorsEntry ::
  ValidationErrorsEntry
newValidationErrorsEntry =
  ValidationErrorsEntry'
    { checkName = Prelude.Nothing,
      errorMessage = Prelude.Nothing
    }

-- | Checks the name of the policy.
validationErrorsEntry_checkName :: Lens.Lens' ValidationErrorsEntry (Prelude.Maybe Prelude.Text)
validationErrorsEntry_checkName = Lens.lens (\ValidationErrorsEntry' {checkName} -> checkName) (\s@ValidationErrorsEntry' {} a -> s {checkName = a} :: ValidationErrorsEntry)

-- | Displays error messages if validation encounters problems during
-- validation of the resource policy.
validationErrorsEntry_errorMessage :: Lens.Lens' ValidationErrorsEntry (Prelude.Maybe Prelude.Text)
validationErrorsEntry_errorMessage = Lens.lens (\ValidationErrorsEntry' {errorMessage} -> errorMessage) (\s@ValidationErrorsEntry' {} a -> s {errorMessage = a} :: ValidationErrorsEntry)

instance Prelude.FromJSON ValidationErrorsEntry where
  parseJSON =
    Prelude.withObject
      "ValidationErrorsEntry"
      ( \x ->
          ValidationErrorsEntry'
            Prelude.<$> (x Prelude..:? "CheckName")
            Prelude.<*> (x Prelude..:? "ErrorMessage")
      )

instance Prelude.Hashable ValidationErrorsEntry

instance Prelude.NFData ValidationErrorsEntry
