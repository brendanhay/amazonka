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
-- Module      : Amazonka.Nimble.Types.ValidationResult
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.ValidationResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types.LaunchProfileValidationState
import Amazonka.Nimble.Types.LaunchProfileValidationStatusCode
import Amazonka.Nimble.Types.LaunchProfileValidationType
import qualified Amazonka.Prelude as Prelude

-- | The launch profile validation result.
--
-- /See:/ 'newValidationResult' smart constructor.
data ValidationResult = ValidationResult'
  { -- | The current state.
    state :: LaunchProfileValidationState,
    -- | The status code. This will contain the failure reason if the state is
    -- @VALIDATION_FAILED@.
    statusCode :: LaunchProfileValidationStatusCode,
    -- | The status message for the validation result.
    statusMessage :: Prelude.Text,
    -- | The type of the validation result.
    type' :: LaunchProfileValidationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidationResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'validationResult_state' - The current state.
--
-- 'statusCode', 'validationResult_statusCode' - The status code. This will contain the failure reason if the state is
-- @VALIDATION_FAILED@.
--
-- 'statusMessage', 'validationResult_statusMessage' - The status message for the validation result.
--
-- 'type'', 'validationResult_type' - The type of the validation result.
newValidationResult ::
  -- | 'state'
  LaunchProfileValidationState ->
  -- | 'statusCode'
  LaunchProfileValidationStatusCode ->
  -- | 'statusMessage'
  Prelude.Text ->
  -- | 'type''
  LaunchProfileValidationType ->
  ValidationResult
newValidationResult
  pState_
  pStatusCode_
  pStatusMessage_
  pType_ =
    ValidationResult'
      { state = pState_,
        statusCode = pStatusCode_,
        statusMessage = pStatusMessage_,
        type' = pType_
      }

-- | The current state.
validationResult_state :: Lens.Lens' ValidationResult LaunchProfileValidationState
validationResult_state = Lens.lens (\ValidationResult' {state} -> state) (\s@ValidationResult' {} a -> s {state = a} :: ValidationResult)

-- | The status code. This will contain the failure reason if the state is
-- @VALIDATION_FAILED@.
validationResult_statusCode :: Lens.Lens' ValidationResult LaunchProfileValidationStatusCode
validationResult_statusCode = Lens.lens (\ValidationResult' {statusCode} -> statusCode) (\s@ValidationResult' {} a -> s {statusCode = a} :: ValidationResult)

-- | The status message for the validation result.
validationResult_statusMessage :: Lens.Lens' ValidationResult Prelude.Text
validationResult_statusMessage = Lens.lens (\ValidationResult' {statusMessage} -> statusMessage) (\s@ValidationResult' {} a -> s {statusMessage = a} :: ValidationResult)

-- | The type of the validation result.
validationResult_type :: Lens.Lens' ValidationResult LaunchProfileValidationType
validationResult_type = Lens.lens (\ValidationResult' {type'} -> type') (\s@ValidationResult' {} a -> s {type' = a} :: ValidationResult)

instance Data.FromJSON ValidationResult where
  parseJSON =
    Data.withObject
      "ValidationResult"
      ( \x ->
          ValidationResult'
            Prelude.<$> (x Data..: "state")
            Prelude.<*> (x Data..: "statusCode")
            Prelude.<*> (x Data..: "statusMessage")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable ValidationResult where
  hashWithSalt _salt ValidationResult' {..} =
    _salt `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` statusCode
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ValidationResult where
  rnf ValidationResult' {..} =
    Prelude.rnf state
      `Prelude.seq` Prelude.rnf statusCode
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf type'
