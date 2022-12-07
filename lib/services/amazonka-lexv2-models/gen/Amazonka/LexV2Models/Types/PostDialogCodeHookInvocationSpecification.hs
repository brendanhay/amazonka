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
-- Module      : Amazonka.LexV2Models.Types.PostDialogCodeHookInvocationSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.PostDialogCodeHookInvocationSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.ConditionalSpecification
import Amazonka.LexV2Models.Types.DialogState
import Amazonka.LexV2Models.Types.ResponseSpecification
import qualified Amazonka.Prelude as Prelude

-- | Specifies next steps to run after the dialog code hook finishes.
--
-- /See:/ 'newPostDialogCodeHookInvocationSpecification' smart constructor.
data PostDialogCodeHookInvocationSpecification = PostDialogCodeHookInvocationSpecification'
  { -- | Specifies the next step that the bot runs when the code hook times out.
    timeoutNextStep :: Prelude.Maybe DialogState,
    -- | A list of conditional branches to evaluate if the code hook times out.
    timeoutConditional :: Prelude.Maybe ConditionalSpecification,
    -- | Specifies the next step the bot runs after the dialog code hook throws
    -- an exception or returns with the @State@ field of the @Intent@ object
    -- set to @Failed@.
    failureNextStep :: Prelude.Maybe DialogState,
    timeoutResponse :: Prelude.Maybe ResponseSpecification,
    -- | Specifics the next step the bot runs after the dialog code hook finishes
    -- successfully.
    successNextStep :: Prelude.Maybe DialogState,
    -- | A list of conditional branches to evaluate after the dialog code hook
    -- finishes successfully.
    successConditional :: Prelude.Maybe ConditionalSpecification,
    successResponse :: Prelude.Maybe ResponseSpecification,
    -- | A list of conditional branches to evaluate after the dialog code hook
    -- throws an exception or returns with the @State@ field of the @Intent@
    -- object set to @Failed@.
    failureConditional :: Prelude.Maybe ConditionalSpecification,
    failureResponse :: Prelude.Maybe ResponseSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PostDialogCodeHookInvocationSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeoutNextStep', 'postDialogCodeHookInvocationSpecification_timeoutNextStep' - Specifies the next step that the bot runs when the code hook times out.
--
-- 'timeoutConditional', 'postDialogCodeHookInvocationSpecification_timeoutConditional' - A list of conditional branches to evaluate if the code hook times out.
--
-- 'failureNextStep', 'postDialogCodeHookInvocationSpecification_failureNextStep' - Specifies the next step the bot runs after the dialog code hook throws
-- an exception or returns with the @State@ field of the @Intent@ object
-- set to @Failed@.
--
-- 'timeoutResponse', 'postDialogCodeHookInvocationSpecification_timeoutResponse' - Undocumented member.
--
-- 'successNextStep', 'postDialogCodeHookInvocationSpecification_successNextStep' - Specifics the next step the bot runs after the dialog code hook finishes
-- successfully.
--
-- 'successConditional', 'postDialogCodeHookInvocationSpecification_successConditional' - A list of conditional branches to evaluate after the dialog code hook
-- finishes successfully.
--
-- 'successResponse', 'postDialogCodeHookInvocationSpecification_successResponse' - Undocumented member.
--
-- 'failureConditional', 'postDialogCodeHookInvocationSpecification_failureConditional' - A list of conditional branches to evaluate after the dialog code hook
-- throws an exception or returns with the @State@ field of the @Intent@
-- object set to @Failed@.
--
-- 'failureResponse', 'postDialogCodeHookInvocationSpecification_failureResponse' - Undocumented member.
newPostDialogCodeHookInvocationSpecification ::
  PostDialogCodeHookInvocationSpecification
newPostDialogCodeHookInvocationSpecification =
  PostDialogCodeHookInvocationSpecification'
    { timeoutNextStep =
        Prelude.Nothing,
      timeoutConditional =
        Prelude.Nothing,
      failureNextStep =
        Prelude.Nothing,
      timeoutResponse =
        Prelude.Nothing,
      successNextStep =
        Prelude.Nothing,
      successConditional =
        Prelude.Nothing,
      successResponse =
        Prelude.Nothing,
      failureConditional =
        Prelude.Nothing,
      failureResponse =
        Prelude.Nothing
    }

-- | Specifies the next step that the bot runs when the code hook times out.
postDialogCodeHookInvocationSpecification_timeoutNextStep :: Lens.Lens' PostDialogCodeHookInvocationSpecification (Prelude.Maybe DialogState)
postDialogCodeHookInvocationSpecification_timeoutNextStep = Lens.lens (\PostDialogCodeHookInvocationSpecification' {timeoutNextStep} -> timeoutNextStep) (\s@PostDialogCodeHookInvocationSpecification' {} a -> s {timeoutNextStep = a} :: PostDialogCodeHookInvocationSpecification)

-- | A list of conditional branches to evaluate if the code hook times out.
postDialogCodeHookInvocationSpecification_timeoutConditional :: Lens.Lens' PostDialogCodeHookInvocationSpecification (Prelude.Maybe ConditionalSpecification)
postDialogCodeHookInvocationSpecification_timeoutConditional = Lens.lens (\PostDialogCodeHookInvocationSpecification' {timeoutConditional} -> timeoutConditional) (\s@PostDialogCodeHookInvocationSpecification' {} a -> s {timeoutConditional = a} :: PostDialogCodeHookInvocationSpecification)

-- | Specifies the next step the bot runs after the dialog code hook throws
-- an exception or returns with the @State@ field of the @Intent@ object
-- set to @Failed@.
postDialogCodeHookInvocationSpecification_failureNextStep :: Lens.Lens' PostDialogCodeHookInvocationSpecification (Prelude.Maybe DialogState)
postDialogCodeHookInvocationSpecification_failureNextStep = Lens.lens (\PostDialogCodeHookInvocationSpecification' {failureNextStep} -> failureNextStep) (\s@PostDialogCodeHookInvocationSpecification' {} a -> s {failureNextStep = a} :: PostDialogCodeHookInvocationSpecification)

-- | Undocumented member.
postDialogCodeHookInvocationSpecification_timeoutResponse :: Lens.Lens' PostDialogCodeHookInvocationSpecification (Prelude.Maybe ResponseSpecification)
postDialogCodeHookInvocationSpecification_timeoutResponse = Lens.lens (\PostDialogCodeHookInvocationSpecification' {timeoutResponse} -> timeoutResponse) (\s@PostDialogCodeHookInvocationSpecification' {} a -> s {timeoutResponse = a} :: PostDialogCodeHookInvocationSpecification)

-- | Specifics the next step the bot runs after the dialog code hook finishes
-- successfully.
postDialogCodeHookInvocationSpecification_successNextStep :: Lens.Lens' PostDialogCodeHookInvocationSpecification (Prelude.Maybe DialogState)
postDialogCodeHookInvocationSpecification_successNextStep = Lens.lens (\PostDialogCodeHookInvocationSpecification' {successNextStep} -> successNextStep) (\s@PostDialogCodeHookInvocationSpecification' {} a -> s {successNextStep = a} :: PostDialogCodeHookInvocationSpecification)

-- | A list of conditional branches to evaluate after the dialog code hook
-- finishes successfully.
postDialogCodeHookInvocationSpecification_successConditional :: Lens.Lens' PostDialogCodeHookInvocationSpecification (Prelude.Maybe ConditionalSpecification)
postDialogCodeHookInvocationSpecification_successConditional = Lens.lens (\PostDialogCodeHookInvocationSpecification' {successConditional} -> successConditional) (\s@PostDialogCodeHookInvocationSpecification' {} a -> s {successConditional = a} :: PostDialogCodeHookInvocationSpecification)

-- | Undocumented member.
postDialogCodeHookInvocationSpecification_successResponse :: Lens.Lens' PostDialogCodeHookInvocationSpecification (Prelude.Maybe ResponseSpecification)
postDialogCodeHookInvocationSpecification_successResponse = Lens.lens (\PostDialogCodeHookInvocationSpecification' {successResponse} -> successResponse) (\s@PostDialogCodeHookInvocationSpecification' {} a -> s {successResponse = a} :: PostDialogCodeHookInvocationSpecification)

-- | A list of conditional branches to evaluate after the dialog code hook
-- throws an exception or returns with the @State@ field of the @Intent@
-- object set to @Failed@.
postDialogCodeHookInvocationSpecification_failureConditional :: Lens.Lens' PostDialogCodeHookInvocationSpecification (Prelude.Maybe ConditionalSpecification)
postDialogCodeHookInvocationSpecification_failureConditional = Lens.lens (\PostDialogCodeHookInvocationSpecification' {failureConditional} -> failureConditional) (\s@PostDialogCodeHookInvocationSpecification' {} a -> s {failureConditional = a} :: PostDialogCodeHookInvocationSpecification)

-- | Undocumented member.
postDialogCodeHookInvocationSpecification_failureResponse :: Lens.Lens' PostDialogCodeHookInvocationSpecification (Prelude.Maybe ResponseSpecification)
postDialogCodeHookInvocationSpecification_failureResponse = Lens.lens (\PostDialogCodeHookInvocationSpecification' {failureResponse} -> failureResponse) (\s@PostDialogCodeHookInvocationSpecification' {} a -> s {failureResponse = a} :: PostDialogCodeHookInvocationSpecification)

instance
  Data.FromJSON
    PostDialogCodeHookInvocationSpecification
  where
  parseJSON =
    Data.withObject
      "PostDialogCodeHookInvocationSpecification"
      ( \x ->
          PostDialogCodeHookInvocationSpecification'
            Prelude.<$> (x Data..:? "timeoutNextStep")
              Prelude.<*> (x Data..:? "timeoutConditional")
              Prelude.<*> (x Data..:? "failureNextStep")
              Prelude.<*> (x Data..:? "timeoutResponse")
              Prelude.<*> (x Data..:? "successNextStep")
              Prelude.<*> (x Data..:? "successConditional")
              Prelude.<*> (x Data..:? "successResponse")
              Prelude.<*> (x Data..:? "failureConditional")
              Prelude.<*> (x Data..:? "failureResponse")
      )

instance
  Prelude.Hashable
    PostDialogCodeHookInvocationSpecification
  where
  hashWithSalt
    _salt
    PostDialogCodeHookInvocationSpecification' {..} =
      _salt `Prelude.hashWithSalt` timeoutNextStep
        `Prelude.hashWithSalt` timeoutConditional
        `Prelude.hashWithSalt` failureNextStep
        `Prelude.hashWithSalt` timeoutResponse
        `Prelude.hashWithSalt` successNextStep
        `Prelude.hashWithSalt` successConditional
        `Prelude.hashWithSalt` successResponse
        `Prelude.hashWithSalt` failureConditional
        `Prelude.hashWithSalt` failureResponse

instance
  Prelude.NFData
    PostDialogCodeHookInvocationSpecification
  where
  rnf PostDialogCodeHookInvocationSpecification' {..} =
    Prelude.rnf timeoutNextStep
      `Prelude.seq` Prelude.rnf timeoutConditional
      `Prelude.seq` Prelude.rnf failureNextStep
      `Prelude.seq` Prelude.rnf timeoutResponse
      `Prelude.seq` Prelude.rnf successNextStep
      `Prelude.seq` Prelude.rnf successConditional
      `Prelude.seq` Prelude.rnf successResponse
      `Prelude.seq` Prelude.rnf failureConditional
      `Prelude.seq` Prelude.rnf failureResponse

instance
  Data.ToJSON
    PostDialogCodeHookInvocationSpecification
  where
  toJSON PostDialogCodeHookInvocationSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("timeoutNextStep" Data..=)
              Prelude.<$> timeoutNextStep,
            ("timeoutConditional" Data..=)
              Prelude.<$> timeoutConditional,
            ("failureNextStep" Data..=)
              Prelude.<$> failureNextStep,
            ("timeoutResponse" Data..=)
              Prelude.<$> timeoutResponse,
            ("successNextStep" Data..=)
              Prelude.<$> successNextStep,
            ("successConditional" Data..=)
              Prelude.<$> successConditional,
            ("successResponse" Data..=)
              Prelude.<$> successResponse,
            ("failureConditional" Data..=)
              Prelude.<$> failureConditional,
            ("failureResponse" Data..=)
              Prelude.<$> failureResponse
          ]
      )
