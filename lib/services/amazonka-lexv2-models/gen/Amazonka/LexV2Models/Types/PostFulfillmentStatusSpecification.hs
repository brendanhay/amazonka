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
-- Module      : Amazonka.LexV2Models.Types.PostFulfillmentStatusSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.PostFulfillmentStatusSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types.ConditionalSpecification
import Amazonka.LexV2Models.Types.DialogState
import Amazonka.LexV2Models.Types.ResponseSpecification
import qualified Amazonka.Prelude as Prelude

-- | Provides a setting that determines whether the post-fulfillment response
-- is sent to the user. For more information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/streaming-progress.html#progress-complete>
--
-- /See:/ 'newPostFulfillmentStatusSpecification' smart constructor.
data PostFulfillmentStatusSpecification = PostFulfillmentStatusSpecification'
  { -- | Specifies the next step that the bot runs when the fulfillment code hook
    -- times out.
    timeoutNextStep :: Prelude.Maybe DialogState,
    -- | A list of conditional branches to evaluate if the fulfillment code hook
    -- times out.
    timeoutConditional :: Prelude.Maybe ConditionalSpecification,
    -- | Specifies the next step the bot runs after the fulfillment code hook
    -- throws an exception or returns with the @State@ field of the @Intent@
    -- object set to @Failed@.
    failureNextStep :: Prelude.Maybe DialogState,
    timeoutResponse :: Prelude.Maybe ResponseSpecification,
    -- | Specifies the next step in the conversation that Amazon Lex invokes when
    -- the fulfillment code hook completes successfully.
    successNextStep :: Prelude.Maybe DialogState,
    -- | A list of conditional branches to evaluate after the fulfillment code
    -- hook finishes successfully.
    successConditional :: Prelude.Maybe ConditionalSpecification,
    successResponse :: Prelude.Maybe ResponseSpecification,
    -- | A list of conditional branches to evaluate after the fulfillment code
    -- hook throws an exception or returns with the @State@ field of the
    -- @Intent@ object set to @Failed@.
    failureConditional :: Prelude.Maybe ConditionalSpecification,
    failureResponse :: Prelude.Maybe ResponseSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PostFulfillmentStatusSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeoutNextStep', 'postFulfillmentStatusSpecification_timeoutNextStep' - Specifies the next step that the bot runs when the fulfillment code hook
-- times out.
--
-- 'timeoutConditional', 'postFulfillmentStatusSpecification_timeoutConditional' - A list of conditional branches to evaluate if the fulfillment code hook
-- times out.
--
-- 'failureNextStep', 'postFulfillmentStatusSpecification_failureNextStep' - Specifies the next step the bot runs after the fulfillment code hook
-- throws an exception or returns with the @State@ field of the @Intent@
-- object set to @Failed@.
--
-- 'timeoutResponse', 'postFulfillmentStatusSpecification_timeoutResponse' - Undocumented member.
--
-- 'successNextStep', 'postFulfillmentStatusSpecification_successNextStep' - Specifies the next step in the conversation that Amazon Lex invokes when
-- the fulfillment code hook completes successfully.
--
-- 'successConditional', 'postFulfillmentStatusSpecification_successConditional' - A list of conditional branches to evaluate after the fulfillment code
-- hook finishes successfully.
--
-- 'successResponse', 'postFulfillmentStatusSpecification_successResponse' - Undocumented member.
--
-- 'failureConditional', 'postFulfillmentStatusSpecification_failureConditional' - A list of conditional branches to evaluate after the fulfillment code
-- hook throws an exception or returns with the @State@ field of the
-- @Intent@ object set to @Failed@.
--
-- 'failureResponse', 'postFulfillmentStatusSpecification_failureResponse' - Undocumented member.
newPostFulfillmentStatusSpecification ::
  PostFulfillmentStatusSpecification
newPostFulfillmentStatusSpecification =
  PostFulfillmentStatusSpecification'
    { timeoutNextStep =
        Prelude.Nothing,
      timeoutConditional = Prelude.Nothing,
      failureNextStep = Prelude.Nothing,
      timeoutResponse = Prelude.Nothing,
      successNextStep = Prelude.Nothing,
      successConditional = Prelude.Nothing,
      successResponse = Prelude.Nothing,
      failureConditional = Prelude.Nothing,
      failureResponse = Prelude.Nothing
    }

-- | Specifies the next step that the bot runs when the fulfillment code hook
-- times out.
postFulfillmentStatusSpecification_timeoutNextStep :: Lens.Lens' PostFulfillmentStatusSpecification (Prelude.Maybe DialogState)
postFulfillmentStatusSpecification_timeoutNextStep = Lens.lens (\PostFulfillmentStatusSpecification' {timeoutNextStep} -> timeoutNextStep) (\s@PostFulfillmentStatusSpecification' {} a -> s {timeoutNextStep = a} :: PostFulfillmentStatusSpecification)

-- | A list of conditional branches to evaluate if the fulfillment code hook
-- times out.
postFulfillmentStatusSpecification_timeoutConditional :: Lens.Lens' PostFulfillmentStatusSpecification (Prelude.Maybe ConditionalSpecification)
postFulfillmentStatusSpecification_timeoutConditional = Lens.lens (\PostFulfillmentStatusSpecification' {timeoutConditional} -> timeoutConditional) (\s@PostFulfillmentStatusSpecification' {} a -> s {timeoutConditional = a} :: PostFulfillmentStatusSpecification)

-- | Specifies the next step the bot runs after the fulfillment code hook
-- throws an exception or returns with the @State@ field of the @Intent@
-- object set to @Failed@.
postFulfillmentStatusSpecification_failureNextStep :: Lens.Lens' PostFulfillmentStatusSpecification (Prelude.Maybe DialogState)
postFulfillmentStatusSpecification_failureNextStep = Lens.lens (\PostFulfillmentStatusSpecification' {failureNextStep} -> failureNextStep) (\s@PostFulfillmentStatusSpecification' {} a -> s {failureNextStep = a} :: PostFulfillmentStatusSpecification)

-- | Undocumented member.
postFulfillmentStatusSpecification_timeoutResponse :: Lens.Lens' PostFulfillmentStatusSpecification (Prelude.Maybe ResponseSpecification)
postFulfillmentStatusSpecification_timeoutResponse = Lens.lens (\PostFulfillmentStatusSpecification' {timeoutResponse} -> timeoutResponse) (\s@PostFulfillmentStatusSpecification' {} a -> s {timeoutResponse = a} :: PostFulfillmentStatusSpecification)

-- | Specifies the next step in the conversation that Amazon Lex invokes when
-- the fulfillment code hook completes successfully.
postFulfillmentStatusSpecification_successNextStep :: Lens.Lens' PostFulfillmentStatusSpecification (Prelude.Maybe DialogState)
postFulfillmentStatusSpecification_successNextStep = Lens.lens (\PostFulfillmentStatusSpecification' {successNextStep} -> successNextStep) (\s@PostFulfillmentStatusSpecification' {} a -> s {successNextStep = a} :: PostFulfillmentStatusSpecification)

-- | A list of conditional branches to evaluate after the fulfillment code
-- hook finishes successfully.
postFulfillmentStatusSpecification_successConditional :: Lens.Lens' PostFulfillmentStatusSpecification (Prelude.Maybe ConditionalSpecification)
postFulfillmentStatusSpecification_successConditional = Lens.lens (\PostFulfillmentStatusSpecification' {successConditional} -> successConditional) (\s@PostFulfillmentStatusSpecification' {} a -> s {successConditional = a} :: PostFulfillmentStatusSpecification)

-- | Undocumented member.
postFulfillmentStatusSpecification_successResponse :: Lens.Lens' PostFulfillmentStatusSpecification (Prelude.Maybe ResponseSpecification)
postFulfillmentStatusSpecification_successResponse = Lens.lens (\PostFulfillmentStatusSpecification' {successResponse} -> successResponse) (\s@PostFulfillmentStatusSpecification' {} a -> s {successResponse = a} :: PostFulfillmentStatusSpecification)

-- | A list of conditional branches to evaluate after the fulfillment code
-- hook throws an exception or returns with the @State@ field of the
-- @Intent@ object set to @Failed@.
postFulfillmentStatusSpecification_failureConditional :: Lens.Lens' PostFulfillmentStatusSpecification (Prelude.Maybe ConditionalSpecification)
postFulfillmentStatusSpecification_failureConditional = Lens.lens (\PostFulfillmentStatusSpecification' {failureConditional} -> failureConditional) (\s@PostFulfillmentStatusSpecification' {} a -> s {failureConditional = a} :: PostFulfillmentStatusSpecification)

-- | Undocumented member.
postFulfillmentStatusSpecification_failureResponse :: Lens.Lens' PostFulfillmentStatusSpecification (Prelude.Maybe ResponseSpecification)
postFulfillmentStatusSpecification_failureResponse = Lens.lens (\PostFulfillmentStatusSpecification' {failureResponse} -> failureResponse) (\s@PostFulfillmentStatusSpecification' {} a -> s {failureResponse = a} :: PostFulfillmentStatusSpecification)

instance
  Core.FromJSON
    PostFulfillmentStatusSpecification
  where
  parseJSON =
    Core.withObject
      "PostFulfillmentStatusSpecification"
      ( \x ->
          PostFulfillmentStatusSpecification'
            Prelude.<$> (x Core..:? "timeoutNextStep")
            Prelude.<*> (x Core..:? "timeoutConditional")
            Prelude.<*> (x Core..:? "failureNextStep")
            Prelude.<*> (x Core..:? "timeoutResponse")
            Prelude.<*> (x Core..:? "successNextStep")
            Prelude.<*> (x Core..:? "successConditional")
            Prelude.<*> (x Core..:? "successResponse")
            Prelude.<*> (x Core..:? "failureConditional")
            Prelude.<*> (x Core..:? "failureResponse")
      )

instance
  Prelude.Hashable
    PostFulfillmentStatusSpecification
  where
  hashWithSalt
    _salt
    PostFulfillmentStatusSpecification' {..} =
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
    PostFulfillmentStatusSpecification
  where
  rnf PostFulfillmentStatusSpecification' {..} =
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
  Core.ToJSON
    PostFulfillmentStatusSpecification
  where
  toJSON PostFulfillmentStatusSpecification' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("timeoutNextStep" Core..=)
              Prelude.<$> timeoutNextStep,
            ("timeoutConditional" Core..=)
              Prelude.<$> timeoutConditional,
            ("failureNextStep" Core..=)
              Prelude.<$> failureNextStep,
            ("timeoutResponse" Core..=)
              Prelude.<$> timeoutResponse,
            ("successNextStep" Core..=)
              Prelude.<$> successNextStep,
            ("successConditional" Core..=)
              Prelude.<$> successConditional,
            ("successResponse" Core..=)
              Prelude.<$> successResponse,
            ("failureConditional" Core..=)
              Prelude.<$> failureConditional,
            ("failureResponse" Core..=)
              Prelude.<$> failureResponse
          ]
      )
