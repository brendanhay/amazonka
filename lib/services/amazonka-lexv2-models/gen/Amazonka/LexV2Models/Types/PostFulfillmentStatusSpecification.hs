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
import qualified Amazonka.Data as Data
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
  { -- | A list of conditional branches to evaluate after the fulfillment code
    -- hook throws an exception or returns with the @State@ field of the
    -- @Intent@ object set to @Failed@.
    failureConditional :: Prelude.Maybe ConditionalSpecification,
    -- | Specifies the next step the bot runs after the fulfillment code hook
    -- throws an exception or returns with the @State@ field of the @Intent@
    -- object set to @Failed@.
    failureNextStep :: Prelude.Maybe DialogState,
    failureResponse :: Prelude.Maybe ResponseSpecification,
    -- | A list of conditional branches to evaluate after the fulfillment code
    -- hook finishes successfully.
    successConditional :: Prelude.Maybe ConditionalSpecification,
    -- | Specifies the next step in the conversation that Amazon Lex invokes when
    -- the fulfillment code hook completes successfully.
    successNextStep :: Prelude.Maybe DialogState,
    successResponse :: Prelude.Maybe ResponseSpecification,
    -- | A list of conditional branches to evaluate if the fulfillment code hook
    -- times out.
    timeoutConditional :: Prelude.Maybe ConditionalSpecification,
    -- | Specifies the next step that the bot runs when the fulfillment code hook
    -- times out.
    timeoutNextStep :: Prelude.Maybe DialogState,
    timeoutResponse :: Prelude.Maybe ResponseSpecification
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
-- 'failureConditional', 'postFulfillmentStatusSpecification_failureConditional' - A list of conditional branches to evaluate after the fulfillment code
-- hook throws an exception or returns with the @State@ field of the
-- @Intent@ object set to @Failed@.
--
-- 'failureNextStep', 'postFulfillmentStatusSpecification_failureNextStep' - Specifies the next step the bot runs after the fulfillment code hook
-- throws an exception or returns with the @State@ field of the @Intent@
-- object set to @Failed@.
--
-- 'failureResponse', 'postFulfillmentStatusSpecification_failureResponse' - Undocumented member.
--
-- 'successConditional', 'postFulfillmentStatusSpecification_successConditional' - A list of conditional branches to evaluate after the fulfillment code
-- hook finishes successfully.
--
-- 'successNextStep', 'postFulfillmentStatusSpecification_successNextStep' - Specifies the next step in the conversation that Amazon Lex invokes when
-- the fulfillment code hook completes successfully.
--
-- 'successResponse', 'postFulfillmentStatusSpecification_successResponse' - Undocumented member.
--
-- 'timeoutConditional', 'postFulfillmentStatusSpecification_timeoutConditional' - A list of conditional branches to evaluate if the fulfillment code hook
-- times out.
--
-- 'timeoutNextStep', 'postFulfillmentStatusSpecification_timeoutNextStep' - Specifies the next step that the bot runs when the fulfillment code hook
-- times out.
--
-- 'timeoutResponse', 'postFulfillmentStatusSpecification_timeoutResponse' - Undocumented member.
newPostFulfillmentStatusSpecification ::
  PostFulfillmentStatusSpecification
newPostFulfillmentStatusSpecification =
  PostFulfillmentStatusSpecification'
    { failureConditional =
        Prelude.Nothing,
      failureNextStep = Prelude.Nothing,
      failureResponse = Prelude.Nothing,
      successConditional = Prelude.Nothing,
      successNextStep = Prelude.Nothing,
      successResponse = Prelude.Nothing,
      timeoutConditional = Prelude.Nothing,
      timeoutNextStep = Prelude.Nothing,
      timeoutResponse = Prelude.Nothing
    }

-- | A list of conditional branches to evaluate after the fulfillment code
-- hook throws an exception or returns with the @State@ field of the
-- @Intent@ object set to @Failed@.
postFulfillmentStatusSpecification_failureConditional :: Lens.Lens' PostFulfillmentStatusSpecification (Prelude.Maybe ConditionalSpecification)
postFulfillmentStatusSpecification_failureConditional = Lens.lens (\PostFulfillmentStatusSpecification' {failureConditional} -> failureConditional) (\s@PostFulfillmentStatusSpecification' {} a -> s {failureConditional = a} :: PostFulfillmentStatusSpecification)

-- | Specifies the next step the bot runs after the fulfillment code hook
-- throws an exception or returns with the @State@ field of the @Intent@
-- object set to @Failed@.
postFulfillmentStatusSpecification_failureNextStep :: Lens.Lens' PostFulfillmentStatusSpecification (Prelude.Maybe DialogState)
postFulfillmentStatusSpecification_failureNextStep = Lens.lens (\PostFulfillmentStatusSpecification' {failureNextStep} -> failureNextStep) (\s@PostFulfillmentStatusSpecification' {} a -> s {failureNextStep = a} :: PostFulfillmentStatusSpecification)

-- | Undocumented member.
postFulfillmentStatusSpecification_failureResponse :: Lens.Lens' PostFulfillmentStatusSpecification (Prelude.Maybe ResponseSpecification)
postFulfillmentStatusSpecification_failureResponse = Lens.lens (\PostFulfillmentStatusSpecification' {failureResponse} -> failureResponse) (\s@PostFulfillmentStatusSpecification' {} a -> s {failureResponse = a} :: PostFulfillmentStatusSpecification)

-- | A list of conditional branches to evaluate after the fulfillment code
-- hook finishes successfully.
postFulfillmentStatusSpecification_successConditional :: Lens.Lens' PostFulfillmentStatusSpecification (Prelude.Maybe ConditionalSpecification)
postFulfillmentStatusSpecification_successConditional = Lens.lens (\PostFulfillmentStatusSpecification' {successConditional} -> successConditional) (\s@PostFulfillmentStatusSpecification' {} a -> s {successConditional = a} :: PostFulfillmentStatusSpecification)

-- | Specifies the next step in the conversation that Amazon Lex invokes when
-- the fulfillment code hook completes successfully.
postFulfillmentStatusSpecification_successNextStep :: Lens.Lens' PostFulfillmentStatusSpecification (Prelude.Maybe DialogState)
postFulfillmentStatusSpecification_successNextStep = Lens.lens (\PostFulfillmentStatusSpecification' {successNextStep} -> successNextStep) (\s@PostFulfillmentStatusSpecification' {} a -> s {successNextStep = a} :: PostFulfillmentStatusSpecification)

-- | Undocumented member.
postFulfillmentStatusSpecification_successResponse :: Lens.Lens' PostFulfillmentStatusSpecification (Prelude.Maybe ResponseSpecification)
postFulfillmentStatusSpecification_successResponse = Lens.lens (\PostFulfillmentStatusSpecification' {successResponse} -> successResponse) (\s@PostFulfillmentStatusSpecification' {} a -> s {successResponse = a} :: PostFulfillmentStatusSpecification)

-- | A list of conditional branches to evaluate if the fulfillment code hook
-- times out.
postFulfillmentStatusSpecification_timeoutConditional :: Lens.Lens' PostFulfillmentStatusSpecification (Prelude.Maybe ConditionalSpecification)
postFulfillmentStatusSpecification_timeoutConditional = Lens.lens (\PostFulfillmentStatusSpecification' {timeoutConditional} -> timeoutConditional) (\s@PostFulfillmentStatusSpecification' {} a -> s {timeoutConditional = a} :: PostFulfillmentStatusSpecification)

-- | Specifies the next step that the bot runs when the fulfillment code hook
-- times out.
postFulfillmentStatusSpecification_timeoutNextStep :: Lens.Lens' PostFulfillmentStatusSpecification (Prelude.Maybe DialogState)
postFulfillmentStatusSpecification_timeoutNextStep = Lens.lens (\PostFulfillmentStatusSpecification' {timeoutNextStep} -> timeoutNextStep) (\s@PostFulfillmentStatusSpecification' {} a -> s {timeoutNextStep = a} :: PostFulfillmentStatusSpecification)

-- | Undocumented member.
postFulfillmentStatusSpecification_timeoutResponse :: Lens.Lens' PostFulfillmentStatusSpecification (Prelude.Maybe ResponseSpecification)
postFulfillmentStatusSpecification_timeoutResponse = Lens.lens (\PostFulfillmentStatusSpecification' {timeoutResponse} -> timeoutResponse) (\s@PostFulfillmentStatusSpecification' {} a -> s {timeoutResponse = a} :: PostFulfillmentStatusSpecification)

instance
  Data.FromJSON
    PostFulfillmentStatusSpecification
  where
  parseJSON =
    Data.withObject
      "PostFulfillmentStatusSpecification"
      ( \x ->
          PostFulfillmentStatusSpecification'
            Prelude.<$> (x Data..:? "failureConditional")
            Prelude.<*> (x Data..:? "failureNextStep")
            Prelude.<*> (x Data..:? "failureResponse")
            Prelude.<*> (x Data..:? "successConditional")
            Prelude.<*> (x Data..:? "successNextStep")
            Prelude.<*> (x Data..:? "successResponse")
            Prelude.<*> (x Data..:? "timeoutConditional")
            Prelude.<*> (x Data..:? "timeoutNextStep")
            Prelude.<*> (x Data..:? "timeoutResponse")
      )

instance
  Prelude.Hashable
    PostFulfillmentStatusSpecification
  where
  hashWithSalt
    _salt
    PostFulfillmentStatusSpecification' {..} =
      _salt `Prelude.hashWithSalt` failureConditional
        `Prelude.hashWithSalt` failureNextStep
        `Prelude.hashWithSalt` failureResponse
        `Prelude.hashWithSalt` successConditional
        `Prelude.hashWithSalt` successNextStep
        `Prelude.hashWithSalt` successResponse
        `Prelude.hashWithSalt` timeoutConditional
        `Prelude.hashWithSalt` timeoutNextStep
        `Prelude.hashWithSalt` timeoutResponse

instance
  Prelude.NFData
    PostFulfillmentStatusSpecification
  where
  rnf PostFulfillmentStatusSpecification' {..} =
    Prelude.rnf failureConditional
      `Prelude.seq` Prelude.rnf failureNextStep
      `Prelude.seq` Prelude.rnf failureResponse
      `Prelude.seq` Prelude.rnf successConditional
      `Prelude.seq` Prelude.rnf successNextStep
      `Prelude.seq` Prelude.rnf successResponse
      `Prelude.seq` Prelude.rnf timeoutConditional
      `Prelude.seq` Prelude.rnf timeoutNextStep
      `Prelude.seq` Prelude.rnf timeoutResponse

instance
  Data.ToJSON
    PostFulfillmentStatusSpecification
  where
  toJSON PostFulfillmentStatusSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("failureConditional" Data..=)
              Prelude.<$> failureConditional,
            ("failureNextStep" Data..=)
              Prelude.<$> failureNextStep,
            ("failureResponse" Data..=)
              Prelude.<$> failureResponse,
            ("successConditional" Data..=)
              Prelude.<$> successConditional,
            ("successNextStep" Data..=)
              Prelude.<$> successNextStep,
            ("successResponse" Data..=)
              Prelude.<$> successResponse,
            ("timeoutConditional" Data..=)
              Prelude.<$> timeoutConditional,
            ("timeoutNextStep" Data..=)
              Prelude.<$> timeoutNextStep,
            ("timeoutResponse" Data..=)
              Prelude.<$> timeoutResponse
          ]
      )
