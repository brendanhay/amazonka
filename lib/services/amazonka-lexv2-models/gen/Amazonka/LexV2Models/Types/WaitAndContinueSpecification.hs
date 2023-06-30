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
-- Module      : Amazonka.LexV2Models.Types.WaitAndContinueSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.WaitAndContinueSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.ResponseSpecification
import Amazonka.LexV2Models.Types.StillWaitingResponseSpecification
import qualified Amazonka.Prelude as Prelude

-- | Specifies the prompts that Amazon Lex uses while a bot is waiting for
-- customer input.
--
-- /See:/ 'newWaitAndContinueSpecification' smart constructor.
data WaitAndContinueSpecification = WaitAndContinueSpecification'
  { -- | Specifies whether the bot will wait for a user to respond. When this
    -- field is false, wait and continue responses for a slot aren\'t used. If
    -- the @active@ field isn\'t specified, the default is true.
    active :: Prelude.Maybe Prelude.Bool,
    -- | A response that Amazon Lex sends periodically to the user to indicate
    -- that the bot is still waiting for input from the user.
    stillWaitingResponse :: Prelude.Maybe StillWaitingResponseSpecification,
    -- | The response that Amazon Lex sends to indicate that the bot is waiting
    -- for the conversation to continue.
    waitingResponse :: ResponseSpecification,
    -- | The response that Amazon Lex sends to indicate that the bot is ready to
    -- continue the conversation.
    continueResponse :: ResponseSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WaitAndContinueSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'active', 'waitAndContinueSpecification_active' - Specifies whether the bot will wait for a user to respond. When this
-- field is false, wait and continue responses for a slot aren\'t used. If
-- the @active@ field isn\'t specified, the default is true.
--
-- 'stillWaitingResponse', 'waitAndContinueSpecification_stillWaitingResponse' - A response that Amazon Lex sends periodically to the user to indicate
-- that the bot is still waiting for input from the user.
--
-- 'waitingResponse', 'waitAndContinueSpecification_waitingResponse' - The response that Amazon Lex sends to indicate that the bot is waiting
-- for the conversation to continue.
--
-- 'continueResponse', 'waitAndContinueSpecification_continueResponse' - The response that Amazon Lex sends to indicate that the bot is ready to
-- continue the conversation.
newWaitAndContinueSpecification ::
  -- | 'waitingResponse'
  ResponseSpecification ->
  -- | 'continueResponse'
  ResponseSpecification ->
  WaitAndContinueSpecification
newWaitAndContinueSpecification
  pWaitingResponse_
  pContinueResponse_ =
    WaitAndContinueSpecification'
      { active =
          Prelude.Nothing,
        stillWaitingResponse = Prelude.Nothing,
        waitingResponse = pWaitingResponse_,
        continueResponse = pContinueResponse_
      }

-- | Specifies whether the bot will wait for a user to respond. When this
-- field is false, wait and continue responses for a slot aren\'t used. If
-- the @active@ field isn\'t specified, the default is true.
waitAndContinueSpecification_active :: Lens.Lens' WaitAndContinueSpecification (Prelude.Maybe Prelude.Bool)
waitAndContinueSpecification_active = Lens.lens (\WaitAndContinueSpecification' {active} -> active) (\s@WaitAndContinueSpecification' {} a -> s {active = a} :: WaitAndContinueSpecification)

-- | A response that Amazon Lex sends periodically to the user to indicate
-- that the bot is still waiting for input from the user.
waitAndContinueSpecification_stillWaitingResponse :: Lens.Lens' WaitAndContinueSpecification (Prelude.Maybe StillWaitingResponseSpecification)
waitAndContinueSpecification_stillWaitingResponse = Lens.lens (\WaitAndContinueSpecification' {stillWaitingResponse} -> stillWaitingResponse) (\s@WaitAndContinueSpecification' {} a -> s {stillWaitingResponse = a} :: WaitAndContinueSpecification)

-- | The response that Amazon Lex sends to indicate that the bot is waiting
-- for the conversation to continue.
waitAndContinueSpecification_waitingResponse :: Lens.Lens' WaitAndContinueSpecification ResponseSpecification
waitAndContinueSpecification_waitingResponse = Lens.lens (\WaitAndContinueSpecification' {waitingResponse} -> waitingResponse) (\s@WaitAndContinueSpecification' {} a -> s {waitingResponse = a} :: WaitAndContinueSpecification)

-- | The response that Amazon Lex sends to indicate that the bot is ready to
-- continue the conversation.
waitAndContinueSpecification_continueResponse :: Lens.Lens' WaitAndContinueSpecification ResponseSpecification
waitAndContinueSpecification_continueResponse = Lens.lens (\WaitAndContinueSpecification' {continueResponse} -> continueResponse) (\s@WaitAndContinueSpecification' {} a -> s {continueResponse = a} :: WaitAndContinueSpecification)

instance Data.FromJSON WaitAndContinueSpecification where
  parseJSON =
    Data.withObject
      "WaitAndContinueSpecification"
      ( \x ->
          WaitAndContinueSpecification'
            Prelude.<$> (x Data..:? "active")
            Prelude.<*> (x Data..:? "stillWaitingResponse")
            Prelude.<*> (x Data..: "waitingResponse")
            Prelude.<*> (x Data..: "continueResponse")
      )

instance
  Prelude.Hashable
    WaitAndContinueSpecification
  where
  hashWithSalt _salt WaitAndContinueSpecification' {..} =
    _salt
      `Prelude.hashWithSalt` active
      `Prelude.hashWithSalt` stillWaitingResponse
      `Prelude.hashWithSalt` waitingResponse
      `Prelude.hashWithSalt` continueResponse

instance Prelude.NFData WaitAndContinueSpecification where
  rnf WaitAndContinueSpecification' {..} =
    Prelude.rnf active
      `Prelude.seq` Prelude.rnf stillWaitingResponse
      `Prelude.seq` Prelude.rnf waitingResponse
      `Prelude.seq` Prelude.rnf continueResponse

instance Data.ToJSON WaitAndContinueSpecification where
  toJSON WaitAndContinueSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("active" Data..=) Prelude.<$> active,
            ("stillWaitingResponse" Data..=)
              Prelude.<$> stillWaitingResponse,
            Prelude.Just
              ("waitingResponse" Data..= waitingResponse),
            Prelude.Just
              ("continueResponse" Data..= continueResponse)
          ]
      )
