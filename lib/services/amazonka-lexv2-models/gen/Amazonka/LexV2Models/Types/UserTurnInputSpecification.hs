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
-- Module      : Amazonka.LexV2Models.Types.UserTurnInputSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.UserTurnInputSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.InputSessionStateSpecification
import Amazonka.LexV2Models.Types.UtteranceInputSpecification
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the user messages in the turn in the input.
--
-- /See:/ 'newUserTurnInputSpecification' smart constructor.
data UserTurnInputSpecification = UserTurnInputSpecification'
  { -- | Request attributes of the user turn.
    requestAttributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Contains information about the session state in the input.
    sessionState :: Prelude.Maybe InputSessionStateSpecification,
    -- | The utterance input in the user turn.
    utteranceInput :: UtteranceInputSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserTurnInputSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestAttributes', 'userTurnInputSpecification_requestAttributes' - Request attributes of the user turn.
--
-- 'sessionState', 'userTurnInputSpecification_sessionState' - Contains information about the session state in the input.
--
-- 'utteranceInput', 'userTurnInputSpecification_utteranceInput' - The utterance input in the user turn.
newUserTurnInputSpecification ::
  -- | 'utteranceInput'
  UtteranceInputSpecification ->
  UserTurnInputSpecification
newUserTurnInputSpecification pUtteranceInput_ =
  UserTurnInputSpecification'
    { requestAttributes =
        Prelude.Nothing,
      sessionState = Prelude.Nothing,
      utteranceInput = pUtteranceInput_
    }

-- | Request attributes of the user turn.
userTurnInputSpecification_requestAttributes :: Lens.Lens' UserTurnInputSpecification (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
userTurnInputSpecification_requestAttributes = Lens.lens (\UserTurnInputSpecification' {requestAttributes} -> requestAttributes) (\s@UserTurnInputSpecification' {} a -> s {requestAttributes = a} :: UserTurnInputSpecification) Prelude.. Lens.mapping Lens.coerced

-- | Contains information about the session state in the input.
userTurnInputSpecification_sessionState :: Lens.Lens' UserTurnInputSpecification (Prelude.Maybe InputSessionStateSpecification)
userTurnInputSpecification_sessionState = Lens.lens (\UserTurnInputSpecification' {sessionState} -> sessionState) (\s@UserTurnInputSpecification' {} a -> s {sessionState = a} :: UserTurnInputSpecification)

-- | The utterance input in the user turn.
userTurnInputSpecification_utteranceInput :: Lens.Lens' UserTurnInputSpecification UtteranceInputSpecification
userTurnInputSpecification_utteranceInput = Lens.lens (\UserTurnInputSpecification' {utteranceInput} -> utteranceInput) (\s@UserTurnInputSpecification' {} a -> s {utteranceInput = a} :: UserTurnInputSpecification)

instance Data.FromJSON UserTurnInputSpecification where
  parseJSON =
    Data.withObject
      "UserTurnInputSpecification"
      ( \x ->
          UserTurnInputSpecification'
            Prelude.<$> ( x
                            Data..:? "requestAttributes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "sessionState")
            Prelude.<*> (x Data..: "utteranceInput")
      )

instance Prelude.Hashable UserTurnInputSpecification where
  hashWithSalt _salt UserTurnInputSpecification' {..} =
    _salt
      `Prelude.hashWithSalt` requestAttributes
      `Prelude.hashWithSalt` sessionState
      `Prelude.hashWithSalt` utteranceInput

instance Prelude.NFData UserTurnInputSpecification where
  rnf UserTurnInputSpecification' {..} =
    Prelude.rnf requestAttributes
      `Prelude.seq` Prelude.rnf sessionState
      `Prelude.seq` Prelude.rnf utteranceInput
