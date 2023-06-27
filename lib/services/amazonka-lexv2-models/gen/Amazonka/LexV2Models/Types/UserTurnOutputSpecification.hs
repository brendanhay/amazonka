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
-- Module      : Amazonka.LexV2Models.Types.UserTurnOutputSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.UserTurnOutputSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.ActiveContext
import Amazonka.LexV2Models.Types.UserTurnIntentOutput
import qualified Amazonka.Prelude as Prelude

-- | Contains results that are output for the user turn by the test
-- execution.
--
-- /See:/ 'newUserTurnOutputSpecification' smart constructor.
data UserTurnOutputSpecification = UserTurnOutputSpecification'
  { -- | The contexts that are active in the turn.
    activeContexts :: Prelude.Maybe [ActiveContext],
    -- | The transcript that is output for the user turn by the test execution.
    transcript :: Prelude.Maybe Prelude.Text,
    -- | Contains information about the intent.
    intent :: UserTurnIntentOutput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserTurnOutputSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeContexts', 'userTurnOutputSpecification_activeContexts' - The contexts that are active in the turn.
--
-- 'transcript', 'userTurnOutputSpecification_transcript' - The transcript that is output for the user turn by the test execution.
--
-- 'intent', 'userTurnOutputSpecification_intent' - Contains information about the intent.
newUserTurnOutputSpecification ::
  -- | 'intent'
  UserTurnIntentOutput ->
  UserTurnOutputSpecification
newUserTurnOutputSpecification pIntent_ =
  UserTurnOutputSpecification'
    { activeContexts =
        Prelude.Nothing,
      transcript = Prelude.Nothing,
      intent = pIntent_
    }

-- | The contexts that are active in the turn.
userTurnOutputSpecification_activeContexts :: Lens.Lens' UserTurnOutputSpecification (Prelude.Maybe [ActiveContext])
userTurnOutputSpecification_activeContexts = Lens.lens (\UserTurnOutputSpecification' {activeContexts} -> activeContexts) (\s@UserTurnOutputSpecification' {} a -> s {activeContexts = a} :: UserTurnOutputSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The transcript that is output for the user turn by the test execution.
userTurnOutputSpecification_transcript :: Lens.Lens' UserTurnOutputSpecification (Prelude.Maybe Prelude.Text)
userTurnOutputSpecification_transcript = Lens.lens (\UserTurnOutputSpecification' {transcript} -> transcript) (\s@UserTurnOutputSpecification' {} a -> s {transcript = a} :: UserTurnOutputSpecification)

-- | Contains information about the intent.
userTurnOutputSpecification_intent :: Lens.Lens' UserTurnOutputSpecification UserTurnIntentOutput
userTurnOutputSpecification_intent = Lens.lens (\UserTurnOutputSpecification' {intent} -> intent) (\s@UserTurnOutputSpecification' {} a -> s {intent = a} :: UserTurnOutputSpecification)

instance Data.FromJSON UserTurnOutputSpecification where
  parseJSON =
    Data.withObject
      "UserTurnOutputSpecification"
      ( \x ->
          UserTurnOutputSpecification'
            Prelude.<$> (x Data..:? "activeContexts" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "transcript")
            Prelude.<*> (x Data..: "intent")
      )

instance Prelude.Hashable UserTurnOutputSpecification where
  hashWithSalt _salt UserTurnOutputSpecification' {..} =
    _salt
      `Prelude.hashWithSalt` activeContexts
      `Prelude.hashWithSalt` transcript
      `Prelude.hashWithSalt` intent

instance Prelude.NFData UserTurnOutputSpecification where
  rnf UserTurnOutputSpecification' {..} =
    Prelude.rnf activeContexts
      `Prelude.seq` Prelude.rnf transcript
      `Prelude.seq` Prelude.rnf intent
