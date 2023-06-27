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
-- Module      : Amazonka.LexV2Models.Types.InputSessionStateSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.InputSessionStateSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.ActiveContext
import Amazonka.LexV2Models.Types.RuntimeHints
import qualified Amazonka.Prelude as Prelude

-- | Specifications for the current state of the dialog between the user and
-- the bot in the test set.
--
-- /See:/ 'newInputSessionStateSpecification' smart constructor.
data InputSessionStateSpecification = InputSessionStateSpecification'
  { -- | Active contexts for the session state.
    activeContexts :: Prelude.Maybe [ActiveContext],
    -- | Runtime hints for the session state.
    runtimeHints :: Prelude.Maybe RuntimeHints,
    -- | Session attributes for the session state.
    sessionAttributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputSessionStateSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeContexts', 'inputSessionStateSpecification_activeContexts' - Active contexts for the session state.
--
-- 'runtimeHints', 'inputSessionStateSpecification_runtimeHints' - Runtime hints for the session state.
--
-- 'sessionAttributes', 'inputSessionStateSpecification_sessionAttributes' - Session attributes for the session state.
newInputSessionStateSpecification ::
  InputSessionStateSpecification
newInputSessionStateSpecification =
  InputSessionStateSpecification'
    { activeContexts =
        Prelude.Nothing,
      runtimeHints = Prelude.Nothing,
      sessionAttributes = Prelude.Nothing
    }

-- | Active contexts for the session state.
inputSessionStateSpecification_activeContexts :: Lens.Lens' InputSessionStateSpecification (Prelude.Maybe [ActiveContext])
inputSessionStateSpecification_activeContexts = Lens.lens (\InputSessionStateSpecification' {activeContexts} -> activeContexts) (\s@InputSessionStateSpecification' {} a -> s {activeContexts = a} :: InputSessionStateSpecification) Prelude.. Lens.mapping Lens.coerced

-- | Runtime hints for the session state.
inputSessionStateSpecification_runtimeHints :: Lens.Lens' InputSessionStateSpecification (Prelude.Maybe RuntimeHints)
inputSessionStateSpecification_runtimeHints = Lens.lens (\InputSessionStateSpecification' {runtimeHints} -> runtimeHints) (\s@InputSessionStateSpecification' {} a -> s {runtimeHints = a} :: InputSessionStateSpecification)

-- | Session attributes for the session state.
inputSessionStateSpecification_sessionAttributes :: Lens.Lens' InputSessionStateSpecification (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
inputSessionStateSpecification_sessionAttributes = Lens.lens (\InputSessionStateSpecification' {sessionAttributes} -> sessionAttributes) (\s@InputSessionStateSpecification' {} a -> s {sessionAttributes = a} :: InputSessionStateSpecification) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON InputSessionStateSpecification where
  parseJSON =
    Data.withObject
      "InputSessionStateSpecification"
      ( \x ->
          InputSessionStateSpecification'
            Prelude.<$> (x Data..:? "activeContexts" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "runtimeHints")
            Prelude.<*> ( x
                            Data..:? "sessionAttributes"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    InputSessionStateSpecification
  where
  hashWithSalt
    _salt
    InputSessionStateSpecification' {..} =
      _salt
        `Prelude.hashWithSalt` activeContexts
        `Prelude.hashWithSalt` runtimeHints
        `Prelude.hashWithSalt` sessionAttributes

instance
  Prelude.NFData
    InputSessionStateSpecification
  where
  rnf InputSessionStateSpecification' {..} =
    Prelude.rnf activeContexts
      `Prelude.seq` Prelude.rnf runtimeHints
      `Prelude.seq` Prelude.rnf sessionAttributes
