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
-- Module      : Amazonka.LexV2Models.Types.UserTurnSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.UserTurnSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.UserTurnInputSpecification
import Amazonka.LexV2Models.Types.UserTurnOutputSpecification
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the expected and input values for the user
-- turn.
--
-- /See:/ 'newUserTurnSpecification' smart constructor.
data UserTurnSpecification = UserTurnSpecification'
  { -- | Contains information about the user messages in the turn in the input.
    input :: UserTurnInputSpecification,
    -- | Contains results about the expected output for the user turn.
    expected :: UserTurnOutputSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserTurnSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'input', 'userTurnSpecification_input' - Contains information about the user messages in the turn in the input.
--
-- 'expected', 'userTurnSpecification_expected' - Contains results about the expected output for the user turn.
newUserTurnSpecification ::
  -- | 'input'
  UserTurnInputSpecification ->
  -- | 'expected'
  UserTurnOutputSpecification ->
  UserTurnSpecification
newUserTurnSpecification pInput_ pExpected_ =
  UserTurnSpecification'
    { input = pInput_,
      expected = pExpected_
    }

-- | Contains information about the user messages in the turn in the input.
userTurnSpecification_input :: Lens.Lens' UserTurnSpecification UserTurnInputSpecification
userTurnSpecification_input = Lens.lens (\UserTurnSpecification' {input} -> input) (\s@UserTurnSpecification' {} a -> s {input = a} :: UserTurnSpecification)

-- | Contains results about the expected output for the user turn.
userTurnSpecification_expected :: Lens.Lens' UserTurnSpecification UserTurnOutputSpecification
userTurnSpecification_expected = Lens.lens (\UserTurnSpecification' {expected} -> expected) (\s@UserTurnSpecification' {} a -> s {expected = a} :: UserTurnSpecification)

instance Data.FromJSON UserTurnSpecification where
  parseJSON =
    Data.withObject
      "UserTurnSpecification"
      ( \x ->
          UserTurnSpecification'
            Prelude.<$> (x Data..: "input")
            Prelude.<*> (x Data..: "expected")
      )

instance Prelude.Hashable UserTurnSpecification where
  hashWithSalt _salt UserTurnSpecification' {..} =
    _salt
      `Prelude.hashWithSalt` input
      `Prelude.hashWithSalt` expected

instance Prelude.NFData UserTurnSpecification where
  rnf UserTurnSpecification' {..} =
    Prelude.rnf input
      `Prelude.seq` Prelude.rnf expected
