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
-- Module      : Amazonka.KafkaConnect.Types.StateDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.StateDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details about the state of a resource.
--
-- /See:/ 'newStateDescription' smart constructor.
data StateDescription = StateDescription'
  { -- | A message that describes the state of a resource.
    message :: Prelude.Maybe Prelude.Text,
    -- | A code that describes the state of a resource.
    code :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StateDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'stateDescription_message' - A message that describes the state of a resource.
--
-- 'code', 'stateDescription_code' - A code that describes the state of a resource.
newStateDescription ::
  StateDescription
newStateDescription =
  StateDescription'
    { message = Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | A message that describes the state of a resource.
stateDescription_message :: Lens.Lens' StateDescription (Prelude.Maybe Prelude.Text)
stateDescription_message = Lens.lens (\StateDescription' {message} -> message) (\s@StateDescription' {} a -> s {message = a} :: StateDescription)

-- | A code that describes the state of a resource.
stateDescription_code :: Lens.Lens' StateDescription (Prelude.Maybe Prelude.Text)
stateDescription_code = Lens.lens (\StateDescription' {code} -> code) (\s@StateDescription' {} a -> s {code = a} :: StateDescription)

instance Core.FromJSON StateDescription where
  parseJSON =
    Core.withObject
      "StateDescription"
      ( \x ->
          StateDescription'
            Prelude.<$> (x Core..:? "message")
            Prelude.<*> (x Core..:? "code")
      )

instance Prelude.Hashable StateDescription where
  hashWithSalt _salt StateDescription' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` code

instance Prelude.NFData StateDescription where
  rnf StateDescription' {..} =
    Prelude.rnf message `Prelude.seq` Prelude.rnf code
