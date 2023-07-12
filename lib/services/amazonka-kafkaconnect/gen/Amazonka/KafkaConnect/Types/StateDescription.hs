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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.StateDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about the state of a resource.
--
-- /See:/ 'newStateDescription' smart constructor.
data StateDescription = StateDescription'
  { -- | A code that describes the state of a resource.
    code :: Prelude.Maybe Prelude.Text,
    -- | A message that describes the state of a resource.
    message :: Prelude.Maybe Prelude.Text
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
-- 'code', 'stateDescription_code' - A code that describes the state of a resource.
--
-- 'message', 'stateDescription_message' - A message that describes the state of a resource.
newStateDescription ::
  StateDescription
newStateDescription =
  StateDescription'
    { code = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | A code that describes the state of a resource.
stateDescription_code :: Lens.Lens' StateDescription (Prelude.Maybe Prelude.Text)
stateDescription_code = Lens.lens (\StateDescription' {code} -> code) (\s@StateDescription' {} a -> s {code = a} :: StateDescription)

-- | A message that describes the state of a resource.
stateDescription_message :: Lens.Lens' StateDescription (Prelude.Maybe Prelude.Text)
stateDescription_message = Lens.lens (\StateDescription' {message} -> message) (\s@StateDescription' {} a -> s {message = a} :: StateDescription)

instance Data.FromJSON StateDescription where
  parseJSON =
    Data.withObject
      "StateDescription"
      ( \x ->
          StateDescription'
            Prelude.<$> (x Data..:? "code")
            Prelude.<*> (x Data..:? "message")
      )

instance Prelude.Hashable StateDescription where
  hashWithSalt _salt StateDescription' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData StateDescription where
  rnf StateDescription' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
