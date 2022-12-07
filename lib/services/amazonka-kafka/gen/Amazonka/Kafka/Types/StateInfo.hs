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
-- Module      : Amazonka.Kafka.Types.StateInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.StateInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newStateInfo' smart constructor.
data StateInfo = StateInfo'
  { message :: Prelude.Maybe Prelude.Text,
    code :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StateInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'stateInfo_message' - Undocumented member.
--
-- 'code', 'stateInfo_code' - Undocumented member.
newStateInfo ::
  StateInfo
newStateInfo =
  StateInfo'
    { message = Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | Undocumented member.
stateInfo_message :: Lens.Lens' StateInfo (Prelude.Maybe Prelude.Text)
stateInfo_message = Lens.lens (\StateInfo' {message} -> message) (\s@StateInfo' {} a -> s {message = a} :: StateInfo)

-- | Undocumented member.
stateInfo_code :: Lens.Lens' StateInfo (Prelude.Maybe Prelude.Text)
stateInfo_code = Lens.lens (\StateInfo' {code} -> code) (\s@StateInfo' {} a -> s {code = a} :: StateInfo)

instance Data.FromJSON StateInfo where
  parseJSON =
    Data.withObject
      "StateInfo"
      ( \x ->
          StateInfo'
            Prelude.<$> (x Data..:? "message")
            Prelude.<*> (x Data..:? "code")
      )

instance Prelude.Hashable StateInfo where
  hashWithSalt _salt StateInfo' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` code

instance Prelude.NFData StateInfo where
  rnf StateInfo' {..} =
    Prelude.rnf message `Prelude.seq` Prelude.rnf code
