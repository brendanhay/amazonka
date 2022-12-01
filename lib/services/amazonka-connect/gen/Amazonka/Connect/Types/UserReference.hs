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
-- Module      : Amazonka.Connect.Types.UserReference
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.UserReference where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the user.
--
-- /See:/ 'newUserReference' smart constructor.
data UserReference = UserReference'
  { -- | The Amazon Resource Name (ARN) for the user.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the user.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'userReference_arn' - The Amazon Resource Name (ARN) for the user.
--
-- 'id', 'userReference_id' - The unique identifier for the user.
newUserReference ::
  UserReference
newUserReference =
  UserReference'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) for the user.
userReference_arn :: Lens.Lens' UserReference (Prelude.Maybe Prelude.Text)
userReference_arn = Lens.lens (\UserReference' {arn} -> arn) (\s@UserReference' {} a -> s {arn = a} :: UserReference)

-- | The unique identifier for the user.
userReference_id :: Lens.Lens' UserReference (Prelude.Maybe Prelude.Text)
userReference_id = Lens.lens (\UserReference' {id} -> id) (\s@UserReference' {} a -> s {id = a} :: UserReference)

instance Core.FromJSON UserReference where
  parseJSON =
    Core.withObject
      "UserReference"
      ( \x ->
          UserReference'
            Prelude.<$> (x Core..:? "Arn") Prelude.<*> (x Core..:? "Id")
      )

instance Prelude.Hashable UserReference where
  hashWithSalt _salt UserReference' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id

instance Prelude.NFData UserReference where
  rnf UserReference' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf id
