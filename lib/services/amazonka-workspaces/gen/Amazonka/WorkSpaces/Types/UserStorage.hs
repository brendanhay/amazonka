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
-- Module      : Amazonka.WorkSpaces.Types.UserStorage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.UserStorage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the user volume for a WorkSpace bundle.
--
-- /See:/ 'newUserStorage' smart constructor.
data UserStorage = UserStorage'
  { -- | The size of the user volume.
    capacity :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserStorage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacity', 'userStorage_capacity' - The size of the user volume.
newUserStorage ::
  UserStorage
newUserStorage =
  UserStorage' {capacity = Prelude.Nothing}

-- | The size of the user volume.
userStorage_capacity :: Lens.Lens' UserStorage (Prelude.Maybe Prelude.Text)
userStorage_capacity = Lens.lens (\UserStorage' {capacity} -> capacity) (\s@UserStorage' {} a -> s {capacity = a} :: UserStorage)

instance Data.FromJSON UserStorage where
  parseJSON =
    Data.withObject
      "UserStorage"
      ( \x ->
          UserStorage' Prelude.<$> (x Data..:? "Capacity")
      )

instance Prelude.Hashable UserStorage where
  hashWithSalt _salt UserStorage' {..} =
    _salt `Prelude.hashWithSalt` capacity

instance Prelude.NFData UserStorage where
  rnf UserStorage' {..} = Prelude.rnf capacity

instance Data.ToJSON UserStorage where
  toJSON UserStorage' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Capacity" Data..=) Prelude.<$> capacity]
      )
