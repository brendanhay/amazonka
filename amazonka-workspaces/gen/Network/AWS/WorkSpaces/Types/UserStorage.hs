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
-- Module      : Network.AWS.WorkSpaces.Types.UserStorage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.UserStorage where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the user storage for a WorkSpace bundle.
--
-- /See:/ 'newUserStorage' smart constructor.
data UserStorage = UserStorage'
  { -- | The size of the user storage.
    capacity :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UserStorage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacity', 'userStorage_capacity' - The size of the user storage.
newUserStorage ::
  UserStorage
newUserStorage =
  UserStorage' {capacity = Core.Nothing}

-- | The size of the user storage.
userStorage_capacity :: Lens.Lens' UserStorage (Core.Maybe Core.Text)
userStorage_capacity = Lens.lens (\UserStorage' {capacity} -> capacity) (\s@UserStorage' {} a -> s {capacity = a} :: UserStorage)

instance Core.FromJSON UserStorage where
  parseJSON =
    Core.withObject
      "UserStorage"
      ( \x ->
          UserStorage' Core.<$> (x Core..:? "Capacity")
      )

instance Core.Hashable UserStorage

instance Core.NFData UserStorage
