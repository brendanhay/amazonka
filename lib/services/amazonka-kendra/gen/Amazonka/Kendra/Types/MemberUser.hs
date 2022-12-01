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
-- Module      : Amazonka.Kendra.Types.MemberUser
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.MemberUser where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The users that belong to a group.
--
-- /See:/ 'newMemberUser' smart constructor.
data MemberUser = MemberUser'
  { -- | The identifier of the user you want to map to a group.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MemberUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userId', 'memberUser_userId' - The identifier of the user you want to map to a group.
newMemberUser ::
  -- | 'userId'
  Prelude.Text ->
  MemberUser
newMemberUser pUserId_ =
  MemberUser' {userId = pUserId_}

-- | The identifier of the user you want to map to a group.
memberUser_userId :: Lens.Lens' MemberUser Prelude.Text
memberUser_userId = Lens.lens (\MemberUser' {userId} -> userId) (\s@MemberUser' {} a -> s {userId = a} :: MemberUser)

instance Prelude.Hashable MemberUser where
  hashWithSalt _salt MemberUser' {..} =
    _salt `Prelude.hashWithSalt` userId

instance Prelude.NFData MemberUser where
  rnf MemberUser' {..} = Prelude.rnf userId

instance Core.ToJSON MemberUser where
  toJSON MemberUser' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("UserId" Core..= userId)]
      )
