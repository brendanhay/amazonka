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
-- Module      : Amazonka.CleanRooms.Types.MemberSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.MemberSpecification where

import Amazonka.CleanRooms.Types.MemberAbility
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Basic metadata used to construct a new member.
--
-- /See:/ 'newMemberSpecification' smart constructor.
data MemberSpecification = MemberSpecification'
  { -- | The identifier used to reference members of the collaboration. Currently
    -- only supports AWS Account ID.
    accountId :: Prelude.Text,
    -- | The abilities granted to the collaboration member.
    memberAbilities :: [MemberAbility],
    -- | The member\'s display name.
    displayName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MemberSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'memberSpecification_accountId' - The identifier used to reference members of the collaboration. Currently
-- only supports AWS Account ID.
--
-- 'memberAbilities', 'memberSpecification_memberAbilities' - The abilities granted to the collaboration member.
--
-- 'displayName', 'memberSpecification_displayName' - The member\'s display name.
newMemberSpecification ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'displayName'
  Prelude.Text ->
  MemberSpecification
newMemberSpecification pAccountId_ pDisplayName_ =
  MemberSpecification'
    { accountId = pAccountId_,
      memberAbilities = Prelude.mempty,
      displayName = pDisplayName_
    }

-- | The identifier used to reference members of the collaboration. Currently
-- only supports AWS Account ID.
memberSpecification_accountId :: Lens.Lens' MemberSpecification Prelude.Text
memberSpecification_accountId = Lens.lens (\MemberSpecification' {accountId} -> accountId) (\s@MemberSpecification' {} a -> s {accountId = a} :: MemberSpecification)

-- | The abilities granted to the collaboration member.
memberSpecification_memberAbilities :: Lens.Lens' MemberSpecification [MemberAbility]
memberSpecification_memberAbilities = Lens.lens (\MemberSpecification' {memberAbilities} -> memberAbilities) (\s@MemberSpecification' {} a -> s {memberAbilities = a} :: MemberSpecification) Prelude.. Lens.coerced

-- | The member\'s display name.
memberSpecification_displayName :: Lens.Lens' MemberSpecification Prelude.Text
memberSpecification_displayName = Lens.lens (\MemberSpecification' {displayName} -> displayName) (\s@MemberSpecification' {} a -> s {displayName = a} :: MemberSpecification)

instance Prelude.Hashable MemberSpecification where
  hashWithSalt _salt MemberSpecification' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` memberAbilities
      `Prelude.hashWithSalt` displayName

instance Prelude.NFData MemberSpecification where
  rnf MemberSpecification' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf memberAbilities
      `Prelude.seq` Prelude.rnf displayName

instance Data.ToJSON MemberSpecification where
  toJSON MemberSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("accountId" Data..= accountId),
            Prelude.Just
              ("memberAbilities" Data..= memberAbilities),
            Prelude.Just ("displayName" Data..= displayName)
          ]
      )
