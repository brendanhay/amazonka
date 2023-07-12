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
-- Module      : Amazonka.IdentityStore.Types.MemberId
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IdentityStore.Types.MemberId where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object containing the identifier of a group member.
--
-- /See:/ 'newMemberId' smart constructor.
data MemberId = MemberId'
  { -- | An object containing the identifiers of resources that can be members.
    userId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MemberId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userId', 'memberId_userId' - An object containing the identifiers of resources that can be members.
newMemberId ::
  MemberId
newMemberId = MemberId' {userId = Prelude.Nothing}

-- | An object containing the identifiers of resources that can be members.
memberId_userId :: Lens.Lens' MemberId (Prelude.Maybe Prelude.Text)
memberId_userId = Lens.lens (\MemberId' {userId} -> userId) (\s@MemberId' {} a -> s {userId = a} :: MemberId)

instance Data.FromJSON MemberId where
  parseJSON =
    Data.withObject
      "MemberId"
      (\x -> MemberId' Prelude.<$> (x Data..:? "UserId"))

instance Prelude.Hashable MemberId where
  hashWithSalt _salt MemberId' {..} =
    _salt `Prelude.hashWithSalt` userId

instance Prelude.NFData MemberId where
  rnf MemberId' {..} = Prelude.rnf userId

instance Data.ToJSON MemberId where
  toJSON MemberId' {..} =
    Data.object
      ( Prelude.catMaybes
          [("UserId" Data..=) Prelude.<$> userId]
      )
