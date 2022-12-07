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
-- Module      : Amazonka.QuickSight.Types.MemberIdArnPair
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.MemberIdArnPair where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that consists of a member Amazon Resource Name (ARN) and a
-- member ID.
--
-- /See:/ 'newMemberIdArnPair' smart constructor.
data MemberIdArnPair = MemberIdArnPair'
  { -- | The ID of the member.
    memberId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the member.
    memberArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MemberIdArnPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memberId', 'memberIdArnPair_memberId' - The ID of the member.
--
-- 'memberArn', 'memberIdArnPair_memberArn' - The Amazon Resource Name (ARN) of the member.
newMemberIdArnPair ::
  MemberIdArnPair
newMemberIdArnPair =
  MemberIdArnPair'
    { memberId = Prelude.Nothing,
      memberArn = Prelude.Nothing
    }

-- | The ID of the member.
memberIdArnPair_memberId :: Lens.Lens' MemberIdArnPair (Prelude.Maybe Prelude.Text)
memberIdArnPair_memberId = Lens.lens (\MemberIdArnPair' {memberId} -> memberId) (\s@MemberIdArnPair' {} a -> s {memberId = a} :: MemberIdArnPair)

-- | The Amazon Resource Name (ARN) of the member.
memberIdArnPair_memberArn :: Lens.Lens' MemberIdArnPair (Prelude.Maybe Prelude.Text)
memberIdArnPair_memberArn = Lens.lens (\MemberIdArnPair' {memberArn} -> memberArn) (\s@MemberIdArnPair' {} a -> s {memberArn = a} :: MemberIdArnPair)

instance Data.FromJSON MemberIdArnPair where
  parseJSON =
    Data.withObject
      "MemberIdArnPair"
      ( \x ->
          MemberIdArnPair'
            Prelude.<$> (x Data..:? "MemberId")
            Prelude.<*> (x Data..:? "MemberArn")
      )

instance Prelude.Hashable MemberIdArnPair where
  hashWithSalt _salt MemberIdArnPair' {..} =
    _salt `Prelude.hashWithSalt` memberId
      `Prelude.hashWithSalt` memberArn

instance Prelude.NFData MemberIdArnPair where
  rnf MemberIdArnPair' {..} =
    Prelude.rnf memberId
      `Prelude.seq` Prelude.rnf memberArn
