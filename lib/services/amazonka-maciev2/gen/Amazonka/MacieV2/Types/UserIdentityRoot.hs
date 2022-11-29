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
-- Module      : Amazonka.MacieV2.Types.UserIdentityRoot
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.UserIdentityRoot where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about an Amazon Web Services account and entity
-- that performed an action on an affected resource. The action was
-- performed using the credentials for your Amazon Web Services account.
--
-- /See:/ 'newUserIdentityRoot' smart constructor.
data UserIdentityRoot = UserIdentityRoot'
  { -- | The unique identifier for the entity that performed the action.
    principalId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the principal that performed the
    -- action. The last section of the ARN contains the name of the user or
    -- role that performed the action.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the Amazon Web Services account.
    accountId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserIdentityRoot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principalId', 'userIdentityRoot_principalId' - The unique identifier for the entity that performed the action.
--
-- 'arn', 'userIdentityRoot_arn' - The Amazon Resource Name (ARN) of the principal that performed the
-- action. The last section of the ARN contains the name of the user or
-- role that performed the action.
--
-- 'accountId', 'userIdentityRoot_accountId' - The unique identifier for the Amazon Web Services account.
newUserIdentityRoot ::
  UserIdentityRoot
newUserIdentityRoot =
  UserIdentityRoot'
    { principalId = Prelude.Nothing,
      arn = Prelude.Nothing,
      accountId = Prelude.Nothing
    }

-- | The unique identifier for the entity that performed the action.
userIdentityRoot_principalId :: Lens.Lens' UserIdentityRoot (Prelude.Maybe Prelude.Text)
userIdentityRoot_principalId = Lens.lens (\UserIdentityRoot' {principalId} -> principalId) (\s@UserIdentityRoot' {} a -> s {principalId = a} :: UserIdentityRoot)

-- | The Amazon Resource Name (ARN) of the principal that performed the
-- action. The last section of the ARN contains the name of the user or
-- role that performed the action.
userIdentityRoot_arn :: Lens.Lens' UserIdentityRoot (Prelude.Maybe Prelude.Text)
userIdentityRoot_arn = Lens.lens (\UserIdentityRoot' {arn} -> arn) (\s@UserIdentityRoot' {} a -> s {arn = a} :: UserIdentityRoot)

-- | The unique identifier for the Amazon Web Services account.
userIdentityRoot_accountId :: Lens.Lens' UserIdentityRoot (Prelude.Maybe Prelude.Text)
userIdentityRoot_accountId = Lens.lens (\UserIdentityRoot' {accountId} -> accountId) (\s@UserIdentityRoot' {} a -> s {accountId = a} :: UserIdentityRoot)

instance Core.FromJSON UserIdentityRoot where
  parseJSON =
    Core.withObject
      "UserIdentityRoot"
      ( \x ->
          UserIdentityRoot'
            Prelude.<$> (x Core..:? "principalId")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "accountId")
      )

instance Prelude.Hashable UserIdentityRoot where
  hashWithSalt _salt UserIdentityRoot' {..} =
    _salt `Prelude.hashWithSalt` principalId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` accountId

instance Prelude.NFData UserIdentityRoot where
  rnf UserIdentityRoot' {..} =
    Prelude.rnf principalId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf accountId
