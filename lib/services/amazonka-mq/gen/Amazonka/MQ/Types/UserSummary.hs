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
-- Module      : Amazonka.MQ.Types.UserSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MQ.Types.UserSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MQ.Types.ChangeType
import qualified Amazonka.Prelude as Prelude

-- | Returns a list of all broker users. Does not apply to RabbitMQ brokers.
--
-- /See:/ 'newUserSummary' smart constructor.
data UserSummary = UserSummary'
  { -- | The type of change pending for the broker user.
    pendingChange :: Prelude.Maybe ChangeType,
    -- | Required. The username of the broker user. This value can contain only
    -- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
    -- ~). This value must be 2-100 characters long.
    username :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pendingChange', 'userSummary_pendingChange' - The type of change pending for the broker user.
--
-- 'username', 'userSummary_username' - Required. The username of the broker user. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 2-100 characters long.
newUserSummary ::
  -- | 'username'
  Prelude.Text ->
  UserSummary
newUserSummary pUsername_ =
  UserSummary'
    { pendingChange = Prelude.Nothing,
      username = pUsername_
    }

-- | The type of change pending for the broker user.
userSummary_pendingChange :: Lens.Lens' UserSummary (Prelude.Maybe ChangeType)
userSummary_pendingChange = Lens.lens (\UserSummary' {pendingChange} -> pendingChange) (\s@UserSummary' {} a -> s {pendingChange = a} :: UserSummary)

-- | Required. The username of the broker user. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 2-100 characters long.
userSummary_username :: Lens.Lens' UserSummary Prelude.Text
userSummary_username = Lens.lens (\UserSummary' {username} -> username) (\s@UserSummary' {} a -> s {username = a} :: UserSummary)

instance Data.FromJSON UserSummary where
  parseJSON =
    Data.withObject
      "UserSummary"
      ( \x ->
          UserSummary'
            Prelude.<$> (x Data..:? "pendingChange")
            Prelude.<*> (x Data..: "username")
      )

instance Prelude.Hashable UserSummary where
  hashWithSalt _salt UserSummary' {..} =
    _salt
      `Prelude.hashWithSalt` pendingChange
      `Prelude.hashWithSalt` username

instance Prelude.NFData UserSummary where
  rnf UserSummary' {..} =
    Prelude.rnf pendingChange
      `Prelude.seq` Prelude.rnf username
