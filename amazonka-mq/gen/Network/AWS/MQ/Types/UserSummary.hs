{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MQ.Types.UserSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.UserSummary where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types.ChangeType
import qualified Network.AWS.Prelude as Prelude

-- | Returns a list of all broker users.
--
-- /See:/ 'newUserSummary' smart constructor.
data UserSummary = UserSummary'
  { -- | The type of change pending for the broker user.
    pendingChange :: Prelude.Maybe ChangeType,
    -- | Required. The username of the broker user. This value can contain only
    -- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
    -- ~). This value must be 2-100 characters long.
    username :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  UserSummary
newUserSummary =
  UserSummary'
    { pendingChange = Prelude.Nothing,
      username = Prelude.Nothing
    }

-- | The type of change pending for the broker user.
userSummary_pendingChange :: Lens.Lens' UserSummary (Prelude.Maybe ChangeType)
userSummary_pendingChange = Lens.lens (\UserSummary' {pendingChange} -> pendingChange) (\s@UserSummary' {} a -> s {pendingChange = a} :: UserSummary)

-- | Required. The username of the broker user. This value can contain only
-- alphanumeric characters, dashes, periods, underscores, and tildes (- . _
-- ~). This value must be 2-100 characters long.
userSummary_username :: Lens.Lens' UserSummary (Prelude.Maybe Prelude.Text)
userSummary_username = Lens.lens (\UserSummary' {username} -> username) (\s@UserSummary' {} a -> s {username = a} :: UserSummary)

instance Prelude.FromJSON UserSummary where
  parseJSON =
    Prelude.withObject
      "UserSummary"
      ( \x ->
          UserSummary'
            Prelude.<$> (x Prelude..:? "pendingChange")
            Prelude.<*> (x Prelude..:? "username")
      )

instance Prelude.Hashable UserSummary

instance Prelude.NFData UserSummary
