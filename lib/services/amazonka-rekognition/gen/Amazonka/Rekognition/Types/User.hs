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
-- Module      : Amazonka.Rekognition.Types.User
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.User where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.UserStatus

-- | Metadata of the user stored in a collection.
--
-- /See:/ 'newUser' smart constructor.
data User = User'
  { -- | A provided ID for the User. Unique within the collection.
    userId :: Prelude.Maybe Prelude.Text,
    -- | Communicates if the UserID has been updated with latest set of faces to
    -- be associated with the UserID.
    userStatus :: Prelude.Maybe UserStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'User' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userId', 'user_userId' - A provided ID for the User. Unique within the collection.
--
-- 'userStatus', 'user_userStatus' - Communicates if the UserID has been updated with latest set of faces to
-- be associated with the UserID.
newUser ::
  User
newUser =
  User'
    { userId = Prelude.Nothing,
      userStatus = Prelude.Nothing
    }

-- | A provided ID for the User. Unique within the collection.
user_userId :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_userId = Lens.lens (\User' {userId} -> userId) (\s@User' {} a -> s {userId = a} :: User)

-- | Communicates if the UserID has been updated with latest set of faces to
-- be associated with the UserID.
user_userStatus :: Lens.Lens' User (Prelude.Maybe UserStatus)
user_userStatus = Lens.lens (\User' {userStatus} -> userStatus) (\s@User' {} a -> s {userStatus = a} :: User)

instance Data.FromJSON User where
  parseJSON =
    Data.withObject
      "User"
      ( \x ->
          User'
            Prelude.<$> (x Data..:? "UserId")
            Prelude.<*> (x Data..:? "UserStatus")
      )

instance Prelude.Hashable User where
  hashWithSalt _salt User' {..} =
    _salt
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` userStatus

instance Prelude.NFData User where
  rnf User' {..} =
    Prelude.rnf userId
      `Prelude.seq` Prelude.rnf userStatus
