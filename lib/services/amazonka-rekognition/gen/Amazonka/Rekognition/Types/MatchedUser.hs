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
-- Module      : Amazonka.Rekognition.Types.MatchedUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.MatchedUser where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.UserStatus

-- | Contains metadata for a UserID matched with a given face.
--
-- /See:/ 'newMatchedUser' smart constructor.
data MatchedUser = MatchedUser'
  { -- | A provided ID for the UserID. Unique within the collection.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The status of the user matched to a provided FaceID.
    userStatus :: Prelude.Maybe UserStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MatchedUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userId', 'matchedUser_userId' - A provided ID for the UserID. Unique within the collection.
--
-- 'userStatus', 'matchedUser_userStatus' - The status of the user matched to a provided FaceID.
newMatchedUser ::
  MatchedUser
newMatchedUser =
  MatchedUser'
    { userId = Prelude.Nothing,
      userStatus = Prelude.Nothing
    }

-- | A provided ID for the UserID. Unique within the collection.
matchedUser_userId :: Lens.Lens' MatchedUser (Prelude.Maybe Prelude.Text)
matchedUser_userId = Lens.lens (\MatchedUser' {userId} -> userId) (\s@MatchedUser' {} a -> s {userId = a} :: MatchedUser)

-- | The status of the user matched to a provided FaceID.
matchedUser_userStatus :: Lens.Lens' MatchedUser (Prelude.Maybe UserStatus)
matchedUser_userStatus = Lens.lens (\MatchedUser' {userStatus} -> userStatus) (\s@MatchedUser' {} a -> s {userStatus = a} :: MatchedUser)

instance Data.FromJSON MatchedUser where
  parseJSON =
    Data.withObject
      "MatchedUser"
      ( \x ->
          MatchedUser'
            Prelude.<$> (x Data..:? "UserId")
            Prelude.<*> (x Data..:? "UserStatus")
      )

instance Prelude.Hashable MatchedUser where
  hashWithSalt _salt MatchedUser' {..} =
    _salt
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` userStatus

instance Prelude.NFData MatchedUser where
  rnf MatchedUser' {..} =
    Prelude.rnf userId
      `Prelude.seq` Prelude.rnf userStatus
