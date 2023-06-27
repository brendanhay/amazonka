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
-- Module      : Amazonka.Rekognition.Types.UserMatch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.UserMatch where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.MatchedUser

-- | Provides UserID metadata along with the confidence in the match of this
-- UserID with the input face.
--
-- /See:/ 'newUserMatch' smart constructor.
data UserMatch = UserMatch'
  { -- | Describes the UserID metadata.
    similarity :: Prelude.Maybe Prelude.Double,
    -- | Confidence in the match of this UserID with the input face.
    user :: Prelude.Maybe MatchedUser
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserMatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'similarity', 'userMatch_similarity' - Describes the UserID metadata.
--
-- 'user', 'userMatch_user' - Confidence in the match of this UserID with the input face.
newUserMatch ::
  UserMatch
newUserMatch =
  UserMatch'
    { similarity = Prelude.Nothing,
      user = Prelude.Nothing
    }

-- | Describes the UserID metadata.
userMatch_similarity :: Lens.Lens' UserMatch (Prelude.Maybe Prelude.Double)
userMatch_similarity = Lens.lens (\UserMatch' {similarity} -> similarity) (\s@UserMatch' {} a -> s {similarity = a} :: UserMatch)

-- | Confidence in the match of this UserID with the input face.
userMatch_user :: Lens.Lens' UserMatch (Prelude.Maybe MatchedUser)
userMatch_user = Lens.lens (\UserMatch' {user} -> user) (\s@UserMatch' {} a -> s {user = a} :: UserMatch)

instance Data.FromJSON UserMatch where
  parseJSON =
    Data.withObject
      "UserMatch"
      ( \x ->
          UserMatch'
            Prelude.<$> (x Data..:? "Similarity")
            Prelude.<*> (x Data..:? "User")
      )

instance Prelude.Hashable UserMatch where
  hashWithSalt _salt UserMatch' {..} =
    _salt
      `Prelude.hashWithSalt` similarity
      `Prelude.hashWithSalt` user

instance Prelude.NFData UserMatch where
  rnf UserMatch' {..} =
    Prelude.rnf similarity
      `Prelude.seq` Prelude.rnf user
