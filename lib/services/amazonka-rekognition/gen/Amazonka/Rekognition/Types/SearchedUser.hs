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
-- Module      : Amazonka.Rekognition.Types.SearchedUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.SearchedUser where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains metadata about a User searched for within a collection.
--
-- /See:/ 'newSearchedUser' smart constructor.
data SearchedUser = SearchedUser'
  { -- | A provided ID for the UserID. Unique within the collection.
    userId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchedUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userId', 'searchedUser_userId' - A provided ID for the UserID. Unique within the collection.
newSearchedUser ::
  SearchedUser
newSearchedUser =
  SearchedUser' {userId = Prelude.Nothing}

-- | A provided ID for the UserID. Unique within the collection.
searchedUser_userId :: Lens.Lens' SearchedUser (Prelude.Maybe Prelude.Text)
searchedUser_userId = Lens.lens (\SearchedUser' {userId} -> userId) (\s@SearchedUser' {} a -> s {userId = a} :: SearchedUser)

instance Data.FromJSON SearchedUser where
  parseJSON =
    Data.withObject
      "SearchedUser"
      ( \x ->
          SearchedUser' Prelude.<$> (x Data..:? "UserId")
      )

instance Prelude.Hashable SearchedUser where
  hashWithSalt _salt SearchedUser' {..} =
    _salt `Prelude.hashWithSalt` userId

instance Prelude.NFData SearchedUser where
  rnf SearchedUser' {..} = Prelude.rnf userId
