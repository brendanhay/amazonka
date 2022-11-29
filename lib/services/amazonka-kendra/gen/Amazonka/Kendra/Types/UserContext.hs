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
-- Module      : Amazonka.Kendra.Types.UserContext
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.UserContext where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kendra.Types.DataSourceGroup
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the user context for an Amazon Kendra index.
--
-- User context filtering is a kind of personalized search with the benefit
-- of controlling access to documents. For example, not all teams that
-- search the company portal for information should access top-secret
-- company documents, nor are these documents relevant to all users. Only
-- specific users or groups of teams given access to top-secret documents
-- should see these documents in their search results.
--
-- You provide one of the following:
--
-- -   User token
--
-- -   User ID, the groups the user belongs to, and any data sources the
--     groups can access.
--
-- If you provide both, an exception is thrown.
--
-- /See:/ 'newUserContext' smart constructor.
data UserContext = UserContext'
  { -- | The list of data source groups you want to filter search results based
    -- on groups\' access to documents in that data source.
    dataSourceGroups :: Prelude.Maybe (Prelude.NonEmpty DataSourceGroup),
    -- | The identifier of the user you want to filter search results based on
    -- their access to documents.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The list of groups you want to filter search results based on the
    -- groups\' access to documents.
    groups :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The user context token for filtering search results for a user. It must
    -- be a JWT or a JSON token.
    token :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSourceGroups', 'userContext_dataSourceGroups' - The list of data source groups you want to filter search results based
-- on groups\' access to documents in that data source.
--
-- 'userId', 'userContext_userId' - The identifier of the user you want to filter search results based on
-- their access to documents.
--
-- 'groups', 'userContext_groups' - The list of groups you want to filter search results based on the
-- groups\' access to documents.
--
-- 'token', 'userContext_token' - The user context token for filtering search results for a user. It must
-- be a JWT or a JSON token.
newUserContext ::
  UserContext
newUserContext =
  UserContext'
    { dataSourceGroups = Prelude.Nothing,
      userId = Prelude.Nothing,
      groups = Prelude.Nothing,
      token = Prelude.Nothing
    }

-- | The list of data source groups you want to filter search results based
-- on groups\' access to documents in that data source.
userContext_dataSourceGroups :: Lens.Lens' UserContext (Prelude.Maybe (Prelude.NonEmpty DataSourceGroup))
userContext_dataSourceGroups = Lens.lens (\UserContext' {dataSourceGroups} -> dataSourceGroups) (\s@UserContext' {} a -> s {dataSourceGroups = a} :: UserContext) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the user you want to filter search results based on
-- their access to documents.
userContext_userId :: Lens.Lens' UserContext (Prelude.Maybe Prelude.Text)
userContext_userId = Lens.lens (\UserContext' {userId} -> userId) (\s@UserContext' {} a -> s {userId = a} :: UserContext)

-- | The list of groups you want to filter search results based on the
-- groups\' access to documents.
userContext_groups :: Lens.Lens' UserContext (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
userContext_groups = Lens.lens (\UserContext' {groups} -> groups) (\s@UserContext' {} a -> s {groups = a} :: UserContext) Prelude.. Lens.mapping Lens.coerced

-- | The user context token for filtering search results for a user. It must
-- be a JWT or a JSON token.
userContext_token :: Lens.Lens' UserContext (Prelude.Maybe Prelude.Text)
userContext_token = Lens.lens (\UserContext' {token} -> token) (\s@UserContext' {} a -> s {token = a} :: UserContext)

instance Prelude.Hashable UserContext where
  hashWithSalt _salt UserContext' {..} =
    _salt `Prelude.hashWithSalt` dataSourceGroups
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` groups
      `Prelude.hashWithSalt` token

instance Prelude.NFData UserContext where
  rnf UserContext' {..} =
    Prelude.rnf dataSourceGroups
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf groups
      `Prelude.seq` Prelude.rnf token

instance Core.ToJSON UserContext where
  toJSON UserContext' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DataSourceGroups" Core..=)
              Prelude.<$> dataSourceGroups,
            ("UserId" Core..=) Prelude.<$> userId,
            ("Groups" Core..=) Prelude.<$> groups,
            ("Token" Core..=) Prelude.<$> token
          ]
      )
