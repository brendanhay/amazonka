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
-- Module      : Network.AWS.Kendra.Types.UserContext
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.UserContext where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.DataSourceGroup
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about the user context for an Amazon Kendra index.
--
-- This is used for filtering search results for different users based on
-- their access to documents.
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
  { -- | The list of groups you want to filter search results based on the
    -- groups\' access to documents.
    groups :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The user context token for filtering search results for a user. It must
    -- be a JWT or a JSON token.
    token :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the user you want to filter search results based on
    -- their access to documents.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The list of data source groups you want to filter search results based
    -- on groups\' access to documents in that data source.
    dataSourceGroups :: Prelude.Maybe (Prelude.NonEmpty DataSourceGroup)
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
-- 'groups', 'userContext_groups' - The list of groups you want to filter search results based on the
-- groups\' access to documents.
--
-- 'token', 'userContext_token' - The user context token for filtering search results for a user. It must
-- be a JWT or a JSON token.
--
-- 'userId', 'userContext_userId' - The identifier of the user you want to filter search results based on
-- their access to documents.
--
-- 'dataSourceGroups', 'userContext_dataSourceGroups' - The list of data source groups you want to filter search results based
-- on groups\' access to documents in that data source.
newUserContext ::
  UserContext
newUserContext =
  UserContext'
    { groups = Prelude.Nothing,
      token = Prelude.Nothing,
      userId = Prelude.Nothing,
      dataSourceGroups = Prelude.Nothing
    }

-- | The list of groups you want to filter search results based on the
-- groups\' access to documents.
userContext_groups :: Lens.Lens' UserContext (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
userContext_groups = Lens.lens (\UserContext' {groups} -> groups) (\s@UserContext' {} a -> s {groups = a} :: UserContext) Prelude.. Lens.mapping Lens.coerced

-- | The user context token for filtering search results for a user. It must
-- be a JWT or a JSON token.
userContext_token :: Lens.Lens' UserContext (Prelude.Maybe Prelude.Text)
userContext_token = Lens.lens (\UserContext' {token} -> token) (\s@UserContext' {} a -> s {token = a} :: UserContext)

-- | The identifier of the user you want to filter search results based on
-- their access to documents.
userContext_userId :: Lens.Lens' UserContext (Prelude.Maybe Prelude.Text)
userContext_userId = Lens.lens (\UserContext' {userId} -> userId) (\s@UserContext' {} a -> s {userId = a} :: UserContext)

-- | The list of data source groups you want to filter search results based
-- on groups\' access to documents in that data source.
userContext_dataSourceGroups :: Lens.Lens' UserContext (Prelude.Maybe (Prelude.NonEmpty DataSourceGroup))
userContext_dataSourceGroups = Lens.lens (\UserContext' {dataSourceGroups} -> dataSourceGroups) (\s@UserContext' {} a -> s {dataSourceGroups = a} :: UserContext) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable UserContext

instance Prelude.NFData UserContext

instance Core.ToJSON UserContext where
  toJSON UserContext' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Groups" Core..=) Prelude.<$> groups,
            ("Token" Core..=) Prelude.<$> token,
            ("UserId" Core..=) Prelude.<$> userId,
            ("DataSourceGroups" Core..=)
              Prelude.<$> dataSourceGroups
          ]
      )
