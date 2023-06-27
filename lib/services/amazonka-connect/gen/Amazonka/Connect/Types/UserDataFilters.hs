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
-- Module      : Amazonka.Connect.Types.UserDataFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.UserDataFilters where

import Amazonka.Connect.Types.ContactFilter
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A filter for the user data.
--
-- /See:/ 'newUserDataFilters' smart constructor.
data UserDataFilters = UserDataFilters'
  { -- | A list of up to 100 agent IDs or ARNs.
    agents :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A filter for the user data based on the contact information that is
    -- associated to the user. It contains a list of contact states.
    contactFilter :: Prelude.Maybe ContactFilter,
    -- | A list of up to 100 queues or ARNs.
    queues :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of up to 100 routing profile IDs or ARNs.
    routingProfiles :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A UserHierarchyGroup ID or ARN.
    userHierarchyGroups :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserDataFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agents', 'userDataFilters_agents' - A list of up to 100 agent IDs or ARNs.
--
-- 'contactFilter', 'userDataFilters_contactFilter' - A filter for the user data based on the contact information that is
-- associated to the user. It contains a list of contact states.
--
-- 'queues', 'userDataFilters_queues' - A list of up to 100 queues or ARNs.
--
-- 'routingProfiles', 'userDataFilters_routingProfiles' - A list of up to 100 routing profile IDs or ARNs.
--
-- 'userHierarchyGroups', 'userDataFilters_userHierarchyGroups' - A UserHierarchyGroup ID or ARN.
newUserDataFilters ::
  UserDataFilters
newUserDataFilters =
  UserDataFilters'
    { agents = Prelude.Nothing,
      contactFilter = Prelude.Nothing,
      queues = Prelude.Nothing,
      routingProfiles = Prelude.Nothing,
      userHierarchyGroups = Prelude.Nothing
    }

-- | A list of up to 100 agent IDs or ARNs.
userDataFilters_agents :: Lens.Lens' UserDataFilters (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
userDataFilters_agents = Lens.lens (\UserDataFilters' {agents} -> agents) (\s@UserDataFilters' {} a -> s {agents = a} :: UserDataFilters) Prelude.. Lens.mapping Lens.coerced

-- | A filter for the user data based on the contact information that is
-- associated to the user. It contains a list of contact states.
userDataFilters_contactFilter :: Lens.Lens' UserDataFilters (Prelude.Maybe ContactFilter)
userDataFilters_contactFilter = Lens.lens (\UserDataFilters' {contactFilter} -> contactFilter) (\s@UserDataFilters' {} a -> s {contactFilter = a} :: UserDataFilters)

-- | A list of up to 100 queues or ARNs.
userDataFilters_queues :: Lens.Lens' UserDataFilters (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
userDataFilters_queues = Lens.lens (\UserDataFilters' {queues} -> queues) (\s@UserDataFilters' {} a -> s {queues = a} :: UserDataFilters) Prelude.. Lens.mapping Lens.coerced

-- | A list of up to 100 routing profile IDs or ARNs.
userDataFilters_routingProfiles :: Lens.Lens' UserDataFilters (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
userDataFilters_routingProfiles = Lens.lens (\UserDataFilters' {routingProfiles} -> routingProfiles) (\s@UserDataFilters' {} a -> s {routingProfiles = a} :: UserDataFilters) Prelude.. Lens.mapping Lens.coerced

-- | A UserHierarchyGroup ID or ARN.
userDataFilters_userHierarchyGroups :: Lens.Lens' UserDataFilters (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
userDataFilters_userHierarchyGroups = Lens.lens (\UserDataFilters' {userHierarchyGroups} -> userHierarchyGroups) (\s@UserDataFilters' {} a -> s {userHierarchyGroups = a} :: UserDataFilters) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable UserDataFilters where
  hashWithSalt _salt UserDataFilters' {..} =
    _salt
      `Prelude.hashWithSalt` agents
      `Prelude.hashWithSalt` contactFilter
      `Prelude.hashWithSalt` queues
      `Prelude.hashWithSalt` routingProfiles
      `Prelude.hashWithSalt` userHierarchyGroups

instance Prelude.NFData UserDataFilters where
  rnf UserDataFilters' {..} =
    Prelude.rnf agents
      `Prelude.seq` Prelude.rnf contactFilter
      `Prelude.seq` Prelude.rnf queues
      `Prelude.seq` Prelude.rnf routingProfiles
      `Prelude.seq` Prelude.rnf userHierarchyGroups

instance Data.ToJSON UserDataFilters where
  toJSON UserDataFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Agents" Data..=) Prelude.<$> agents,
            ("ContactFilter" Data..=) Prelude.<$> contactFilter,
            ("Queues" Data..=) Prelude.<$> queues,
            ("RoutingProfiles" Data..=)
              Prelude.<$> routingProfiles,
            ("UserHierarchyGroups" Data..=)
              Prelude.<$> userHierarchyGroups
          ]
      )
