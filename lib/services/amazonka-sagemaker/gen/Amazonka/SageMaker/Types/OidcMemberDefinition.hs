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
-- Module      : Amazonka.SageMaker.Types.OidcMemberDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.OidcMemberDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of user groups that exist in your OIDC Identity Provider (IdP).
-- One to ten groups can be used to create a single private work team. When
-- you add a user group to the list of @Groups@, you can add that user
-- group to one or more private work teams. If you add a user group to a
-- private work team, all workers in that user group are added to the work
-- team.
--
-- /See:/ 'newOidcMemberDefinition' smart constructor.
data OidcMemberDefinition = OidcMemberDefinition'
  { -- | A list of comma seperated strings that identifies user groups in your
    -- OIDC IdP. Each user group is made up of a group of private workers.
    groups :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OidcMemberDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groups', 'oidcMemberDefinition_groups' - A list of comma seperated strings that identifies user groups in your
-- OIDC IdP. Each user group is made up of a group of private workers.
newOidcMemberDefinition ::
  -- | 'groups'
  Prelude.NonEmpty Prelude.Text ->
  OidcMemberDefinition
newOidcMemberDefinition pGroups_ =
  OidcMemberDefinition'
    { groups =
        Lens.coerced Lens.# pGroups_
    }

-- | A list of comma seperated strings that identifies user groups in your
-- OIDC IdP. Each user group is made up of a group of private workers.
oidcMemberDefinition_groups :: Lens.Lens' OidcMemberDefinition (Prelude.NonEmpty Prelude.Text)
oidcMemberDefinition_groups = Lens.lens (\OidcMemberDefinition' {groups} -> groups) (\s@OidcMemberDefinition' {} a -> s {groups = a} :: OidcMemberDefinition) Prelude.. Lens.coerced

instance Data.FromJSON OidcMemberDefinition where
  parseJSON =
    Data.withObject
      "OidcMemberDefinition"
      ( \x ->
          OidcMemberDefinition'
            Prelude.<$> (x Data..: "Groups")
      )

instance Prelude.Hashable OidcMemberDefinition where
  hashWithSalt _salt OidcMemberDefinition' {..} =
    _salt `Prelude.hashWithSalt` groups

instance Prelude.NFData OidcMemberDefinition where
  rnf OidcMemberDefinition' {..} = Prelude.rnf groups

instance Data.ToJSON OidcMemberDefinition where
  toJSON OidcMemberDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Groups" Data..= groups)]
      )
