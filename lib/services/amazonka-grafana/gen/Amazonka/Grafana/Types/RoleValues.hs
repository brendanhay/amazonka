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
-- Module      : Amazonka.Grafana.Types.RoleValues
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Grafana.Types.RoleValues where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This structure defines which groups defined in the SAML assertion
-- attribute are to be mapped to the Grafana @Admin@ and @Editor@ roles in
-- the workspace. SAML authenticated users not part of @Admin@ or @Editor@
-- role groups have @Viewer@ permission over the workspace.
--
-- /See:/ 'newRoleValues' smart constructor.
data RoleValues = RoleValues'
  { -- | A list of groups from the SAML assertion attribute to grant the Grafana
    -- @Admin@ role to.
    admin :: Prelude.Maybe [Prelude.Text],
    -- | A list of groups from the SAML assertion attribute to grant the Grafana
    -- @Editor@ role to.
    editor :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RoleValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'admin', 'roleValues_admin' - A list of groups from the SAML assertion attribute to grant the Grafana
-- @Admin@ role to.
--
-- 'editor', 'roleValues_editor' - A list of groups from the SAML assertion attribute to grant the Grafana
-- @Editor@ role to.
newRoleValues ::
  RoleValues
newRoleValues =
  RoleValues'
    { admin = Prelude.Nothing,
      editor = Prelude.Nothing
    }

-- | A list of groups from the SAML assertion attribute to grant the Grafana
-- @Admin@ role to.
roleValues_admin :: Lens.Lens' RoleValues (Prelude.Maybe [Prelude.Text])
roleValues_admin = Lens.lens (\RoleValues' {admin} -> admin) (\s@RoleValues' {} a -> s {admin = a} :: RoleValues) Prelude.. Lens.mapping Lens.coerced

-- | A list of groups from the SAML assertion attribute to grant the Grafana
-- @Editor@ role to.
roleValues_editor :: Lens.Lens' RoleValues (Prelude.Maybe [Prelude.Text])
roleValues_editor = Lens.lens (\RoleValues' {editor} -> editor) (\s@RoleValues' {} a -> s {editor = a} :: RoleValues) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON RoleValues where
  parseJSON =
    Data.withObject
      "RoleValues"
      ( \x ->
          RoleValues'
            Prelude.<$> (x Data..:? "admin" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "editor" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable RoleValues where
  hashWithSalt _salt RoleValues' {..} =
    _salt
      `Prelude.hashWithSalt` admin
      `Prelude.hashWithSalt` editor

instance Prelude.NFData RoleValues where
  rnf RoleValues' {..} =
    Prelude.rnf admin `Prelude.seq` Prelude.rnf editor

instance Data.ToJSON RoleValues where
  toJSON RoleValues' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("admin" Data..=) Prelude.<$> admin,
            ("editor" Data..=) Prelude.<$> editor
          ]
      )
