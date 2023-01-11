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
-- Module      : Amazonka.Nimble.Types.StudioMembership
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StudioMembership where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types.StudioPersona
import qualified Amazonka.Prelude as Prelude

-- | A studio member is an association of a user from your studio identity
-- source to elevated permissions that they are granted in the studio.
--
-- When you add a user to your studio using the Nimble Studio console, they
-- are given access to the studio\'s IAM Identity Center application and
-- are given access to log in to the Nimble Studio portal. These users have
-- the permissions provided by the studio\'s user IAM role and do not
-- appear in the studio membership collection. Only studio admins appear in
-- studio membership.
--
-- When you add a user to studio membership with the ADMIN persona, upon
-- logging in to the Nimble Studio portal, they are granted permissions
-- specified by the Studio\'s Admin IAM role.
--
-- /See:/ 'newStudioMembership' smart constructor.
data StudioMembership = StudioMembership'
  { -- | The ID of the identity store.
    identityStoreId :: Prelude.Maybe Prelude.Text,
    -- | The persona.
    persona :: Prelude.Maybe StudioPersona,
    -- | The principal ID.
    principalId :: Prelude.Maybe Prelude.Text,
    -- | The Active Directory Security Identifier for this user, if available.
    sid :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StudioMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityStoreId', 'studioMembership_identityStoreId' - The ID of the identity store.
--
-- 'persona', 'studioMembership_persona' - The persona.
--
-- 'principalId', 'studioMembership_principalId' - The principal ID.
--
-- 'sid', 'studioMembership_sid' - The Active Directory Security Identifier for this user, if available.
newStudioMembership ::
  StudioMembership
newStudioMembership =
  StudioMembership'
    { identityStoreId =
        Prelude.Nothing,
      persona = Prelude.Nothing,
      principalId = Prelude.Nothing,
      sid = Prelude.Nothing
    }

-- | The ID of the identity store.
studioMembership_identityStoreId :: Lens.Lens' StudioMembership (Prelude.Maybe Prelude.Text)
studioMembership_identityStoreId = Lens.lens (\StudioMembership' {identityStoreId} -> identityStoreId) (\s@StudioMembership' {} a -> s {identityStoreId = a} :: StudioMembership)

-- | The persona.
studioMembership_persona :: Lens.Lens' StudioMembership (Prelude.Maybe StudioPersona)
studioMembership_persona = Lens.lens (\StudioMembership' {persona} -> persona) (\s@StudioMembership' {} a -> s {persona = a} :: StudioMembership)

-- | The principal ID.
studioMembership_principalId :: Lens.Lens' StudioMembership (Prelude.Maybe Prelude.Text)
studioMembership_principalId = Lens.lens (\StudioMembership' {principalId} -> principalId) (\s@StudioMembership' {} a -> s {principalId = a} :: StudioMembership)

-- | The Active Directory Security Identifier for this user, if available.
studioMembership_sid :: Lens.Lens' StudioMembership (Prelude.Maybe Prelude.Text)
studioMembership_sid = Lens.lens (\StudioMembership' {sid} -> sid) (\s@StudioMembership' {} a -> s {sid = a} :: StudioMembership)

instance Data.FromJSON StudioMembership where
  parseJSON =
    Data.withObject
      "StudioMembership"
      ( \x ->
          StudioMembership'
            Prelude.<$> (x Data..:? "identityStoreId")
            Prelude.<*> (x Data..:? "persona")
            Prelude.<*> (x Data..:? "principalId")
            Prelude.<*> (x Data..:? "sid")
      )

instance Prelude.Hashable StudioMembership where
  hashWithSalt _salt StudioMembership' {..} =
    _salt `Prelude.hashWithSalt` identityStoreId
      `Prelude.hashWithSalt` persona
      `Prelude.hashWithSalt` principalId
      `Prelude.hashWithSalt` sid

instance Prelude.NFData StudioMembership where
  rnf StudioMembership' {..} =
    Prelude.rnf identityStoreId
      `Prelude.seq` Prelude.rnf persona
      `Prelude.seq` Prelude.rnf principalId
      `Prelude.seq` Prelude.rnf sid
