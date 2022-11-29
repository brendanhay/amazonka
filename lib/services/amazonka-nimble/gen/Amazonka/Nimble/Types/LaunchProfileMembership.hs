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
-- Module      : Amazonka.Nimble.Types.LaunchProfileMembership
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.LaunchProfileMembership where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Nimble.Types.LaunchProfilePersona
import qualified Amazonka.Prelude as Prelude

-- | Launch profile membership enables your studio admins to delegate launch
-- profile access to other studio users in the Nimble Studio portal without
-- needing to write or maintain complex IAM policies. A launch profile
-- member is a user association from your studio identity source who is
-- granted permissions to a launch profile.
--
-- A launch profile member (type USER) provides the following permissions
-- to that launch profile:
--
-- -   GetLaunchProfile
--
-- -   GetLaunchProfileInitialization
--
-- -   GetLaunchProfileMembers
--
-- -   GetLaunchProfileMember
--
-- -   CreateStreamingSession
--
-- -   GetLaunchProfileDetails
--
-- /See:/ 'newLaunchProfileMembership' smart constructor.
data LaunchProfileMembership = LaunchProfileMembership'
  { -- | The principal ID.
    principalId :: Prelude.Maybe Prelude.Text,
    -- | The persona.
    persona :: Prelude.Maybe LaunchProfilePersona,
    -- | The Active Directory Security Identifier for this user, if available.
    sid :: Prelude.Maybe Prelude.Text,
    -- | The ID of the identity store.
    identityStoreId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchProfileMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principalId', 'launchProfileMembership_principalId' - The principal ID.
--
-- 'persona', 'launchProfileMembership_persona' - The persona.
--
-- 'sid', 'launchProfileMembership_sid' - The Active Directory Security Identifier for this user, if available.
--
-- 'identityStoreId', 'launchProfileMembership_identityStoreId' - The ID of the identity store.
newLaunchProfileMembership ::
  LaunchProfileMembership
newLaunchProfileMembership =
  LaunchProfileMembership'
    { principalId =
        Prelude.Nothing,
      persona = Prelude.Nothing,
      sid = Prelude.Nothing,
      identityStoreId = Prelude.Nothing
    }

-- | The principal ID.
launchProfileMembership_principalId :: Lens.Lens' LaunchProfileMembership (Prelude.Maybe Prelude.Text)
launchProfileMembership_principalId = Lens.lens (\LaunchProfileMembership' {principalId} -> principalId) (\s@LaunchProfileMembership' {} a -> s {principalId = a} :: LaunchProfileMembership)

-- | The persona.
launchProfileMembership_persona :: Lens.Lens' LaunchProfileMembership (Prelude.Maybe LaunchProfilePersona)
launchProfileMembership_persona = Lens.lens (\LaunchProfileMembership' {persona} -> persona) (\s@LaunchProfileMembership' {} a -> s {persona = a} :: LaunchProfileMembership)

-- | The Active Directory Security Identifier for this user, if available.
launchProfileMembership_sid :: Lens.Lens' LaunchProfileMembership (Prelude.Maybe Prelude.Text)
launchProfileMembership_sid = Lens.lens (\LaunchProfileMembership' {sid} -> sid) (\s@LaunchProfileMembership' {} a -> s {sid = a} :: LaunchProfileMembership)

-- | The ID of the identity store.
launchProfileMembership_identityStoreId :: Lens.Lens' LaunchProfileMembership (Prelude.Maybe Prelude.Text)
launchProfileMembership_identityStoreId = Lens.lens (\LaunchProfileMembership' {identityStoreId} -> identityStoreId) (\s@LaunchProfileMembership' {} a -> s {identityStoreId = a} :: LaunchProfileMembership)

instance Core.FromJSON LaunchProfileMembership where
  parseJSON =
    Core.withObject
      "LaunchProfileMembership"
      ( \x ->
          LaunchProfileMembership'
            Prelude.<$> (x Core..:? "principalId")
            Prelude.<*> (x Core..:? "persona")
            Prelude.<*> (x Core..:? "sid")
            Prelude.<*> (x Core..:? "identityStoreId")
      )

instance Prelude.Hashable LaunchProfileMembership where
  hashWithSalt _salt LaunchProfileMembership' {..} =
    _salt `Prelude.hashWithSalt` principalId
      `Prelude.hashWithSalt` persona
      `Prelude.hashWithSalt` sid
      `Prelude.hashWithSalt` identityStoreId

instance Prelude.NFData LaunchProfileMembership where
  rnf LaunchProfileMembership' {..} =
    Prelude.rnf principalId
      `Prelude.seq` Prelude.rnf persona
      `Prelude.seq` Prelude.rnf sid
      `Prelude.seq` Prelude.rnf identityStoreId
