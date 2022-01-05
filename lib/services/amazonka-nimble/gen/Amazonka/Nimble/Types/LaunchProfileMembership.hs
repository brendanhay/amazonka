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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.LaunchProfileMembership where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Nimble.Types.LaunchProfilePersona
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newLaunchProfileMembership' smart constructor.
data LaunchProfileMembership = LaunchProfileMembership'
  { -- | The ID of the identity store.
    identityStoreId :: Prelude.Maybe Prelude.Text,
    -- | The principal ID.
    principalId :: Prelude.Maybe Prelude.Text,
    -- | The persona.
    persona :: Prelude.Maybe LaunchProfilePersona
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
-- 'identityStoreId', 'launchProfileMembership_identityStoreId' - The ID of the identity store.
--
-- 'principalId', 'launchProfileMembership_principalId' - The principal ID.
--
-- 'persona', 'launchProfileMembership_persona' - The persona.
newLaunchProfileMembership ::
  LaunchProfileMembership
newLaunchProfileMembership =
  LaunchProfileMembership'
    { identityStoreId =
        Prelude.Nothing,
      principalId = Prelude.Nothing,
      persona = Prelude.Nothing
    }

-- | The ID of the identity store.
launchProfileMembership_identityStoreId :: Lens.Lens' LaunchProfileMembership (Prelude.Maybe Prelude.Text)
launchProfileMembership_identityStoreId = Lens.lens (\LaunchProfileMembership' {identityStoreId} -> identityStoreId) (\s@LaunchProfileMembership' {} a -> s {identityStoreId = a} :: LaunchProfileMembership)

-- | The principal ID.
launchProfileMembership_principalId :: Lens.Lens' LaunchProfileMembership (Prelude.Maybe Prelude.Text)
launchProfileMembership_principalId = Lens.lens (\LaunchProfileMembership' {principalId} -> principalId) (\s@LaunchProfileMembership' {} a -> s {principalId = a} :: LaunchProfileMembership)

-- | The persona.
launchProfileMembership_persona :: Lens.Lens' LaunchProfileMembership (Prelude.Maybe LaunchProfilePersona)
launchProfileMembership_persona = Lens.lens (\LaunchProfileMembership' {persona} -> persona) (\s@LaunchProfileMembership' {} a -> s {persona = a} :: LaunchProfileMembership)

instance Core.FromJSON LaunchProfileMembership where
  parseJSON =
    Core.withObject
      "LaunchProfileMembership"
      ( \x ->
          LaunchProfileMembership'
            Prelude.<$> (x Core..:? "identityStoreId")
            Prelude.<*> (x Core..:? "principalId")
            Prelude.<*> (x Core..:? "persona")
      )

instance Prelude.Hashable LaunchProfileMembership where
  hashWithSalt _salt LaunchProfileMembership' {..} =
    _salt `Prelude.hashWithSalt` identityStoreId
      `Prelude.hashWithSalt` principalId
      `Prelude.hashWithSalt` persona

instance Prelude.NFData LaunchProfileMembership where
  rnf LaunchProfileMembership' {..} =
    Prelude.rnf identityStoreId
      `Prelude.seq` Prelude.rnf principalId
      `Prelude.seq` Prelude.rnf persona
