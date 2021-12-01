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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StudioMembership where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Nimble.Types.StudioPersona
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newStudioMembership' smart constructor.
data StudioMembership = StudioMembership'
  { -- | The ID of the identity store.
    identityStoreId :: Prelude.Maybe Prelude.Text,
    -- | The principal ID.
    principalId :: Prelude.Maybe Prelude.Text,
    -- | The persona.
    persona :: Prelude.Maybe StudioPersona
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
-- 'principalId', 'studioMembership_principalId' - The principal ID.
--
-- 'persona', 'studioMembership_persona' - The persona.
newStudioMembership ::
  StudioMembership
newStudioMembership =
  StudioMembership'
    { identityStoreId =
        Prelude.Nothing,
      principalId = Prelude.Nothing,
      persona = Prelude.Nothing
    }

-- | The ID of the identity store.
studioMembership_identityStoreId :: Lens.Lens' StudioMembership (Prelude.Maybe Prelude.Text)
studioMembership_identityStoreId = Lens.lens (\StudioMembership' {identityStoreId} -> identityStoreId) (\s@StudioMembership' {} a -> s {identityStoreId = a} :: StudioMembership)

-- | The principal ID.
studioMembership_principalId :: Lens.Lens' StudioMembership (Prelude.Maybe Prelude.Text)
studioMembership_principalId = Lens.lens (\StudioMembership' {principalId} -> principalId) (\s@StudioMembership' {} a -> s {principalId = a} :: StudioMembership)

-- | The persona.
studioMembership_persona :: Lens.Lens' StudioMembership (Prelude.Maybe StudioPersona)
studioMembership_persona = Lens.lens (\StudioMembership' {persona} -> persona) (\s@StudioMembership' {} a -> s {persona = a} :: StudioMembership)

instance Core.FromJSON StudioMembership where
  parseJSON =
    Core.withObject
      "StudioMembership"
      ( \x ->
          StudioMembership'
            Prelude.<$> (x Core..:? "identityStoreId")
            Prelude.<*> (x Core..:? "principalId")
            Prelude.<*> (x Core..:? "persona")
      )

instance Prelude.Hashable StudioMembership where
  hashWithSalt salt' StudioMembership' {..} =
    salt' `Prelude.hashWithSalt` persona
      `Prelude.hashWithSalt` principalId
      `Prelude.hashWithSalt` identityStoreId

instance Prelude.NFData StudioMembership where
  rnf StudioMembership' {..} =
    Prelude.rnf identityStoreId
      `Prelude.seq` Prelude.rnf persona
      `Prelude.seq` Prelude.rnf principalId
