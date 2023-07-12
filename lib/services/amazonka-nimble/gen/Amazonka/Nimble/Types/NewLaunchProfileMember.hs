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
-- Module      : Amazonka.Nimble.Types.NewLaunchProfileMember
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.NewLaunchProfileMember where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types.LaunchProfilePersona
import qualified Amazonka.Prelude as Prelude

-- | A new member that is added to a launch profile.
--
-- /See:/ 'newNewLaunchProfileMember' smart constructor.
data NewLaunchProfileMember = NewLaunchProfileMember'
  { -- | The persona.
    persona :: LaunchProfilePersona,
    -- | The principal ID.
    principalId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NewLaunchProfileMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'persona', 'newLaunchProfileMember_persona' - The persona.
--
-- 'principalId', 'newLaunchProfileMember_principalId' - The principal ID.
newNewLaunchProfileMember ::
  -- | 'persona'
  LaunchProfilePersona ->
  -- | 'principalId'
  Prelude.Text ->
  NewLaunchProfileMember
newNewLaunchProfileMember pPersona_ pPrincipalId_ =
  NewLaunchProfileMember'
    { persona = pPersona_,
      principalId = pPrincipalId_
    }

-- | The persona.
newLaunchProfileMember_persona :: Lens.Lens' NewLaunchProfileMember LaunchProfilePersona
newLaunchProfileMember_persona = Lens.lens (\NewLaunchProfileMember' {persona} -> persona) (\s@NewLaunchProfileMember' {} a -> s {persona = a} :: NewLaunchProfileMember)

-- | The principal ID.
newLaunchProfileMember_principalId :: Lens.Lens' NewLaunchProfileMember Prelude.Text
newLaunchProfileMember_principalId = Lens.lens (\NewLaunchProfileMember' {principalId} -> principalId) (\s@NewLaunchProfileMember' {} a -> s {principalId = a} :: NewLaunchProfileMember)

instance Prelude.Hashable NewLaunchProfileMember where
  hashWithSalt _salt NewLaunchProfileMember' {..} =
    _salt
      `Prelude.hashWithSalt` persona
      `Prelude.hashWithSalt` principalId

instance Prelude.NFData NewLaunchProfileMember where
  rnf NewLaunchProfileMember' {..} =
    Prelude.rnf persona
      `Prelude.seq` Prelude.rnf principalId

instance Data.ToJSON NewLaunchProfileMember where
  toJSON NewLaunchProfileMember' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("persona" Data..= persona),
            Prelude.Just ("principalId" Data..= principalId)
          ]
      )
