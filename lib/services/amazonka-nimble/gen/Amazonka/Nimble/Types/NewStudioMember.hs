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
-- Module      : Amazonka.Nimble.Types.NewStudioMember
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.NewStudioMember where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types.StudioPersona
import qualified Amazonka.Prelude as Prelude

-- | A new studio user\'s membership.
--
-- /See:/ 'newNewStudioMember' smart constructor.
data NewStudioMember = NewStudioMember'
  { -- | The persona.
    persona :: StudioPersona,
    -- | The principal ID.
    principalId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NewStudioMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'persona', 'newStudioMember_persona' - The persona.
--
-- 'principalId', 'newStudioMember_principalId' - The principal ID.
newNewStudioMember ::
  -- | 'persona'
  StudioPersona ->
  -- | 'principalId'
  Prelude.Text ->
  NewStudioMember
newNewStudioMember pPersona_ pPrincipalId_ =
  NewStudioMember'
    { persona = pPersona_,
      principalId = pPrincipalId_
    }

-- | The persona.
newStudioMember_persona :: Lens.Lens' NewStudioMember StudioPersona
newStudioMember_persona = Lens.lens (\NewStudioMember' {persona} -> persona) (\s@NewStudioMember' {} a -> s {persona = a} :: NewStudioMember)

-- | The principal ID.
newStudioMember_principalId :: Lens.Lens' NewStudioMember Prelude.Text
newStudioMember_principalId = Lens.lens (\NewStudioMember' {principalId} -> principalId) (\s@NewStudioMember' {} a -> s {principalId = a} :: NewStudioMember)

instance Prelude.Hashable NewStudioMember where
  hashWithSalt _salt NewStudioMember' {..} =
    _salt
      `Prelude.hashWithSalt` persona
      `Prelude.hashWithSalt` principalId

instance Prelude.NFData NewStudioMember where
  rnf NewStudioMember' {..} =
    Prelude.rnf persona `Prelude.seq`
      Prelude.rnf principalId

instance Data.ToJSON NewStudioMember where
  toJSON NewStudioMember' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("persona" Data..= persona),
            Prelude.Just ("principalId" Data..= principalId)
          ]
      )
