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
-- Module      : Amazonka.Connect.Types.ParticipantDetailsToAdd
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.ParticipantDetailsToAdd where

import Amazonka.Connect.Types.ParticipantRole
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details to add for the participant.
--
-- /See:/ 'newParticipantDetailsToAdd' smart constructor.
data ParticipantDetailsToAdd = ParticipantDetailsToAdd'
  { -- | The display name of the participant.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The role of the participant being added.
    participantRole :: Prelude.Maybe ParticipantRole
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParticipantDetailsToAdd' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayName', 'participantDetailsToAdd_displayName' - The display name of the participant.
--
-- 'participantRole', 'participantDetailsToAdd_participantRole' - The role of the participant being added.
newParticipantDetailsToAdd ::
  ParticipantDetailsToAdd
newParticipantDetailsToAdd =
  ParticipantDetailsToAdd'
    { displayName =
        Prelude.Nothing,
      participantRole = Prelude.Nothing
    }

-- | The display name of the participant.
participantDetailsToAdd_displayName :: Lens.Lens' ParticipantDetailsToAdd (Prelude.Maybe Prelude.Text)
participantDetailsToAdd_displayName = Lens.lens (\ParticipantDetailsToAdd' {displayName} -> displayName) (\s@ParticipantDetailsToAdd' {} a -> s {displayName = a} :: ParticipantDetailsToAdd)

-- | The role of the participant being added.
participantDetailsToAdd_participantRole :: Lens.Lens' ParticipantDetailsToAdd (Prelude.Maybe ParticipantRole)
participantDetailsToAdd_participantRole = Lens.lens (\ParticipantDetailsToAdd' {participantRole} -> participantRole) (\s@ParticipantDetailsToAdd' {} a -> s {participantRole = a} :: ParticipantDetailsToAdd)

instance Prelude.Hashable ParticipantDetailsToAdd where
  hashWithSalt _salt ParticipantDetailsToAdd' {..} =
    _salt
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` participantRole

instance Prelude.NFData ParticipantDetailsToAdd where
  rnf ParticipantDetailsToAdd' {..} =
    Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf participantRole

instance Data.ToJSON ParticipantDetailsToAdd where
  toJSON ParticipantDetailsToAdd' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DisplayName" Data..=) Prelude.<$> displayName,
            ("ParticipantRole" Data..=)
              Prelude.<$> participantRole
          ]
      )
