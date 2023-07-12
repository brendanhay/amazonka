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
-- Module      : Amazonka.Connect.Types.ParticipantDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.ParticipantDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The customer\'s details.
--
-- /See:/ 'newParticipantDetails' smart constructor.
data ParticipantDetails = ParticipantDetails'
  { -- | Display name of the participant.
    displayName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParticipantDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayName', 'participantDetails_displayName' - Display name of the participant.
newParticipantDetails ::
  -- | 'displayName'
  Prelude.Text ->
  ParticipantDetails
newParticipantDetails pDisplayName_ =
  ParticipantDetails' {displayName = pDisplayName_}

-- | Display name of the participant.
participantDetails_displayName :: Lens.Lens' ParticipantDetails Prelude.Text
participantDetails_displayName = Lens.lens (\ParticipantDetails' {displayName} -> displayName) (\s@ParticipantDetails' {} a -> s {displayName = a} :: ParticipantDetails)

instance Prelude.Hashable ParticipantDetails where
  hashWithSalt _salt ParticipantDetails' {..} =
    _salt `Prelude.hashWithSalt` displayName

instance Prelude.NFData ParticipantDetails where
  rnf ParticipantDetails' {..} = Prelude.rnf displayName

instance Data.ToJSON ParticipantDetails where
  toJSON ParticipantDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("DisplayName" Data..= displayName)]
      )
