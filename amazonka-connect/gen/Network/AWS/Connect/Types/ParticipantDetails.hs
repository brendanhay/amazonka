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
-- Module      : Network.AWS.Connect.Types.ParticipantDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.ParticipantDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The customer\'s details.
--
-- /See:/ 'newParticipantDetails' smart constructor.
data ParticipantDetails = ParticipantDetails'
  { -- | Display name of the participant.
    displayName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  ParticipantDetails
newParticipantDetails pDisplayName_ =
  ParticipantDetails' {displayName = pDisplayName_}

-- | Display name of the participant.
participantDetails_displayName :: Lens.Lens' ParticipantDetails Core.Text
participantDetails_displayName = Lens.lens (\ParticipantDetails' {displayName} -> displayName) (\s@ParticipantDetails' {} a -> s {displayName = a} :: ParticipantDetails)

instance Core.Hashable ParticipantDetails

instance Core.NFData ParticipantDetails

instance Core.ToJSON ParticipantDetails where
  toJSON ParticipantDetails' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("DisplayName" Core..= displayName)]
      )
