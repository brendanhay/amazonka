{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.ParticipantDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.ParticipantDetails
  ( ParticipantDetails (..),

    -- * Smart constructor
    mkParticipantDetails,

    -- * Lenses
    pdDisplayName,
  )
where

import qualified Network.AWS.Connect.Types.DisplayName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The customer's details.
--
-- /See:/ 'mkParticipantDetails' smart constructor.
newtype ParticipantDetails = ParticipantDetails'
  { -- | Display name of the participant.
    displayName :: Types.DisplayName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ParticipantDetails' value with any optional fields omitted.
mkParticipantDetails ::
  -- | 'displayName'
  Types.DisplayName ->
  ParticipantDetails
mkParticipantDetails displayName = ParticipantDetails' {displayName}

-- | Display name of the participant.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdDisplayName :: Lens.Lens' ParticipantDetails Types.DisplayName
pdDisplayName = Lens.field @"displayName"
{-# DEPRECATED pdDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

instance Core.FromJSON ParticipantDetails where
  toJSON ParticipantDetails {..} =
    Core.object
      (Core.catMaybes [Core.Just ("DisplayName" Core..= displayName)])
