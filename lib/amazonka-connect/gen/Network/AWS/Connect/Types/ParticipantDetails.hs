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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The customer's details.
--
-- /See:/ 'mkParticipantDetails' smart constructor.
newtype ParticipantDetails = ParticipantDetails'
  { -- | Display name of the participant.
    displayName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ParticipantDetails' with the minimum fields required to make a request.
--
-- * 'displayName' - Display name of the participant.
mkParticipantDetails ::
  -- | 'displayName'
  Lude.Text ->
  ParticipantDetails
mkParticipantDetails pDisplayName_ =
  ParticipantDetails' {displayName = pDisplayName_}

-- | Display name of the participant.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdDisplayName :: Lens.Lens' ParticipantDetails Lude.Text
pdDisplayName = Lens.lens (displayName :: ParticipantDetails -> Lude.Text) (\s a -> s {displayName = a} :: ParticipantDetails)
{-# DEPRECATED pdDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

instance Lude.ToJSON ParticipantDetails where
  toJSON ParticipantDetails' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("DisplayName" Lude..= displayName)])
