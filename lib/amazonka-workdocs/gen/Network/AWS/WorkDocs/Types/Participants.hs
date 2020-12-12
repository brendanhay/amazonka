{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.Participants
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.Participants
  ( Participants (..),

    -- * Smart constructor
    mkParticipants,

    -- * Lenses
    pGroups,
    pUsers,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkDocs.Types.GroupMetadata
import Network.AWS.WorkDocs.Types.UserMetadata

-- | Describes the users or user groups.
--
-- /See:/ 'mkParticipants' smart constructor.
data Participants = Participants'
  { groups ::
      Lude.Maybe [GroupMetadata],
    users :: Lude.Maybe [UserMetadata]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Participants' with the minimum fields required to make a request.
--
-- * 'groups' - The list of user groups.
-- * 'users' - The list of users.
mkParticipants ::
  Participants
mkParticipants =
  Participants' {groups = Lude.Nothing, users = Lude.Nothing}

-- | The list of user groups.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pGroups :: Lens.Lens' Participants (Lude.Maybe [GroupMetadata])
pGroups = Lens.lens (groups :: Participants -> Lude.Maybe [GroupMetadata]) (\s a -> s {groups = a} :: Participants)
{-# DEPRECATED pGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | The list of users.
--
-- /Note:/ Consider using 'users' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pUsers :: Lens.Lens' Participants (Lude.Maybe [UserMetadata])
pUsers = Lens.lens (users :: Participants -> Lude.Maybe [UserMetadata]) (\s a -> s {users = a} :: Participants)
{-# DEPRECATED pUsers "Use generic-lens or generic-optics with 'users' instead." #-}

instance Lude.FromJSON Participants where
  parseJSON =
    Lude.withObject
      "Participants"
      ( \x ->
          Participants'
            Lude.<$> (x Lude..:? "Groups" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Users" Lude..!= Lude.mempty)
      )
