-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.UserPendingChanges
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.UserPendingChanges
  ( UserPendingChanges (..),

    -- * Smart constructor
    mkUserPendingChanges,

    -- * Lenses
    upcGroups,
    upcConsoleAccess,
    upcPendingChange,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types.ChangeType
import qualified Network.AWS.Prelude as Lude

-- | Returns information about the status of the changes pending for the ActiveMQ user.
--
-- /See:/ 'mkUserPendingChanges' smart constructor.
data UserPendingChanges = UserPendingChanges'
  { groups ::
      Lude.Maybe [Lude.Text],
    consoleAccess :: Lude.Maybe Lude.Bool,
    pendingChange :: Lude.Maybe ChangeType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserPendingChanges' with the minimum fields required to make a request.
--
-- * 'consoleAccess' - Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
-- * 'groups' - The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
-- * 'pendingChange' - Required. The type of change pending for the ActiveMQ user.
mkUserPendingChanges ::
  UserPendingChanges
mkUserPendingChanges =
  UserPendingChanges'
    { groups = Lude.Nothing,
      consoleAccess = Lude.Nothing,
      pendingChange = Lude.Nothing
    }

-- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcGroups :: Lens.Lens' UserPendingChanges (Lude.Maybe [Lude.Text])
upcGroups = Lens.lens (groups :: UserPendingChanges -> Lude.Maybe [Lude.Text]) (\s a -> s {groups = a} :: UserPendingChanges)
{-# DEPRECATED upcGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
--
-- /Note:/ Consider using 'consoleAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcConsoleAccess :: Lens.Lens' UserPendingChanges (Lude.Maybe Lude.Bool)
upcConsoleAccess = Lens.lens (consoleAccess :: UserPendingChanges -> Lude.Maybe Lude.Bool) (\s a -> s {consoleAccess = a} :: UserPendingChanges)
{-# DEPRECATED upcConsoleAccess "Use generic-lens or generic-optics with 'consoleAccess' instead." #-}

-- | Required. The type of change pending for the ActiveMQ user.
--
-- /Note:/ Consider using 'pendingChange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcPendingChange :: Lens.Lens' UserPendingChanges (Lude.Maybe ChangeType)
upcPendingChange = Lens.lens (pendingChange :: UserPendingChanges -> Lude.Maybe ChangeType) (\s a -> s {pendingChange = a} :: UserPendingChanges)
{-# DEPRECATED upcPendingChange "Use generic-lens or generic-optics with 'pendingChange' instead." #-}

instance Lude.FromJSON UserPendingChanges where
  parseJSON =
    Lude.withObject
      "UserPendingChanges"
      ( \x ->
          UserPendingChanges'
            Lude.<$> (x Lude..:? "groups" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "consoleAccess")
            Lude.<*> (x Lude..:? "pendingChange")
      )
