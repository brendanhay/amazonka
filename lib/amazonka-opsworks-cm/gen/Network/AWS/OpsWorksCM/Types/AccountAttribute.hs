{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Types.AccountAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorksCM.Types.AccountAttribute
  ( AccountAttribute (..),

    -- * Smart constructor
    mkAccountAttribute,

    -- * Lenses
    aaUsed,
    aaMaximum,
    aaName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Stores account attributes.
--
-- /See:/ 'mkAccountAttribute' smart constructor.
data AccountAttribute = AccountAttribute'
  { -- | The current usage, such as the current number of servers that are associated with the account.
    used :: Lude.Maybe Lude.Int,
    -- | The maximum allowed value.
    maximum :: Lude.Maybe Lude.Int,
    -- | The attribute name. The following are supported attribute names.
    --
    --
    --     * /ServerLimit:/ The number of current servers/maximum number of servers allowed. By default, you can have a maximum of 10 servers.
    --
    --
    --     * /ManualBackupLimit:/ The number of current manual backups/maximum number of backups allowed. By default, you can have a maximum of 50 manual backups saved.
    name :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AccountAttribute' with the minimum fields required to make a request.
--
-- * 'used' - The current usage, such as the current number of servers that are associated with the account.
-- * 'maximum' - The maximum allowed value.
-- * 'name' - The attribute name. The following are supported attribute names.
--
--
--     * /ServerLimit:/ The number of current servers/maximum number of servers allowed. By default, you can have a maximum of 10 servers.
--
--
--     * /ManualBackupLimit:/ The number of current manual backups/maximum number of backups allowed. By default, you can have a maximum of 50 manual backups saved.
mkAccountAttribute ::
  AccountAttribute
mkAccountAttribute =
  AccountAttribute'
    { used = Lude.Nothing,
      maximum = Lude.Nothing,
      name = Lude.Nothing
    }

-- | The current usage, such as the current number of servers that are associated with the account.
--
-- /Note:/ Consider using 'used' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaUsed :: Lens.Lens' AccountAttribute (Lude.Maybe Lude.Int)
aaUsed = Lens.lens (used :: AccountAttribute -> Lude.Maybe Lude.Int) (\s a -> s {used = a} :: AccountAttribute)
{-# DEPRECATED aaUsed "Use generic-lens or generic-optics with 'used' instead." #-}

-- | The maximum allowed value.
--
-- /Note:/ Consider using 'maximum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaMaximum :: Lens.Lens' AccountAttribute (Lude.Maybe Lude.Int)
aaMaximum = Lens.lens (maximum :: AccountAttribute -> Lude.Maybe Lude.Int) (\s a -> s {maximum = a} :: AccountAttribute)
{-# DEPRECATED aaMaximum "Use generic-lens or generic-optics with 'maximum' instead." #-}

-- | The attribute name. The following are supported attribute names.
--
--
--     * /ServerLimit:/ The number of current servers/maximum number of servers allowed. By default, you can have a maximum of 10 servers.
--
--
--     * /ManualBackupLimit:/ The number of current manual backups/maximum number of backups allowed. By default, you can have a maximum of 50 manual backups saved.
--
--
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaName :: Lens.Lens' AccountAttribute (Lude.Maybe Lude.Text)
aaName = Lens.lens (name :: AccountAttribute -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: AccountAttribute)
{-# DEPRECATED aaName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON AccountAttribute where
  parseJSON =
    Lude.withObject
      "AccountAttribute"
      ( \x ->
          AccountAttribute'
            Lude.<$> (x Lude..:? "Used")
            Lude.<*> (x Lude..:? "Maximum")
            Lude.<*> (x Lude..:? "Name")
      )
