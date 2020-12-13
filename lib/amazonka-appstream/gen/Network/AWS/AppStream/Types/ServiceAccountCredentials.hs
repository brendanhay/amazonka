{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ServiceAccountCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ServiceAccountCredentials
  ( ServiceAccountCredentials (..),

    -- * Smart constructor
    mkServiceAccountCredentials,

    -- * Lenses
    sacAccountName,
    sacAccountPassword,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the credentials for the service account used by the fleet or image builder to connect to the directory.
--
-- /See:/ 'mkServiceAccountCredentials' smart constructor.
data ServiceAccountCredentials = ServiceAccountCredentials'
  { -- | The user name of the account. This account must have the following privileges: create computer objects, join computers to the domain, and change/reset the password on descendant computer objects for the organizational units specified.
    accountName :: Lude.Sensitive Lude.Text,
    -- | The password for the account.
    accountPassword :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServiceAccountCredentials' with the minimum fields required to make a request.
--
-- * 'accountName' - The user name of the account. This account must have the following privileges: create computer objects, join computers to the domain, and change/reset the password on descendant computer objects for the organizational units specified.
-- * 'accountPassword' - The password for the account.
mkServiceAccountCredentials ::
  -- | 'accountName'
  Lude.Sensitive Lude.Text ->
  -- | 'accountPassword'
  Lude.Sensitive Lude.Text ->
  ServiceAccountCredentials
mkServiceAccountCredentials pAccountName_ pAccountPassword_ =
  ServiceAccountCredentials'
    { accountName = pAccountName_,
      accountPassword = pAccountPassword_
    }

-- | The user name of the account. This account must have the following privileges: create computer objects, join computers to the domain, and change/reset the password on descendant computer objects for the organizational units specified.
--
-- /Note:/ Consider using 'accountName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sacAccountName :: Lens.Lens' ServiceAccountCredentials (Lude.Sensitive Lude.Text)
sacAccountName = Lens.lens (accountName :: ServiceAccountCredentials -> Lude.Sensitive Lude.Text) (\s a -> s {accountName = a} :: ServiceAccountCredentials)
{-# DEPRECATED sacAccountName "Use generic-lens or generic-optics with 'accountName' instead." #-}

-- | The password for the account.
--
-- /Note:/ Consider using 'accountPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sacAccountPassword :: Lens.Lens' ServiceAccountCredentials (Lude.Sensitive Lude.Text)
sacAccountPassword = Lens.lens (accountPassword :: ServiceAccountCredentials -> Lude.Sensitive Lude.Text) (\s a -> s {accountPassword = a} :: ServiceAccountCredentials)
{-# DEPRECATED sacAccountPassword "Use generic-lens or generic-optics with 'accountPassword' instead." #-}

instance Lude.FromJSON ServiceAccountCredentials where
  parseJSON =
    Lude.withObject
      "ServiceAccountCredentials"
      ( \x ->
          ServiceAccountCredentials'
            Lude.<$> (x Lude..: "AccountName") Lude.<*> (x Lude..: "AccountPassword")
      )

instance Lude.ToJSON ServiceAccountCredentials where
  toJSON ServiceAccountCredentials' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AccountName" Lude..= accountName),
            Lude.Just ("AccountPassword" Lude..= accountPassword)
          ]
      )
