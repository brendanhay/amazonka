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

import qualified Network.AWS.AppStream.Types.AccountName as Types
import qualified Network.AWS.AppStream.Types.AccountPassword as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the credentials for the service account used by the fleet or image builder to connect to the directory.
--
-- /See:/ 'mkServiceAccountCredentials' smart constructor.
data ServiceAccountCredentials = ServiceAccountCredentials'
  { -- | The user name of the account. This account must have the following privileges: create computer objects, join computers to the domain, and change/reset the password on descendant computer objects for the organizational units specified.
    accountName :: Types.AccountName,
    -- | The password for the account.
    accountPassword :: Types.AccountPassword
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ServiceAccountCredentials' value with any optional fields omitted.
mkServiceAccountCredentials ::
  -- | 'accountName'
  Types.AccountName ->
  -- | 'accountPassword'
  Types.AccountPassword ->
  ServiceAccountCredentials
mkServiceAccountCredentials accountName accountPassword =
  ServiceAccountCredentials' {accountName, accountPassword}

-- | The user name of the account. This account must have the following privileges: create computer objects, join computers to the domain, and change/reset the password on descendant computer objects for the organizational units specified.
--
-- /Note:/ Consider using 'accountName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sacAccountName :: Lens.Lens' ServiceAccountCredentials Types.AccountName
sacAccountName = Lens.field @"accountName"
{-# DEPRECATED sacAccountName "Use generic-lens or generic-optics with 'accountName' instead." #-}

-- | The password for the account.
--
-- /Note:/ Consider using 'accountPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sacAccountPassword :: Lens.Lens' ServiceAccountCredentials Types.AccountPassword
sacAccountPassword = Lens.field @"accountPassword"
{-# DEPRECATED sacAccountPassword "Use generic-lens or generic-optics with 'accountPassword' instead." #-}

instance Core.FromJSON ServiceAccountCredentials where
  toJSON ServiceAccountCredentials {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AccountName" Core..= accountName),
            Core.Just ("AccountPassword" Core..= accountPassword)
          ]
      )

instance Core.FromJSON ServiceAccountCredentials where
  parseJSON =
    Core.withObject "ServiceAccountCredentials" Core.$
      \x ->
        ServiceAccountCredentials'
          Core.<$> (x Core..: "AccountName") Core.<*> (x Core..: "AccountPassword")
