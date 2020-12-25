{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.UserAuthConfigInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.UserAuthConfigInfo
  ( UserAuthConfigInfo (..),

    -- * Smart constructor
    mkUserAuthConfigInfo,

    -- * Lenses
    uaciAuthScheme,
    uaciDescription,
    uaciIAMAuth,
    uaciSecretArn,
    uaciUserName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.AuthScheme as Types
import qualified Network.AWS.RDS.Types.IAMAuthMode as Types
import qualified Network.AWS.RDS.Types.String as Types

-- | Returns the details of authentication used by a proxy to log in as a specific database user.
--
-- /See:/ 'mkUserAuthConfigInfo' smart constructor.
data UserAuthConfigInfo = UserAuthConfigInfo'
  { -- | The type of authentication that the proxy uses for connections from the proxy to the underlying database.
    authScheme :: Core.Maybe Types.AuthScheme,
    -- | A user-specified description about the authentication used by a proxy to log in as a specific database user.
    description :: Core.Maybe Types.String,
    -- | Whether to require or disallow AWS Identity and Access Management (IAM) authentication for connections to the proxy.
    iAMAuth :: Core.Maybe Types.IAMAuthMode,
    -- | The Amazon Resource Name (ARN) representing the secret that the proxy uses to authenticate to the RDS DB instance or Aurora DB cluster. These secrets are stored within Amazon Secrets Manager.
    secretArn :: Core.Maybe Types.String,
    -- | The name of the database user to which the proxy connects.
    userName :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UserAuthConfigInfo' value with any optional fields omitted.
mkUserAuthConfigInfo ::
  UserAuthConfigInfo
mkUserAuthConfigInfo =
  UserAuthConfigInfo'
    { authScheme = Core.Nothing,
      description = Core.Nothing,
      iAMAuth = Core.Nothing,
      secretArn = Core.Nothing,
      userName = Core.Nothing
    }

-- | The type of authentication that the proxy uses for connections from the proxy to the underlying database.
--
-- /Note:/ Consider using 'authScheme' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaciAuthScheme :: Lens.Lens' UserAuthConfigInfo (Core.Maybe Types.AuthScheme)
uaciAuthScheme = Lens.field @"authScheme"
{-# DEPRECATED uaciAuthScheme "Use generic-lens or generic-optics with 'authScheme' instead." #-}

-- | A user-specified description about the authentication used by a proxy to log in as a specific database user.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaciDescription :: Lens.Lens' UserAuthConfigInfo (Core.Maybe Types.String)
uaciDescription = Lens.field @"description"
{-# DEPRECATED uaciDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Whether to require or disallow AWS Identity and Access Management (IAM) authentication for connections to the proxy.
--
-- /Note:/ Consider using 'iAMAuth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaciIAMAuth :: Lens.Lens' UserAuthConfigInfo (Core.Maybe Types.IAMAuthMode)
uaciIAMAuth = Lens.field @"iAMAuth"
{-# DEPRECATED uaciIAMAuth "Use generic-lens or generic-optics with 'iAMAuth' instead." #-}

-- | The Amazon Resource Name (ARN) representing the secret that the proxy uses to authenticate to the RDS DB instance or Aurora DB cluster. These secrets are stored within Amazon Secrets Manager.
--
-- /Note:/ Consider using 'secretArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaciSecretArn :: Lens.Lens' UserAuthConfigInfo (Core.Maybe Types.String)
uaciSecretArn = Lens.field @"secretArn"
{-# DEPRECATED uaciSecretArn "Use generic-lens or generic-optics with 'secretArn' instead." #-}

-- | The name of the database user to which the proxy connects.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaciUserName :: Lens.Lens' UserAuthConfigInfo (Core.Maybe Types.String)
uaciUserName = Lens.field @"userName"
{-# DEPRECATED uaciUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Core.FromXML UserAuthConfigInfo where
  parseXML x =
    UserAuthConfigInfo'
      Core.<$> (x Core..@? "AuthScheme")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "IAMAuth")
      Core.<*> (x Core..@? "SecretArn")
      Core.<*> (x Core..@? "UserName")
