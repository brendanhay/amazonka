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
    uaciIAMAuth,
    uaciUserName,
    uaciAuthScheme,
    uaciSecretARN,
    uaciDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.AuthScheme
import Network.AWS.RDS.Types.IAMAuthMode

-- | Returns the details of authentication used by a proxy to log in as a specific database user.
--
-- /See:/ 'mkUserAuthConfigInfo' smart constructor.
data UserAuthConfigInfo = UserAuthConfigInfo'
  { iamAuth ::
      Lude.Maybe IAMAuthMode,
    userName :: Lude.Maybe Lude.Text,
    authScheme :: Lude.Maybe AuthScheme,
    secretARN :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserAuthConfigInfo' with the minimum fields required to make a request.
--
-- * 'authScheme' - The type of authentication that the proxy uses for connections from the proxy to the underlying database.
-- * 'description' - A user-specified description about the authentication used by a proxy to log in as a specific database user.
-- * 'iamAuth' - Whether to require or disallow AWS Identity and Access Management (IAM) authentication for connections to the proxy.
-- * 'secretARN' - The Amazon Resource Name (ARN) representing the secret that the proxy uses to authenticate to the RDS DB instance or Aurora DB cluster. These secrets are stored within Amazon Secrets Manager.
-- * 'userName' - The name of the database user to which the proxy connects.
mkUserAuthConfigInfo ::
  UserAuthConfigInfo
mkUserAuthConfigInfo =
  UserAuthConfigInfo'
    { iamAuth = Lude.Nothing,
      userName = Lude.Nothing,
      authScheme = Lude.Nothing,
      secretARN = Lude.Nothing,
      description = Lude.Nothing
    }

-- | Whether to require or disallow AWS Identity and Access Management (IAM) authentication for connections to the proxy.
--
-- /Note:/ Consider using 'iamAuth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaciIAMAuth :: Lens.Lens' UserAuthConfigInfo (Lude.Maybe IAMAuthMode)
uaciIAMAuth = Lens.lens (iamAuth :: UserAuthConfigInfo -> Lude.Maybe IAMAuthMode) (\s a -> s {iamAuth = a} :: UserAuthConfigInfo)
{-# DEPRECATED uaciIAMAuth "Use generic-lens or generic-optics with 'iamAuth' instead." #-}

-- | The name of the database user to which the proxy connects.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaciUserName :: Lens.Lens' UserAuthConfigInfo (Lude.Maybe Lude.Text)
uaciUserName = Lens.lens (userName :: UserAuthConfigInfo -> Lude.Maybe Lude.Text) (\s a -> s {userName = a} :: UserAuthConfigInfo)
{-# DEPRECATED uaciUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The type of authentication that the proxy uses for connections from the proxy to the underlying database.
--
-- /Note:/ Consider using 'authScheme' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaciAuthScheme :: Lens.Lens' UserAuthConfigInfo (Lude.Maybe AuthScheme)
uaciAuthScheme = Lens.lens (authScheme :: UserAuthConfigInfo -> Lude.Maybe AuthScheme) (\s a -> s {authScheme = a} :: UserAuthConfigInfo)
{-# DEPRECATED uaciAuthScheme "Use generic-lens or generic-optics with 'authScheme' instead." #-}

-- | The Amazon Resource Name (ARN) representing the secret that the proxy uses to authenticate to the RDS DB instance or Aurora DB cluster. These secrets are stored within Amazon Secrets Manager.
--
-- /Note:/ Consider using 'secretARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaciSecretARN :: Lens.Lens' UserAuthConfigInfo (Lude.Maybe Lude.Text)
uaciSecretARN = Lens.lens (secretARN :: UserAuthConfigInfo -> Lude.Maybe Lude.Text) (\s a -> s {secretARN = a} :: UserAuthConfigInfo)
{-# DEPRECATED uaciSecretARN "Use generic-lens or generic-optics with 'secretARN' instead." #-}

-- | A user-specified description about the authentication used by a proxy to log in as a specific database user.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaciDescription :: Lens.Lens' UserAuthConfigInfo (Lude.Maybe Lude.Text)
uaciDescription = Lens.lens (description :: UserAuthConfigInfo -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UserAuthConfigInfo)
{-# DEPRECATED uaciDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML UserAuthConfigInfo where
  parseXML x =
    UserAuthConfigInfo'
      Lude.<$> (x Lude..@? "IAMAuth")
      Lude.<*> (x Lude..@? "UserName")
      Lude.<*> (x Lude..@? "AuthScheme")
      Lude.<*> (x Lude..@? "SecretArn")
      Lude.<*> (x Lude..@? "Description")
