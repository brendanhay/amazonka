{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.UserAuthConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.UserAuthConfig
  ( UserAuthConfig (..),

    -- * Smart constructor
    mkUserAuthConfig,

    -- * Lenses
    uacIAMAuth,
    uacUserName,
    uacAuthScheme,
    uacSecretARN,
    uacDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.AuthScheme
import Network.AWS.RDS.Types.IAMAuthMode

-- | Specifies the details of authentication used by a proxy to log in as a specific database user.
--
-- /See:/ 'mkUserAuthConfig' smart constructor.
data UserAuthConfig = UserAuthConfig'
  { -- | Whether to require or disallow AWS Identity and Access Management (IAM) authentication for connections to the proxy.
    iamAuth :: Lude.Maybe IAMAuthMode,
    -- | The name of the database user to which the proxy connects.
    userName :: Lude.Maybe Lude.Text,
    -- | The type of authentication that the proxy uses for connections from the proxy to the underlying database.
    authScheme :: Lude.Maybe AuthScheme,
    -- | The Amazon Resource Name (ARN) representing the secret that the proxy uses to authenticate to the RDS DB instance or Aurora DB cluster. These secrets are stored within Amazon Secrets Manager.
    secretARN :: Lude.Maybe Lude.Text,
    -- | A user-specified description about the authentication used by a proxy to log in as a specific database user.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserAuthConfig' with the minimum fields required to make a request.
--
-- * 'iamAuth' - Whether to require or disallow AWS Identity and Access Management (IAM) authentication for connections to the proxy.
-- * 'userName' - The name of the database user to which the proxy connects.
-- * 'authScheme' - The type of authentication that the proxy uses for connections from the proxy to the underlying database.
-- * 'secretARN' - The Amazon Resource Name (ARN) representing the secret that the proxy uses to authenticate to the RDS DB instance or Aurora DB cluster. These secrets are stored within Amazon Secrets Manager.
-- * 'description' - A user-specified description about the authentication used by a proxy to log in as a specific database user.
mkUserAuthConfig ::
  UserAuthConfig
mkUserAuthConfig =
  UserAuthConfig'
    { iamAuth = Lude.Nothing,
      userName = Lude.Nothing,
      authScheme = Lude.Nothing,
      secretARN = Lude.Nothing,
      description = Lude.Nothing
    }

-- | Whether to require or disallow AWS Identity and Access Management (IAM) authentication for connections to the proxy.
--
-- /Note:/ Consider using 'iamAuth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uacIAMAuth :: Lens.Lens' UserAuthConfig (Lude.Maybe IAMAuthMode)
uacIAMAuth = Lens.lens (iamAuth :: UserAuthConfig -> Lude.Maybe IAMAuthMode) (\s a -> s {iamAuth = a} :: UserAuthConfig)
{-# DEPRECATED uacIAMAuth "Use generic-lens or generic-optics with 'iamAuth' instead." #-}

-- | The name of the database user to which the proxy connects.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uacUserName :: Lens.Lens' UserAuthConfig (Lude.Maybe Lude.Text)
uacUserName = Lens.lens (userName :: UserAuthConfig -> Lude.Maybe Lude.Text) (\s a -> s {userName = a} :: UserAuthConfig)
{-# DEPRECATED uacUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The type of authentication that the proxy uses for connections from the proxy to the underlying database.
--
-- /Note:/ Consider using 'authScheme' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uacAuthScheme :: Lens.Lens' UserAuthConfig (Lude.Maybe AuthScheme)
uacAuthScheme = Lens.lens (authScheme :: UserAuthConfig -> Lude.Maybe AuthScheme) (\s a -> s {authScheme = a} :: UserAuthConfig)
{-# DEPRECATED uacAuthScheme "Use generic-lens or generic-optics with 'authScheme' instead." #-}

-- | The Amazon Resource Name (ARN) representing the secret that the proxy uses to authenticate to the RDS DB instance or Aurora DB cluster. These secrets are stored within Amazon Secrets Manager.
--
-- /Note:/ Consider using 'secretARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uacSecretARN :: Lens.Lens' UserAuthConfig (Lude.Maybe Lude.Text)
uacSecretARN = Lens.lens (secretARN :: UserAuthConfig -> Lude.Maybe Lude.Text) (\s a -> s {secretARN = a} :: UserAuthConfig)
{-# DEPRECATED uacSecretARN "Use generic-lens or generic-optics with 'secretARN' instead." #-}

-- | A user-specified description about the authentication used by a proxy to log in as a specific database user.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uacDescription :: Lens.Lens' UserAuthConfig (Lude.Maybe Lude.Text)
uacDescription = Lens.lens (description :: UserAuthConfig -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UserAuthConfig)
{-# DEPRECATED uacDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.ToQuery UserAuthConfig where
  toQuery UserAuthConfig' {..} =
    Lude.mconcat
      [ "IAMAuth" Lude.=: iamAuth,
        "UserName" Lude.=: userName,
        "AuthScheme" Lude.=: authScheme,
        "SecretArn" Lude.=: secretARN,
        "Description" Lude.=: description
      ]
