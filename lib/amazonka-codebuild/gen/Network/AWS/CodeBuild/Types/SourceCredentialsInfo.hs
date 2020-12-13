{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.SourceCredentialsInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.SourceCredentialsInfo
  ( SourceCredentialsInfo (..),

    -- * Smart constructor
    mkSourceCredentialsInfo,

    -- * Lenses
    sciArn,
    sciServerType,
    sciAuthType,
  )
where

import Network.AWS.CodeBuild.Types.AuthType
import Network.AWS.CodeBuild.Types.ServerType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the credentials for a GitHub, GitHub Enterprise, or Bitbucket repository.
--
-- /See:/ 'mkSourceCredentialsInfo' smart constructor.
data SourceCredentialsInfo = SourceCredentialsInfo'
  { -- | The Amazon Resource Name (ARN) of the token.
    arn :: Lude.Maybe Lude.Text,
    -- | The type of source provider. The valid options are GITHUB, GITHUB_ENTERPRISE, or BITBUCKET.
    serverType :: Lude.Maybe ServerType,
    -- | The type of authentication used by the credentials. Valid options are OAUTH, BASIC_AUTH, or PERSONAL_ACCESS_TOKEN.
    authType :: Lude.Maybe AuthType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SourceCredentialsInfo' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the token.
-- * 'serverType' - The type of source provider. The valid options are GITHUB, GITHUB_ENTERPRISE, or BITBUCKET.
-- * 'authType' - The type of authentication used by the credentials. Valid options are OAUTH, BASIC_AUTH, or PERSONAL_ACCESS_TOKEN.
mkSourceCredentialsInfo ::
  SourceCredentialsInfo
mkSourceCredentialsInfo =
  SourceCredentialsInfo'
    { arn = Lude.Nothing,
      serverType = Lude.Nothing,
      authType = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the token.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sciArn :: Lens.Lens' SourceCredentialsInfo (Lude.Maybe Lude.Text)
sciArn = Lens.lens (arn :: SourceCredentialsInfo -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: SourceCredentialsInfo)
{-# DEPRECATED sciArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The type of source provider. The valid options are GITHUB, GITHUB_ENTERPRISE, or BITBUCKET.
--
-- /Note:/ Consider using 'serverType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sciServerType :: Lens.Lens' SourceCredentialsInfo (Lude.Maybe ServerType)
sciServerType = Lens.lens (serverType :: SourceCredentialsInfo -> Lude.Maybe ServerType) (\s a -> s {serverType = a} :: SourceCredentialsInfo)
{-# DEPRECATED sciServerType "Use generic-lens or generic-optics with 'serverType' instead." #-}

-- | The type of authentication used by the credentials. Valid options are OAUTH, BASIC_AUTH, or PERSONAL_ACCESS_TOKEN.
--
-- /Note:/ Consider using 'authType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sciAuthType :: Lens.Lens' SourceCredentialsInfo (Lude.Maybe AuthType)
sciAuthType = Lens.lens (authType :: SourceCredentialsInfo -> Lude.Maybe AuthType) (\s a -> s {authType = a} :: SourceCredentialsInfo)
{-# DEPRECATED sciAuthType "Use generic-lens or generic-optics with 'authType' instead." #-}

instance Lude.FromJSON SourceCredentialsInfo where
  parseJSON =
    Lude.withObject
      "SourceCredentialsInfo"
      ( \x ->
          SourceCredentialsInfo'
            Lude.<$> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "serverType")
            Lude.<*> (x Lude..:? "authType")
      )
