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
    sciAuthType,
    sciServerType,
  )
where

import qualified Network.AWS.CodeBuild.Types.Arn as Types
import qualified Network.AWS.CodeBuild.Types.AuthType as Types
import qualified Network.AWS.CodeBuild.Types.ServerType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the credentials for a GitHub, GitHub Enterprise, or Bitbucket repository.
--
-- /See:/ 'mkSourceCredentialsInfo' smart constructor.
data SourceCredentialsInfo = SourceCredentialsInfo'
  { -- | The Amazon Resource Name (ARN) of the token.
    arn :: Core.Maybe Types.Arn,
    -- | The type of authentication used by the credentials. Valid options are OAUTH, BASIC_AUTH, or PERSONAL_ACCESS_TOKEN.
    authType :: Core.Maybe Types.AuthType,
    -- | The type of source provider. The valid options are GITHUB, GITHUB_ENTERPRISE, or BITBUCKET.
    serverType :: Core.Maybe Types.ServerType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SourceCredentialsInfo' value with any optional fields omitted.
mkSourceCredentialsInfo ::
  SourceCredentialsInfo
mkSourceCredentialsInfo =
  SourceCredentialsInfo'
    { arn = Core.Nothing,
      authType = Core.Nothing,
      serverType = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the token.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sciArn :: Lens.Lens' SourceCredentialsInfo (Core.Maybe Types.Arn)
sciArn = Lens.field @"arn"
{-# DEPRECATED sciArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The type of authentication used by the credentials. Valid options are OAUTH, BASIC_AUTH, or PERSONAL_ACCESS_TOKEN.
--
-- /Note:/ Consider using 'authType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sciAuthType :: Lens.Lens' SourceCredentialsInfo (Core.Maybe Types.AuthType)
sciAuthType = Lens.field @"authType"
{-# DEPRECATED sciAuthType "Use generic-lens or generic-optics with 'authType' instead." #-}

-- | The type of source provider. The valid options are GITHUB, GITHUB_ENTERPRISE, or BITBUCKET.
--
-- /Note:/ Consider using 'serverType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sciServerType :: Lens.Lens' SourceCredentialsInfo (Core.Maybe Types.ServerType)
sciServerType = Lens.field @"serverType"
{-# DEPRECATED sciServerType "Use generic-lens or generic-optics with 'serverType' instead." #-}

instance Core.FromJSON SourceCredentialsInfo where
  parseJSON =
    Core.withObject "SourceCredentialsInfo" Core.$
      \x ->
        SourceCredentialsInfo'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "authType")
          Core.<*> (x Core..:? "serverType")
