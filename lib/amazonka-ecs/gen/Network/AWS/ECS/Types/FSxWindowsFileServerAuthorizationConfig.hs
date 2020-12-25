{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.FSxWindowsFileServerAuthorizationConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.FSxWindowsFileServerAuthorizationConfig
  ( FSxWindowsFileServerAuthorizationConfig (..),

    -- * Smart constructor
    mkFSxWindowsFileServerAuthorizationConfig,

    -- * Lenses
    fswfsacCredentialsParameter,
    fswfsacDomain,
  )
where

import qualified Network.AWS.ECS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The authorization configuration details for Amazon FSx for Windows File Server file system. See <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_FSxWindowsFileServerVolumeConfiguration.html FSxWindowsFileServerVolumeConfiguration> in the /Amazon Elastic Container Service API Reference/ .
--
-- For more information and the input format, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/wfsx-volumes.html Amazon FSx for Windows File Server Volumes> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /See:/ 'mkFSxWindowsFileServerAuthorizationConfig' smart constructor.
data FSxWindowsFileServerAuthorizationConfig = FSxWindowsFileServerAuthorizationConfig'
  { -- | The authorization credential option to use. The authorization credential options can be provided using either the Amazon Resource Name (ARN) of an AWS Secrets Manager secret or AWS Systems Manager Parameter Store parameter. The ARNs refer to the stored credentials.
    credentialsParameter :: Types.String,
    -- | A fully qualified domain name hosted by an <https://docs.aws.amazon.com/directoryservice/latest/admin-guide/directory_microsoft_ad.html AWS Directory Service> Managed Microsoft AD (Active Directory) or self-hosted AD on Amazon EC2.
    domain :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FSxWindowsFileServerAuthorizationConfig' value with any optional fields omitted.
mkFSxWindowsFileServerAuthorizationConfig ::
  -- | 'credentialsParameter'
  Types.String ->
  -- | 'domain'
  Types.String ->
  FSxWindowsFileServerAuthorizationConfig
mkFSxWindowsFileServerAuthorizationConfig
  credentialsParameter
  domain =
    FSxWindowsFileServerAuthorizationConfig'
      { credentialsParameter,
        domain
      }

-- | The authorization credential option to use. The authorization credential options can be provided using either the Amazon Resource Name (ARN) of an AWS Secrets Manager secret or AWS Systems Manager Parameter Store parameter. The ARNs refer to the stored credentials.
--
-- /Note:/ Consider using 'credentialsParameter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fswfsacCredentialsParameter :: Lens.Lens' FSxWindowsFileServerAuthorizationConfig Types.String
fswfsacCredentialsParameter = Lens.field @"credentialsParameter"
{-# DEPRECATED fswfsacCredentialsParameter "Use generic-lens or generic-optics with 'credentialsParameter' instead." #-}

-- | A fully qualified domain name hosted by an <https://docs.aws.amazon.com/directoryservice/latest/admin-guide/directory_microsoft_ad.html AWS Directory Service> Managed Microsoft AD (Active Directory) or self-hosted AD on Amazon EC2.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fswfsacDomain :: Lens.Lens' FSxWindowsFileServerAuthorizationConfig Types.String
fswfsacDomain = Lens.field @"domain"
{-# DEPRECATED fswfsacDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

instance Core.FromJSON FSxWindowsFileServerAuthorizationConfig where
  toJSON FSxWindowsFileServerAuthorizationConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("credentialsParameter" Core..= credentialsParameter),
            Core.Just ("domain" Core..= domain)
          ]
      )

instance Core.FromJSON FSxWindowsFileServerAuthorizationConfig where
  parseJSON =
    Core.withObject "FSxWindowsFileServerAuthorizationConfig" Core.$
      \x ->
        FSxWindowsFileServerAuthorizationConfig'
          Core.<$> (x Core..: "credentialsParameter") Core.<*> (x Core..: "domain")
