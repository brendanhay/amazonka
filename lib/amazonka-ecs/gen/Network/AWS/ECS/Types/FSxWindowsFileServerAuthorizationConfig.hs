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
    fswfsacDomain,
    fswfsacCredentialsParameter,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The authorization configuration details for Amazon FSx for Windows File Server file system. See <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_FSxWindowsFileServerVolumeConfiguration.html FSxWindowsFileServerVolumeConfiguration> in the /Amazon Elastic Container Service API Reference/ .
--
-- For more information and the input format, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/wfsx-volumes.html Amazon FSx for Windows File Server Volumes> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /See:/ 'mkFSxWindowsFileServerAuthorizationConfig' smart constructor.
data FSxWindowsFileServerAuthorizationConfig = FSxWindowsFileServerAuthorizationConfig'
  { -- | A fully qualified domain name hosted by an <https://docs.aws.amazon.com/directoryservice/latest/admin-guide/directory_microsoft_ad.html AWS Directory Service> Managed Microsoft AD (Active Directory) or self-hosted AD on Amazon EC2.
    domain :: Lude.Text,
    -- | The authorization credential option to use. The authorization credential options can be provided using either the Amazon Resource Name (ARN) of an AWS Secrets Manager secret or AWS Systems Manager Parameter Store parameter. The ARNs refer to the stored credentials.
    credentialsParameter :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FSxWindowsFileServerAuthorizationConfig' with the minimum fields required to make a request.
--
-- * 'domain' - A fully qualified domain name hosted by an <https://docs.aws.amazon.com/directoryservice/latest/admin-guide/directory_microsoft_ad.html AWS Directory Service> Managed Microsoft AD (Active Directory) or self-hosted AD on Amazon EC2.
-- * 'credentialsParameter' - The authorization credential option to use. The authorization credential options can be provided using either the Amazon Resource Name (ARN) of an AWS Secrets Manager secret or AWS Systems Manager Parameter Store parameter. The ARNs refer to the stored credentials.
mkFSxWindowsFileServerAuthorizationConfig ::
  -- | 'domain'
  Lude.Text ->
  -- | 'credentialsParameter'
  Lude.Text ->
  FSxWindowsFileServerAuthorizationConfig
mkFSxWindowsFileServerAuthorizationConfig
  pDomain_
  pCredentialsParameter_ =
    FSxWindowsFileServerAuthorizationConfig'
      { domain = pDomain_,
        credentialsParameter = pCredentialsParameter_
      }

-- | A fully qualified domain name hosted by an <https://docs.aws.amazon.com/directoryservice/latest/admin-guide/directory_microsoft_ad.html AWS Directory Service> Managed Microsoft AD (Active Directory) or self-hosted AD on Amazon EC2.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fswfsacDomain :: Lens.Lens' FSxWindowsFileServerAuthorizationConfig Lude.Text
fswfsacDomain = Lens.lens (domain :: FSxWindowsFileServerAuthorizationConfig -> Lude.Text) (\s a -> s {domain = a} :: FSxWindowsFileServerAuthorizationConfig)
{-# DEPRECATED fswfsacDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | The authorization credential option to use. The authorization credential options can be provided using either the Amazon Resource Name (ARN) of an AWS Secrets Manager secret or AWS Systems Manager Parameter Store parameter. The ARNs refer to the stored credentials.
--
-- /Note:/ Consider using 'credentialsParameter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fswfsacCredentialsParameter :: Lens.Lens' FSxWindowsFileServerAuthorizationConfig Lude.Text
fswfsacCredentialsParameter = Lens.lens (credentialsParameter :: FSxWindowsFileServerAuthorizationConfig -> Lude.Text) (\s a -> s {credentialsParameter = a} :: FSxWindowsFileServerAuthorizationConfig)
{-# DEPRECATED fswfsacCredentialsParameter "Use generic-lens or generic-optics with 'credentialsParameter' instead." #-}

instance Lude.FromJSON FSxWindowsFileServerAuthorizationConfig where
  parseJSON =
    Lude.withObject
      "FSxWindowsFileServerAuthorizationConfig"
      ( \x ->
          FSxWindowsFileServerAuthorizationConfig'
            Lude.<$> (x Lude..: "domain") Lude.<*> (x Lude..: "credentialsParameter")
      )

instance Lude.ToJSON FSxWindowsFileServerAuthorizationConfig where
  toJSON FSxWindowsFileServerAuthorizationConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("domain" Lude..= domain),
            Lude.Just ("credentialsParameter" Lude..= credentialsParameter)
          ]
      )
