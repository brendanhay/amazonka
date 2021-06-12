{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.FSxWindowsFileServerAuthorizationConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.FSxWindowsFileServerAuthorizationConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The authorization configuration details for Amazon FSx for Windows File
-- Server file system. See
-- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_FSxWindowsFileServerVolumeConfiguration.html FSxWindowsFileServerVolumeConfiguration>
-- in the /Amazon Elastic Container Service API Reference/.
--
-- For more information and the input format, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/wfsx-volumes.html Amazon FSx for Windows File Server Volumes>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- /See:/ 'newFSxWindowsFileServerAuthorizationConfig' smart constructor.
data FSxWindowsFileServerAuthorizationConfig = FSxWindowsFileServerAuthorizationConfig'
  { -- | The authorization credential option to use. The authorization credential
    -- options can be provided using either the Amazon Resource Name (ARN) of
    -- an AWS Secrets Manager secret or AWS Systems Manager Parameter Store
    -- parameter. The ARNs refer to the stored credentials.
    credentialsParameter :: Core.Text,
    -- | A fully qualified domain name hosted by an
    -- <https://docs.aws.amazon.com/directoryservice/latest/admin-guide/directory_microsoft_ad.html AWS Directory Service>
    -- Managed Microsoft AD (Active Directory) or self-hosted AD on Amazon EC2.
    domain :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FSxWindowsFileServerAuthorizationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'credentialsParameter', 'fSxWindowsFileServerAuthorizationConfig_credentialsParameter' - The authorization credential option to use. The authorization credential
-- options can be provided using either the Amazon Resource Name (ARN) of
-- an AWS Secrets Manager secret or AWS Systems Manager Parameter Store
-- parameter. The ARNs refer to the stored credentials.
--
-- 'domain', 'fSxWindowsFileServerAuthorizationConfig_domain' - A fully qualified domain name hosted by an
-- <https://docs.aws.amazon.com/directoryservice/latest/admin-guide/directory_microsoft_ad.html AWS Directory Service>
-- Managed Microsoft AD (Active Directory) or self-hosted AD on Amazon EC2.
newFSxWindowsFileServerAuthorizationConfig ::
  -- | 'credentialsParameter'
  Core.Text ->
  -- | 'domain'
  Core.Text ->
  FSxWindowsFileServerAuthorizationConfig
newFSxWindowsFileServerAuthorizationConfig
  pCredentialsParameter_
  pDomain_ =
    FSxWindowsFileServerAuthorizationConfig'
      { credentialsParameter =
          pCredentialsParameter_,
        domain = pDomain_
      }

-- | The authorization credential option to use. The authorization credential
-- options can be provided using either the Amazon Resource Name (ARN) of
-- an AWS Secrets Manager secret or AWS Systems Manager Parameter Store
-- parameter. The ARNs refer to the stored credentials.
fSxWindowsFileServerAuthorizationConfig_credentialsParameter :: Lens.Lens' FSxWindowsFileServerAuthorizationConfig Core.Text
fSxWindowsFileServerAuthorizationConfig_credentialsParameter = Lens.lens (\FSxWindowsFileServerAuthorizationConfig' {credentialsParameter} -> credentialsParameter) (\s@FSxWindowsFileServerAuthorizationConfig' {} a -> s {credentialsParameter = a} :: FSxWindowsFileServerAuthorizationConfig)

-- | A fully qualified domain name hosted by an
-- <https://docs.aws.amazon.com/directoryservice/latest/admin-guide/directory_microsoft_ad.html AWS Directory Service>
-- Managed Microsoft AD (Active Directory) or self-hosted AD on Amazon EC2.
fSxWindowsFileServerAuthorizationConfig_domain :: Lens.Lens' FSxWindowsFileServerAuthorizationConfig Core.Text
fSxWindowsFileServerAuthorizationConfig_domain = Lens.lens (\FSxWindowsFileServerAuthorizationConfig' {domain} -> domain) (\s@FSxWindowsFileServerAuthorizationConfig' {} a -> s {domain = a} :: FSxWindowsFileServerAuthorizationConfig)

instance
  Core.FromJSON
    FSxWindowsFileServerAuthorizationConfig
  where
  parseJSON =
    Core.withObject
      "FSxWindowsFileServerAuthorizationConfig"
      ( \x ->
          FSxWindowsFileServerAuthorizationConfig'
            Core.<$> (x Core..: "credentialsParameter")
            Core.<*> (x Core..: "domain")
      )

instance
  Core.Hashable
    FSxWindowsFileServerAuthorizationConfig

instance
  Core.NFData
    FSxWindowsFileServerAuthorizationConfig

instance
  Core.ToJSON
    FSxWindowsFileServerAuthorizationConfig
  where
  toJSON FSxWindowsFileServerAuthorizationConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "credentialsParameter"
                  Core..= credentialsParameter
              ),
            Core.Just ("domain" Core..= domain)
          ]
      )
