{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    credentialsParameter :: Prelude.Text,
    -- | A fully qualified domain name hosted by an
    -- <https://docs.aws.amazon.com/directoryservice/latest/admin-guide/directory_microsoft_ad.html AWS Directory Service>
    -- Managed Microsoft AD (Active Directory) or self-hosted AD on Amazon EC2.
    domain :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'domain'
  Prelude.Text ->
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
fSxWindowsFileServerAuthorizationConfig_credentialsParameter :: Lens.Lens' FSxWindowsFileServerAuthorizationConfig Prelude.Text
fSxWindowsFileServerAuthorizationConfig_credentialsParameter = Lens.lens (\FSxWindowsFileServerAuthorizationConfig' {credentialsParameter} -> credentialsParameter) (\s@FSxWindowsFileServerAuthorizationConfig' {} a -> s {credentialsParameter = a} :: FSxWindowsFileServerAuthorizationConfig)

-- | A fully qualified domain name hosted by an
-- <https://docs.aws.amazon.com/directoryservice/latest/admin-guide/directory_microsoft_ad.html AWS Directory Service>
-- Managed Microsoft AD (Active Directory) or self-hosted AD on Amazon EC2.
fSxWindowsFileServerAuthorizationConfig_domain :: Lens.Lens' FSxWindowsFileServerAuthorizationConfig Prelude.Text
fSxWindowsFileServerAuthorizationConfig_domain = Lens.lens (\FSxWindowsFileServerAuthorizationConfig' {domain} -> domain) (\s@FSxWindowsFileServerAuthorizationConfig' {} a -> s {domain = a} :: FSxWindowsFileServerAuthorizationConfig)

instance
  Prelude.FromJSON
    FSxWindowsFileServerAuthorizationConfig
  where
  parseJSON =
    Prelude.withObject
      "FSxWindowsFileServerAuthorizationConfig"
      ( \x ->
          FSxWindowsFileServerAuthorizationConfig'
            Prelude.<$> (x Prelude..: "credentialsParameter")
            Prelude.<*> (x Prelude..: "domain")
      )

instance
  Prelude.Hashable
    FSxWindowsFileServerAuthorizationConfig

instance
  Prelude.NFData
    FSxWindowsFileServerAuthorizationConfig

instance
  Prelude.ToJSON
    FSxWindowsFileServerAuthorizationConfig
  where
  toJSON FSxWindowsFileServerAuthorizationConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "credentialsParameter"
                  Prelude..= credentialsParameter
              ),
            Prelude.Just ("domain" Prelude..= domain)
          ]
      )
