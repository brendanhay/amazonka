{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The description of the domain.
module Network.AWS.SageMaker.DescribeDomain
  ( -- * Creating a Request
    DescribeDomain (..),
    newDescribeDomain,

    -- * Request Lenses
    describeDomain_domainId,

    -- * Destructuring the Response
    DescribeDomainResponse (..),
    newDescribeDomainResponse,

    -- * Response Lenses
    describeDomainResponse_creationTime,
    describeDomainResponse_status,
    describeDomainResponse_singleSignOnManagedApplicationInstanceId,
    describeDomainResponse_authMode,
    describeDomainResponse_domainArn,
    describeDomainResponse_subnetIds,
    describeDomainResponse_domainId,
    describeDomainResponse_kmsKeyId,
    describeDomainResponse_domainName,
    describeDomainResponse_defaultUserSettings,
    describeDomainResponse_failureReason,
    describeDomainResponse_homeEfsFileSystemId,
    describeDomainResponse_lastModifiedTime,
    describeDomainResponse_url,
    describeDomainResponse_homeEfsFileSystemKmsKeyId,
    describeDomainResponse_appNetworkAccessType,
    describeDomainResponse_vpcId,
    describeDomainResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeDomain' smart constructor.
data DescribeDomain = DescribeDomain'
  { -- | The domain ID.
    domainId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainId', 'describeDomain_domainId' - The domain ID.
newDescribeDomain ::
  -- | 'domainId'
  Prelude.Text ->
  DescribeDomain
newDescribeDomain pDomainId_ =
  DescribeDomain' {domainId = pDomainId_}

-- | The domain ID.
describeDomain_domainId :: Lens.Lens' DescribeDomain Prelude.Text
describeDomain_domainId = Lens.lens (\DescribeDomain' {domainId} -> domainId) (\s@DescribeDomain' {} a -> s {domainId = a} :: DescribeDomain)

instance Core.AWSRequest DescribeDomain where
  type
    AWSResponse DescribeDomain =
      DescribeDomainResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDomainResponse'
            Prelude.<$> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> ( x
                            Core..?> "SingleSignOnManagedApplicationInstanceId"
                        )
            Prelude.<*> (x Core..?> "AuthMode")
            Prelude.<*> (x Core..?> "DomainArn")
            Prelude.<*> (x Core..?> "SubnetIds")
            Prelude.<*> (x Core..?> "DomainId")
            Prelude.<*> (x Core..?> "KmsKeyId")
            Prelude.<*> (x Core..?> "DomainName")
            Prelude.<*> (x Core..?> "DefaultUserSettings")
            Prelude.<*> (x Core..?> "FailureReason")
            Prelude.<*> (x Core..?> "HomeEfsFileSystemId")
            Prelude.<*> (x Core..?> "LastModifiedTime")
            Prelude.<*> (x Core..?> "Url")
            Prelude.<*> (x Core..?> "HomeEfsFileSystemKmsKeyId")
            Prelude.<*> (x Core..?> "AppNetworkAccessType")
            Prelude.<*> (x Core..?> "VpcId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDomain

instance Prelude.NFData DescribeDomain

instance Core.ToHeaders DescribeDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DescribeDomain" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeDomain where
  toJSON DescribeDomain' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("DomainId" Core..= domainId)]
      )

instance Core.ToPath DescribeDomain where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeDomain where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDomainResponse' smart constructor.
data DescribeDomainResponse = DescribeDomainResponse'
  { -- | The creation time.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The status.
    status :: Prelude.Maybe DomainStatus,
    -- | The SSO managed application instance ID.
    singleSignOnManagedApplicationInstanceId :: Prelude.Maybe Prelude.Text,
    -- | The domain\'s authentication mode.
    authMode :: Prelude.Maybe AuthMode,
    -- | The domain\'s Amazon Resource Name (ARN).
    domainArn :: Prelude.Maybe Prelude.Text,
    -- | The VPC subnets that Studio uses for communication.
    subnetIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The domain ID.
    domainId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services KMS customer managed key used to encrypt the EFS
    -- volume attached to the domain.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The domain name.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | Settings which are applied to UserProfiles in this domain if settings
    -- are not explicitly specified in a given UserProfile.
    defaultUserSettings :: Prelude.Maybe UserSettings,
    -- | The failure reason.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Elastic File System (EFS) managed by this Domain.
    homeEfsFileSystemId :: Prelude.Maybe Prelude.Text,
    -- | The last modified time.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The domain\'s URL.
    url :: Prelude.Maybe Prelude.Text,
    -- | This member is deprecated and replaced with @KmsKeyId@.
    homeEfsFileSystemKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the VPC used for non-EFS traffic. The default value is
    -- @PublicInternetOnly@.
    --
    -- -   @PublicInternetOnly@ - Non-EFS traffic is through a VPC managed by
    --     Amazon SageMaker, which allows direct internet access
    --
    -- -   @VpcOnly@ - All Studio traffic is through the specified VPC and
    --     subnets
    appNetworkAccessType :: Prelude.Maybe AppNetworkAccessType,
    -- | The ID of the Amazon Virtual Private Cloud (VPC) that Studio uses for
    -- communication.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'describeDomainResponse_creationTime' - The creation time.
--
-- 'status', 'describeDomainResponse_status' - The status.
--
-- 'singleSignOnManagedApplicationInstanceId', 'describeDomainResponse_singleSignOnManagedApplicationInstanceId' - The SSO managed application instance ID.
--
-- 'authMode', 'describeDomainResponse_authMode' - The domain\'s authentication mode.
--
-- 'domainArn', 'describeDomainResponse_domainArn' - The domain\'s Amazon Resource Name (ARN).
--
-- 'subnetIds', 'describeDomainResponse_subnetIds' - The VPC subnets that Studio uses for communication.
--
-- 'domainId', 'describeDomainResponse_domainId' - The domain ID.
--
-- 'kmsKeyId', 'describeDomainResponse_kmsKeyId' - The Amazon Web Services KMS customer managed key used to encrypt the EFS
-- volume attached to the domain.
--
-- 'domainName', 'describeDomainResponse_domainName' - The domain name.
--
-- 'defaultUserSettings', 'describeDomainResponse_defaultUserSettings' - Settings which are applied to UserProfiles in this domain if settings
-- are not explicitly specified in a given UserProfile.
--
-- 'failureReason', 'describeDomainResponse_failureReason' - The failure reason.
--
-- 'homeEfsFileSystemId', 'describeDomainResponse_homeEfsFileSystemId' - The ID of the Amazon Elastic File System (EFS) managed by this Domain.
--
-- 'lastModifiedTime', 'describeDomainResponse_lastModifiedTime' - The last modified time.
--
-- 'url', 'describeDomainResponse_url' - The domain\'s URL.
--
-- 'homeEfsFileSystemKmsKeyId', 'describeDomainResponse_homeEfsFileSystemKmsKeyId' - This member is deprecated and replaced with @KmsKeyId@.
--
-- 'appNetworkAccessType', 'describeDomainResponse_appNetworkAccessType' - Specifies the VPC used for non-EFS traffic. The default value is
-- @PublicInternetOnly@.
--
-- -   @PublicInternetOnly@ - Non-EFS traffic is through a VPC managed by
--     Amazon SageMaker, which allows direct internet access
--
-- -   @VpcOnly@ - All Studio traffic is through the specified VPC and
--     subnets
--
-- 'vpcId', 'describeDomainResponse_vpcId' - The ID of the Amazon Virtual Private Cloud (VPC) that Studio uses for
-- communication.
--
-- 'httpStatus', 'describeDomainResponse_httpStatus' - The response's http status code.
newDescribeDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDomainResponse
newDescribeDomainResponse pHttpStatus_ =
  DescribeDomainResponse'
    { creationTime =
        Prelude.Nothing,
      status = Prelude.Nothing,
      singleSignOnManagedApplicationInstanceId =
        Prelude.Nothing,
      authMode = Prelude.Nothing,
      domainArn = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      domainId = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      domainName = Prelude.Nothing,
      defaultUserSettings = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      homeEfsFileSystemId = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      url = Prelude.Nothing,
      homeEfsFileSystemKmsKeyId = Prelude.Nothing,
      appNetworkAccessType = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The creation time.
describeDomainResponse_creationTime :: Lens.Lens' DescribeDomainResponse (Prelude.Maybe Prelude.UTCTime)
describeDomainResponse_creationTime = Lens.lens (\DescribeDomainResponse' {creationTime} -> creationTime) (\s@DescribeDomainResponse' {} a -> s {creationTime = a} :: DescribeDomainResponse) Prelude.. Lens.mapping Core._Time

-- | The status.
describeDomainResponse_status :: Lens.Lens' DescribeDomainResponse (Prelude.Maybe DomainStatus)
describeDomainResponse_status = Lens.lens (\DescribeDomainResponse' {status} -> status) (\s@DescribeDomainResponse' {} a -> s {status = a} :: DescribeDomainResponse)

-- | The SSO managed application instance ID.
describeDomainResponse_singleSignOnManagedApplicationInstanceId :: Lens.Lens' DescribeDomainResponse (Prelude.Maybe Prelude.Text)
describeDomainResponse_singleSignOnManagedApplicationInstanceId = Lens.lens (\DescribeDomainResponse' {singleSignOnManagedApplicationInstanceId} -> singleSignOnManagedApplicationInstanceId) (\s@DescribeDomainResponse' {} a -> s {singleSignOnManagedApplicationInstanceId = a} :: DescribeDomainResponse)

-- | The domain\'s authentication mode.
describeDomainResponse_authMode :: Lens.Lens' DescribeDomainResponse (Prelude.Maybe AuthMode)
describeDomainResponse_authMode = Lens.lens (\DescribeDomainResponse' {authMode} -> authMode) (\s@DescribeDomainResponse' {} a -> s {authMode = a} :: DescribeDomainResponse)

-- | The domain\'s Amazon Resource Name (ARN).
describeDomainResponse_domainArn :: Lens.Lens' DescribeDomainResponse (Prelude.Maybe Prelude.Text)
describeDomainResponse_domainArn = Lens.lens (\DescribeDomainResponse' {domainArn} -> domainArn) (\s@DescribeDomainResponse' {} a -> s {domainArn = a} :: DescribeDomainResponse)

-- | The VPC subnets that Studio uses for communication.
describeDomainResponse_subnetIds :: Lens.Lens' DescribeDomainResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeDomainResponse_subnetIds = Lens.lens (\DescribeDomainResponse' {subnetIds} -> subnetIds) (\s@DescribeDomainResponse' {} a -> s {subnetIds = a} :: DescribeDomainResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The domain ID.
describeDomainResponse_domainId :: Lens.Lens' DescribeDomainResponse (Prelude.Maybe Prelude.Text)
describeDomainResponse_domainId = Lens.lens (\DescribeDomainResponse' {domainId} -> domainId) (\s@DescribeDomainResponse' {} a -> s {domainId = a} :: DescribeDomainResponse)

-- | The Amazon Web Services KMS customer managed key used to encrypt the EFS
-- volume attached to the domain.
describeDomainResponse_kmsKeyId :: Lens.Lens' DescribeDomainResponse (Prelude.Maybe Prelude.Text)
describeDomainResponse_kmsKeyId = Lens.lens (\DescribeDomainResponse' {kmsKeyId} -> kmsKeyId) (\s@DescribeDomainResponse' {} a -> s {kmsKeyId = a} :: DescribeDomainResponse)

-- | The domain name.
describeDomainResponse_domainName :: Lens.Lens' DescribeDomainResponse (Prelude.Maybe Prelude.Text)
describeDomainResponse_domainName = Lens.lens (\DescribeDomainResponse' {domainName} -> domainName) (\s@DescribeDomainResponse' {} a -> s {domainName = a} :: DescribeDomainResponse)

-- | Settings which are applied to UserProfiles in this domain if settings
-- are not explicitly specified in a given UserProfile.
describeDomainResponse_defaultUserSettings :: Lens.Lens' DescribeDomainResponse (Prelude.Maybe UserSettings)
describeDomainResponse_defaultUserSettings = Lens.lens (\DescribeDomainResponse' {defaultUserSettings} -> defaultUserSettings) (\s@DescribeDomainResponse' {} a -> s {defaultUserSettings = a} :: DescribeDomainResponse)

-- | The failure reason.
describeDomainResponse_failureReason :: Lens.Lens' DescribeDomainResponse (Prelude.Maybe Prelude.Text)
describeDomainResponse_failureReason = Lens.lens (\DescribeDomainResponse' {failureReason} -> failureReason) (\s@DescribeDomainResponse' {} a -> s {failureReason = a} :: DescribeDomainResponse)

-- | The ID of the Amazon Elastic File System (EFS) managed by this Domain.
describeDomainResponse_homeEfsFileSystemId :: Lens.Lens' DescribeDomainResponse (Prelude.Maybe Prelude.Text)
describeDomainResponse_homeEfsFileSystemId = Lens.lens (\DescribeDomainResponse' {homeEfsFileSystemId} -> homeEfsFileSystemId) (\s@DescribeDomainResponse' {} a -> s {homeEfsFileSystemId = a} :: DescribeDomainResponse)

-- | The last modified time.
describeDomainResponse_lastModifiedTime :: Lens.Lens' DescribeDomainResponse (Prelude.Maybe Prelude.UTCTime)
describeDomainResponse_lastModifiedTime = Lens.lens (\DescribeDomainResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeDomainResponse' {} a -> s {lastModifiedTime = a} :: DescribeDomainResponse) Prelude.. Lens.mapping Core._Time

-- | The domain\'s URL.
describeDomainResponse_url :: Lens.Lens' DescribeDomainResponse (Prelude.Maybe Prelude.Text)
describeDomainResponse_url = Lens.lens (\DescribeDomainResponse' {url} -> url) (\s@DescribeDomainResponse' {} a -> s {url = a} :: DescribeDomainResponse)

-- | This member is deprecated and replaced with @KmsKeyId@.
describeDomainResponse_homeEfsFileSystemKmsKeyId :: Lens.Lens' DescribeDomainResponse (Prelude.Maybe Prelude.Text)
describeDomainResponse_homeEfsFileSystemKmsKeyId = Lens.lens (\DescribeDomainResponse' {homeEfsFileSystemKmsKeyId} -> homeEfsFileSystemKmsKeyId) (\s@DescribeDomainResponse' {} a -> s {homeEfsFileSystemKmsKeyId = a} :: DescribeDomainResponse)

-- | Specifies the VPC used for non-EFS traffic. The default value is
-- @PublicInternetOnly@.
--
-- -   @PublicInternetOnly@ - Non-EFS traffic is through a VPC managed by
--     Amazon SageMaker, which allows direct internet access
--
-- -   @VpcOnly@ - All Studio traffic is through the specified VPC and
--     subnets
describeDomainResponse_appNetworkAccessType :: Lens.Lens' DescribeDomainResponse (Prelude.Maybe AppNetworkAccessType)
describeDomainResponse_appNetworkAccessType = Lens.lens (\DescribeDomainResponse' {appNetworkAccessType} -> appNetworkAccessType) (\s@DescribeDomainResponse' {} a -> s {appNetworkAccessType = a} :: DescribeDomainResponse)

-- | The ID of the Amazon Virtual Private Cloud (VPC) that Studio uses for
-- communication.
describeDomainResponse_vpcId :: Lens.Lens' DescribeDomainResponse (Prelude.Maybe Prelude.Text)
describeDomainResponse_vpcId = Lens.lens (\DescribeDomainResponse' {vpcId} -> vpcId) (\s@DescribeDomainResponse' {} a -> s {vpcId = a} :: DescribeDomainResponse)

-- | The response's http status code.
describeDomainResponse_httpStatus :: Lens.Lens' DescribeDomainResponse Prelude.Int
describeDomainResponse_httpStatus = Lens.lens (\DescribeDomainResponse' {httpStatus} -> httpStatus) (\s@DescribeDomainResponse' {} a -> s {httpStatus = a} :: DescribeDomainResponse)

instance Prelude.NFData DescribeDomainResponse
