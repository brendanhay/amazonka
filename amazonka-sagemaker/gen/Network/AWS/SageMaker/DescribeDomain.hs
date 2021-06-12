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
    describeDomainResponse_status,
    describeDomainResponse_creationTime,
    describeDomainResponse_singleSignOnManagedApplicationInstanceId,
    describeDomainResponse_authMode,
    describeDomainResponse_subnetIds,
    describeDomainResponse_domainId,
    describeDomainResponse_domainArn,
    describeDomainResponse_kmsKeyId,
    describeDomainResponse_domainName,
    describeDomainResponse_defaultUserSettings,
    describeDomainResponse_failureReason,
    describeDomainResponse_homeEfsFileSystemId,
    describeDomainResponse_lastModifiedTime,
    describeDomainResponse_appNetworkAccessType,
    describeDomainResponse_homeEfsFileSystemKmsKeyId,
    describeDomainResponse_url,
    describeDomainResponse_vpcId,
    describeDomainResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeDomain' smart constructor.
data DescribeDomain = DescribeDomain'
  { -- | The domain ID.
    domainId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribeDomain
newDescribeDomain pDomainId_ =
  DescribeDomain' {domainId = pDomainId_}

-- | The domain ID.
describeDomain_domainId :: Lens.Lens' DescribeDomain Core.Text
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
            Core.<$> (x Core..?> "Status")
            Core.<*> (x Core..?> "CreationTime")
            Core.<*> ( x
                         Core..?> "SingleSignOnManagedApplicationInstanceId"
                     )
            Core.<*> (x Core..?> "AuthMode")
            Core.<*> (x Core..?> "SubnetIds")
            Core.<*> (x Core..?> "DomainId")
            Core.<*> (x Core..?> "DomainArn")
            Core.<*> (x Core..?> "KmsKeyId")
            Core.<*> (x Core..?> "DomainName")
            Core.<*> (x Core..?> "DefaultUserSettings")
            Core.<*> (x Core..?> "FailureReason")
            Core.<*> (x Core..?> "HomeEfsFileSystemId")
            Core.<*> (x Core..?> "LastModifiedTime")
            Core.<*> (x Core..?> "AppNetworkAccessType")
            Core.<*> (x Core..?> "HomeEfsFileSystemKmsKeyId")
            Core.<*> (x Core..?> "Url")
            Core.<*> (x Core..?> "VpcId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeDomain

instance Core.NFData DescribeDomain

instance Core.ToHeaders DescribeDomain where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DescribeDomain" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeDomain where
  toJSON DescribeDomain' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("DomainId" Core..= domainId)]
      )

instance Core.ToPath DescribeDomain where
  toPath = Core.const "/"

instance Core.ToQuery DescribeDomain where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeDomainResponse' smart constructor.
data DescribeDomainResponse = DescribeDomainResponse'
  { -- | The status.
    status :: Core.Maybe DomainStatus,
    -- | The creation time.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The SSO managed application instance ID.
    singleSignOnManagedApplicationInstanceId :: Core.Maybe Core.Text,
    -- | The domain\'s authentication mode.
    authMode :: Core.Maybe AuthMode,
    -- | The VPC subnets that Studio uses for communication.
    subnetIds :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The domain ID.
    domainId :: Core.Maybe Core.Text,
    -- | The domain\'s Amazon Resource Name (ARN).
    domainArn :: Core.Maybe Core.Text,
    -- | The AWS KMS customer managed CMK used to encrypt the EFS volume attached
    -- to the domain.
    kmsKeyId :: Core.Maybe Core.Text,
    -- | The domain name.
    domainName :: Core.Maybe Core.Text,
    -- | Settings which are applied to all UserProfiles in this domain, if
    -- settings are not explicitly specified in a given UserProfile.
    defaultUserSettings :: Core.Maybe UserSettings,
    -- | The failure reason.
    failureReason :: Core.Maybe Core.Text,
    -- | The ID of the Amazon Elastic File System (EFS) managed by this Domain.
    homeEfsFileSystemId :: Core.Maybe Core.Text,
    -- | The last modified time.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | Specifies the VPC used for non-EFS traffic. The default value is
    -- @PublicInternetOnly@.
    --
    -- -   @PublicInternetOnly@ - Non-EFS traffic is through a VPC managed by
    --     Amazon SageMaker, which allows direct internet access
    --
    -- -   @VpcOnly@ - All Studio traffic is through the specified VPC and
    --     subnets
    appNetworkAccessType :: Core.Maybe AppNetworkAccessType,
    -- | This member is deprecated and replaced with @KmsKeyId@.
    homeEfsFileSystemKmsKeyId :: Core.Maybe Core.Text,
    -- | The domain\'s URL.
    url :: Core.Maybe Core.Text,
    -- | The ID of the Amazon Virtual Private Cloud (VPC) that Studio uses for
    -- communication.
    vpcId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'describeDomainResponse_status' - The status.
--
-- 'creationTime', 'describeDomainResponse_creationTime' - The creation time.
--
-- 'singleSignOnManagedApplicationInstanceId', 'describeDomainResponse_singleSignOnManagedApplicationInstanceId' - The SSO managed application instance ID.
--
-- 'authMode', 'describeDomainResponse_authMode' - The domain\'s authentication mode.
--
-- 'subnetIds', 'describeDomainResponse_subnetIds' - The VPC subnets that Studio uses for communication.
--
-- 'domainId', 'describeDomainResponse_domainId' - The domain ID.
--
-- 'domainArn', 'describeDomainResponse_domainArn' - The domain\'s Amazon Resource Name (ARN).
--
-- 'kmsKeyId', 'describeDomainResponse_kmsKeyId' - The AWS KMS customer managed CMK used to encrypt the EFS volume attached
-- to the domain.
--
-- 'domainName', 'describeDomainResponse_domainName' - The domain name.
--
-- 'defaultUserSettings', 'describeDomainResponse_defaultUserSettings' - Settings which are applied to all UserProfiles in this domain, if
-- settings are not explicitly specified in a given UserProfile.
--
-- 'failureReason', 'describeDomainResponse_failureReason' - The failure reason.
--
-- 'homeEfsFileSystemId', 'describeDomainResponse_homeEfsFileSystemId' - The ID of the Amazon Elastic File System (EFS) managed by this Domain.
--
-- 'lastModifiedTime', 'describeDomainResponse_lastModifiedTime' - The last modified time.
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
-- 'homeEfsFileSystemKmsKeyId', 'describeDomainResponse_homeEfsFileSystemKmsKeyId' - This member is deprecated and replaced with @KmsKeyId@.
--
-- 'url', 'describeDomainResponse_url' - The domain\'s URL.
--
-- 'vpcId', 'describeDomainResponse_vpcId' - The ID of the Amazon Virtual Private Cloud (VPC) that Studio uses for
-- communication.
--
-- 'httpStatus', 'describeDomainResponse_httpStatus' - The response's http status code.
newDescribeDomainResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeDomainResponse
newDescribeDomainResponse pHttpStatus_ =
  DescribeDomainResponse'
    { status = Core.Nothing,
      creationTime = Core.Nothing,
      singleSignOnManagedApplicationInstanceId =
        Core.Nothing,
      authMode = Core.Nothing,
      subnetIds = Core.Nothing,
      domainId = Core.Nothing,
      domainArn = Core.Nothing,
      kmsKeyId = Core.Nothing,
      domainName = Core.Nothing,
      defaultUserSettings = Core.Nothing,
      failureReason = Core.Nothing,
      homeEfsFileSystemId = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      appNetworkAccessType = Core.Nothing,
      homeEfsFileSystemKmsKeyId = Core.Nothing,
      url = Core.Nothing,
      vpcId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status.
describeDomainResponse_status :: Lens.Lens' DescribeDomainResponse (Core.Maybe DomainStatus)
describeDomainResponse_status = Lens.lens (\DescribeDomainResponse' {status} -> status) (\s@DescribeDomainResponse' {} a -> s {status = a} :: DescribeDomainResponse)

-- | The creation time.
describeDomainResponse_creationTime :: Lens.Lens' DescribeDomainResponse (Core.Maybe Core.UTCTime)
describeDomainResponse_creationTime = Lens.lens (\DescribeDomainResponse' {creationTime} -> creationTime) (\s@DescribeDomainResponse' {} a -> s {creationTime = a} :: DescribeDomainResponse) Core.. Lens.mapping Core._Time

-- | The SSO managed application instance ID.
describeDomainResponse_singleSignOnManagedApplicationInstanceId :: Lens.Lens' DescribeDomainResponse (Core.Maybe Core.Text)
describeDomainResponse_singleSignOnManagedApplicationInstanceId = Lens.lens (\DescribeDomainResponse' {singleSignOnManagedApplicationInstanceId} -> singleSignOnManagedApplicationInstanceId) (\s@DescribeDomainResponse' {} a -> s {singleSignOnManagedApplicationInstanceId = a} :: DescribeDomainResponse)

-- | The domain\'s authentication mode.
describeDomainResponse_authMode :: Lens.Lens' DescribeDomainResponse (Core.Maybe AuthMode)
describeDomainResponse_authMode = Lens.lens (\DescribeDomainResponse' {authMode} -> authMode) (\s@DescribeDomainResponse' {} a -> s {authMode = a} :: DescribeDomainResponse)

-- | The VPC subnets that Studio uses for communication.
describeDomainResponse_subnetIds :: Lens.Lens' DescribeDomainResponse (Core.Maybe (Core.NonEmpty Core.Text))
describeDomainResponse_subnetIds = Lens.lens (\DescribeDomainResponse' {subnetIds} -> subnetIds) (\s@DescribeDomainResponse' {} a -> s {subnetIds = a} :: DescribeDomainResponse) Core.. Lens.mapping Lens._Coerce

-- | The domain ID.
describeDomainResponse_domainId :: Lens.Lens' DescribeDomainResponse (Core.Maybe Core.Text)
describeDomainResponse_domainId = Lens.lens (\DescribeDomainResponse' {domainId} -> domainId) (\s@DescribeDomainResponse' {} a -> s {domainId = a} :: DescribeDomainResponse)

-- | The domain\'s Amazon Resource Name (ARN).
describeDomainResponse_domainArn :: Lens.Lens' DescribeDomainResponse (Core.Maybe Core.Text)
describeDomainResponse_domainArn = Lens.lens (\DescribeDomainResponse' {domainArn} -> domainArn) (\s@DescribeDomainResponse' {} a -> s {domainArn = a} :: DescribeDomainResponse)

-- | The AWS KMS customer managed CMK used to encrypt the EFS volume attached
-- to the domain.
describeDomainResponse_kmsKeyId :: Lens.Lens' DescribeDomainResponse (Core.Maybe Core.Text)
describeDomainResponse_kmsKeyId = Lens.lens (\DescribeDomainResponse' {kmsKeyId} -> kmsKeyId) (\s@DescribeDomainResponse' {} a -> s {kmsKeyId = a} :: DescribeDomainResponse)

-- | The domain name.
describeDomainResponse_domainName :: Lens.Lens' DescribeDomainResponse (Core.Maybe Core.Text)
describeDomainResponse_domainName = Lens.lens (\DescribeDomainResponse' {domainName} -> domainName) (\s@DescribeDomainResponse' {} a -> s {domainName = a} :: DescribeDomainResponse)

-- | Settings which are applied to all UserProfiles in this domain, if
-- settings are not explicitly specified in a given UserProfile.
describeDomainResponse_defaultUserSettings :: Lens.Lens' DescribeDomainResponse (Core.Maybe UserSettings)
describeDomainResponse_defaultUserSettings = Lens.lens (\DescribeDomainResponse' {defaultUserSettings} -> defaultUserSettings) (\s@DescribeDomainResponse' {} a -> s {defaultUserSettings = a} :: DescribeDomainResponse)

-- | The failure reason.
describeDomainResponse_failureReason :: Lens.Lens' DescribeDomainResponse (Core.Maybe Core.Text)
describeDomainResponse_failureReason = Lens.lens (\DescribeDomainResponse' {failureReason} -> failureReason) (\s@DescribeDomainResponse' {} a -> s {failureReason = a} :: DescribeDomainResponse)

-- | The ID of the Amazon Elastic File System (EFS) managed by this Domain.
describeDomainResponse_homeEfsFileSystemId :: Lens.Lens' DescribeDomainResponse (Core.Maybe Core.Text)
describeDomainResponse_homeEfsFileSystemId = Lens.lens (\DescribeDomainResponse' {homeEfsFileSystemId} -> homeEfsFileSystemId) (\s@DescribeDomainResponse' {} a -> s {homeEfsFileSystemId = a} :: DescribeDomainResponse)

-- | The last modified time.
describeDomainResponse_lastModifiedTime :: Lens.Lens' DescribeDomainResponse (Core.Maybe Core.UTCTime)
describeDomainResponse_lastModifiedTime = Lens.lens (\DescribeDomainResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeDomainResponse' {} a -> s {lastModifiedTime = a} :: DescribeDomainResponse) Core.. Lens.mapping Core._Time

-- | Specifies the VPC used for non-EFS traffic. The default value is
-- @PublicInternetOnly@.
--
-- -   @PublicInternetOnly@ - Non-EFS traffic is through a VPC managed by
--     Amazon SageMaker, which allows direct internet access
--
-- -   @VpcOnly@ - All Studio traffic is through the specified VPC and
--     subnets
describeDomainResponse_appNetworkAccessType :: Lens.Lens' DescribeDomainResponse (Core.Maybe AppNetworkAccessType)
describeDomainResponse_appNetworkAccessType = Lens.lens (\DescribeDomainResponse' {appNetworkAccessType} -> appNetworkAccessType) (\s@DescribeDomainResponse' {} a -> s {appNetworkAccessType = a} :: DescribeDomainResponse)

-- | This member is deprecated and replaced with @KmsKeyId@.
describeDomainResponse_homeEfsFileSystemKmsKeyId :: Lens.Lens' DescribeDomainResponse (Core.Maybe Core.Text)
describeDomainResponse_homeEfsFileSystemKmsKeyId = Lens.lens (\DescribeDomainResponse' {homeEfsFileSystemKmsKeyId} -> homeEfsFileSystemKmsKeyId) (\s@DescribeDomainResponse' {} a -> s {homeEfsFileSystemKmsKeyId = a} :: DescribeDomainResponse)

-- | The domain\'s URL.
describeDomainResponse_url :: Lens.Lens' DescribeDomainResponse (Core.Maybe Core.Text)
describeDomainResponse_url = Lens.lens (\DescribeDomainResponse' {url} -> url) (\s@DescribeDomainResponse' {} a -> s {url = a} :: DescribeDomainResponse)

-- | The ID of the Amazon Virtual Private Cloud (VPC) that Studio uses for
-- communication.
describeDomainResponse_vpcId :: Lens.Lens' DescribeDomainResponse (Core.Maybe Core.Text)
describeDomainResponse_vpcId = Lens.lens (\DescribeDomainResponse' {vpcId} -> vpcId) (\s@DescribeDomainResponse' {} a -> s {vpcId = a} :: DescribeDomainResponse)

-- | The response's http status code.
describeDomainResponse_httpStatus :: Lens.Lens' DescribeDomainResponse Core.Int
describeDomainResponse_httpStatus = Lens.lens (\DescribeDomainResponse' {httpStatus} -> httpStatus) (\s@DescribeDomainResponse' {} a -> s {httpStatus = a} :: DescribeDomainResponse)

instance Core.NFData DescribeDomainResponse
