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
-- Module      : Network.AWS.SageMaker.CreateDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @Domain@ used by Amazon SageMaker Studio. A domain consists of
-- an associated Amazon Elastic File System (EFS) volume, a list of
-- authorized users, and a variety of security, application, policy, and
-- Amazon Virtual Private Cloud (VPC) configurations. An AWS account is
-- limited to one domain per region. Users within a domain can share
-- notebook files and other artifacts with each other.
--
-- __EFS storage__
--
-- When a domain is created, an EFS volume is created for use by all of the
-- users within the domain. Each user receives a private home directory
-- within the EFS volume for notebooks, Git repositories, and data files.
--
-- SageMaker uses the AWS Key Management Service (AWS KMS) to encrypt the
-- EFS volume attached to the domain with an AWS managed customer master
-- key (CMK) by default. For more control, you can specify a customer
-- managed CMK. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/encryption-at-rest.html Protect Data at Rest Using Encryption>.
--
-- __VPC configuration__
--
-- All SageMaker Studio traffic between the domain and the EFS volume is
-- through the specified VPC and subnets. For other Studio traffic, you can
-- specify the @AppNetworkAccessType@ parameter. @AppNetworkAccessType@
-- corresponds to the network access type that you choose when you onboard
-- to Studio. The following options are available:
--
-- -   @PublicInternetOnly@ - Non-EFS traffic goes through a VPC managed by
--     Amazon SageMaker, which allows internet access. This is the default
--     value.
--
-- -   @VpcOnly@ - All Studio traffic is through the specified VPC and
--     subnets. Internet access is disabled by default. To allow internet
--     access, you must specify a NAT gateway.
--
--     When internet access is disabled, you won\'t be able to run a Studio
--     notebook or to train or host models unless your VPC has an interface
--     endpoint to the SageMaker API and runtime or a NAT gateway and your
--     security groups allow outbound connections.
--
-- For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/studio-notebooks-and-internet-access.html Connect SageMaker Studio Notebooks to Resources in a VPC>.
module Network.AWS.SageMaker.CreateDomain
  ( -- * Creating a Request
    CreateDomain (..),
    newCreateDomain,

    -- * Request Lenses
    createDomain_kmsKeyId,
    createDomain_tags,
    createDomain_appNetworkAccessType,
    createDomain_homeEfsFileSystemKmsKeyId,
    createDomain_domainName,
    createDomain_authMode,
    createDomain_defaultUserSettings,
    createDomain_subnetIds,
    createDomain_vpcId,

    -- * Destructuring the Response
    CreateDomainResponse (..),
    newCreateDomainResponse,

    -- * Response Lenses
    createDomainResponse_domainArn,
    createDomainResponse_url,
    createDomainResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateDomain' smart constructor.
data CreateDomain = CreateDomain'
  { -- | SageMaker uses AWS KMS to encrypt the EFS volume attached to the domain
    -- with an AWS managed customer master key (CMK) by default. For more
    -- control, specify a customer managed CMK.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Tags to associated with the Domain. Each tag consists of a key and an
    -- optional value. Tag keys must be unique per resource. Tags are
    -- searchable using the Search API.
    tags :: Prelude.Maybe [Tag],
    -- | Specifies the VPC used for non-EFS traffic. The default value is
    -- @PublicInternetOnly@.
    --
    -- -   @PublicInternetOnly@ - Non-EFS traffic is through a VPC managed by
    --     Amazon SageMaker, which allows direct internet access
    --
    -- -   @VpcOnly@ - All Studio traffic is through the specified VPC and
    --     subnets
    appNetworkAccessType :: Prelude.Maybe AppNetworkAccessType,
    -- | This member is deprecated and replaced with @KmsKeyId@.
    homeEfsFileSystemKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | A name for the domain.
    domainName :: Prelude.Text,
    -- | The mode of authentication that members use to access the domain.
    authMode :: AuthMode,
    -- | The default user settings.
    defaultUserSettings :: UserSettings,
    -- | The VPC subnets that Studio uses for communication.
    subnetIds :: Prelude.NonEmpty Prelude.Text,
    -- | The ID of the Amazon Virtual Private Cloud (VPC) that Studio uses for
    -- communication.
    vpcId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'createDomain_kmsKeyId' - SageMaker uses AWS KMS to encrypt the EFS volume attached to the domain
-- with an AWS managed customer master key (CMK) by default. For more
-- control, specify a customer managed CMK.
--
-- 'tags', 'createDomain_tags' - Tags to associated with the Domain. Each tag consists of a key and an
-- optional value. Tag keys must be unique per resource. Tags are
-- searchable using the Search API.
--
-- 'appNetworkAccessType', 'createDomain_appNetworkAccessType' - Specifies the VPC used for non-EFS traffic. The default value is
-- @PublicInternetOnly@.
--
-- -   @PublicInternetOnly@ - Non-EFS traffic is through a VPC managed by
--     Amazon SageMaker, which allows direct internet access
--
-- -   @VpcOnly@ - All Studio traffic is through the specified VPC and
--     subnets
--
-- 'homeEfsFileSystemKmsKeyId', 'createDomain_homeEfsFileSystemKmsKeyId' - This member is deprecated and replaced with @KmsKeyId@.
--
-- 'domainName', 'createDomain_domainName' - A name for the domain.
--
-- 'authMode', 'createDomain_authMode' - The mode of authentication that members use to access the domain.
--
-- 'defaultUserSettings', 'createDomain_defaultUserSettings' - The default user settings.
--
-- 'subnetIds', 'createDomain_subnetIds' - The VPC subnets that Studio uses for communication.
--
-- 'vpcId', 'createDomain_vpcId' - The ID of the Amazon Virtual Private Cloud (VPC) that Studio uses for
-- communication.
newCreateDomain ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'authMode'
  AuthMode ->
  -- | 'defaultUserSettings'
  UserSettings ->
  -- | 'subnetIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'vpcId'
  Prelude.Text ->
  CreateDomain
newCreateDomain
  pDomainName_
  pAuthMode_
  pDefaultUserSettings_
  pSubnetIds_
  pVpcId_ =
    CreateDomain'
      { kmsKeyId = Prelude.Nothing,
        tags = Prelude.Nothing,
        appNetworkAccessType = Prelude.Nothing,
        homeEfsFileSystemKmsKeyId = Prelude.Nothing,
        domainName = pDomainName_,
        authMode = pAuthMode_,
        defaultUserSettings = pDefaultUserSettings_,
        subnetIds = Lens._Coerce Lens.# pSubnetIds_,
        vpcId = pVpcId_
      }

-- | SageMaker uses AWS KMS to encrypt the EFS volume attached to the domain
-- with an AWS managed customer master key (CMK) by default. For more
-- control, specify a customer managed CMK.
createDomain_kmsKeyId :: Lens.Lens' CreateDomain (Prelude.Maybe Prelude.Text)
createDomain_kmsKeyId = Lens.lens (\CreateDomain' {kmsKeyId} -> kmsKeyId) (\s@CreateDomain' {} a -> s {kmsKeyId = a} :: CreateDomain)

-- | Tags to associated with the Domain. Each tag consists of a key and an
-- optional value. Tag keys must be unique per resource. Tags are
-- searchable using the Search API.
createDomain_tags :: Lens.Lens' CreateDomain (Prelude.Maybe [Tag])
createDomain_tags = Lens.lens (\CreateDomain' {tags} -> tags) (\s@CreateDomain' {} a -> s {tags = a} :: CreateDomain) Prelude.. Lens.mapping Lens._Coerce

-- | Specifies the VPC used for non-EFS traffic. The default value is
-- @PublicInternetOnly@.
--
-- -   @PublicInternetOnly@ - Non-EFS traffic is through a VPC managed by
--     Amazon SageMaker, which allows direct internet access
--
-- -   @VpcOnly@ - All Studio traffic is through the specified VPC and
--     subnets
createDomain_appNetworkAccessType :: Lens.Lens' CreateDomain (Prelude.Maybe AppNetworkAccessType)
createDomain_appNetworkAccessType = Lens.lens (\CreateDomain' {appNetworkAccessType} -> appNetworkAccessType) (\s@CreateDomain' {} a -> s {appNetworkAccessType = a} :: CreateDomain)

-- | This member is deprecated and replaced with @KmsKeyId@.
createDomain_homeEfsFileSystemKmsKeyId :: Lens.Lens' CreateDomain (Prelude.Maybe Prelude.Text)
createDomain_homeEfsFileSystemKmsKeyId = Lens.lens (\CreateDomain' {homeEfsFileSystemKmsKeyId} -> homeEfsFileSystemKmsKeyId) (\s@CreateDomain' {} a -> s {homeEfsFileSystemKmsKeyId = a} :: CreateDomain)

-- | A name for the domain.
createDomain_domainName :: Lens.Lens' CreateDomain Prelude.Text
createDomain_domainName = Lens.lens (\CreateDomain' {domainName} -> domainName) (\s@CreateDomain' {} a -> s {domainName = a} :: CreateDomain)

-- | The mode of authentication that members use to access the domain.
createDomain_authMode :: Lens.Lens' CreateDomain AuthMode
createDomain_authMode = Lens.lens (\CreateDomain' {authMode} -> authMode) (\s@CreateDomain' {} a -> s {authMode = a} :: CreateDomain)

-- | The default user settings.
createDomain_defaultUserSettings :: Lens.Lens' CreateDomain UserSettings
createDomain_defaultUserSettings = Lens.lens (\CreateDomain' {defaultUserSettings} -> defaultUserSettings) (\s@CreateDomain' {} a -> s {defaultUserSettings = a} :: CreateDomain)

-- | The VPC subnets that Studio uses for communication.
createDomain_subnetIds :: Lens.Lens' CreateDomain (Prelude.NonEmpty Prelude.Text)
createDomain_subnetIds = Lens.lens (\CreateDomain' {subnetIds} -> subnetIds) (\s@CreateDomain' {} a -> s {subnetIds = a} :: CreateDomain) Prelude.. Lens._Coerce

-- | The ID of the Amazon Virtual Private Cloud (VPC) that Studio uses for
-- communication.
createDomain_vpcId :: Lens.Lens' CreateDomain Prelude.Text
createDomain_vpcId = Lens.lens (\CreateDomain' {vpcId} -> vpcId) (\s@CreateDomain' {} a -> s {vpcId = a} :: CreateDomain)

instance Core.AWSRequest CreateDomain where
  type AWSResponse CreateDomain = CreateDomainResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDomainResponse'
            Prelude.<$> (x Core..?> "DomainArn")
            Prelude.<*> (x Core..?> "Url")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDomain

instance Prelude.NFData CreateDomain

instance Core.ToHeaders CreateDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.CreateDomain" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateDomain where
  toJSON CreateDomain' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("KmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("Tags" Core..=) Prelude.<$> tags,
            ("AppNetworkAccessType" Core..=)
              Prelude.<$> appNetworkAccessType,
            ("HomeEfsFileSystemKmsKeyId" Core..=)
              Prelude.<$> homeEfsFileSystemKmsKeyId,
            Prelude.Just ("DomainName" Core..= domainName),
            Prelude.Just ("AuthMode" Core..= authMode),
            Prelude.Just
              ("DefaultUserSettings" Core..= defaultUserSettings),
            Prelude.Just ("SubnetIds" Core..= subnetIds),
            Prelude.Just ("VpcId" Core..= vpcId)
          ]
      )

instance Core.ToPath CreateDomain where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateDomain where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDomainResponse' smart constructor.
data CreateDomainResponse = CreateDomainResponse'
  { -- | The Amazon Resource Name (ARN) of the created domain.
    domainArn :: Prelude.Maybe Prelude.Text,
    -- | The URL to the created domain.
    url :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainArn', 'createDomainResponse_domainArn' - The Amazon Resource Name (ARN) of the created domain.
--
-- 'url', 'createDomainResponse_url' - The URL to the created domain.
--
-- 'httpStatus', 'createDomainResponse_httpStatus' - The response's http status code.
newCreateDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDomainResponse
newCreateDomainResponse pHttpStatus_ =
  CreateDomainResponse'
    { domainArn = Prelude.Nothing,
      url = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the created domain.
createDomainResponse_domainArn :: Lens.Lens' CreateDomainResponse (Prelude.Maybe Prelude.Text)
createDomainResponse_domainArn = Lens.lens (\CreateDomainResponse' {domainArn} -> domainArn) (\s@CreateDomainResponse' {} a -> s {domainArn = a} :: CreateDomainResponse)

-- | The URL to the created domain.
createDomainResponse_url :: Lens.Lens' CreateDomainResponse (Prelude.Maybe Prelude.Text)
createDomainResponse_url = Lens.lens (\CreateDomainResponse' {url} -> url) (\s@CreateDomainResponse' {} a -> s {url = a} :: CreateDomainResponse)

-- | The response's http status code.
createDomainResponse_httpStatus :: Lens.Lens' CreateDomainResponse Prelude.Int
createDomainResponse_httpStatus = Lens.lens (\CreateDomainResponse' {httpStatus} -> httpStatus) (\s@CreateDomainResponse' {} a -> s {httpStatus = a} :: CreateDomainResponse)

instance Prelude.NFData CreateDomainResponse
