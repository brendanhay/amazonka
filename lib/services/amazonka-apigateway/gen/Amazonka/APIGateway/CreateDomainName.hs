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
-- Module      : Amazonka.APIGateway.CreateDomainName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new domain name.
module Amazonka.APIGateway.CreateDomainName
  ( -- * Creating a Request
    CreateDomainName (..),
    newCreateDomainName,

    -- * Request Lenses
    createDomainName_tags,
    createDomainName_mutualTlsAuthentication,
    createDomainName_regionalCertificateName,
    createDomainName_ownershipVerificationCertificateArn,
    createDomainName_regionalCertificateArn,
    createDomainName_certificateName,
    createDomainName_certificateBody,
    createDomainName_certificateArn,
    createDomainName_certificatePrivateKey,
    createDomainName_certificateChain,
    createDomainName_endpointConfiguration,
    createDomainName_securityPolicy,
    createDomainName_domainName,

    -- * Destructuring the Response
    DomainName (..),
    newDomainName,

    -- * Response Lenses
    domainName_tags,
    domainName_domainNameStatus,
    domainName_mutualTlsAuthentication,
    domainName_regionalCertificateName,
    domainName_regionalDomainName,
    domainName_domainName,
    domainName_ownershipVerificationCertificateArn,
    domainName_regionalCertificateArn,
    domainName_regionalHostedZoneId,
    domainName_certificateName,
    domainName_domainNameStatusMessage,
    domainName_certificateArn,
    domainName_distributionDomainName,
    domainName_certificateUploadDate,
    domainName_endpointConfiguration,
    domainName_distributionHostedZoneId,
    domainName_securityPolicy,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to create a new domain name.
--
-- /See:/ 'newCreateDomainName' smart constructor.
data CreateDomainName = CreateDomainName'
  { -- | The key-value map of strings. The valid character set is
    -- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
    -- start with @aws:@. The tag value can be up to 256 characters.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    mutualTlsAuthentication :: Prelude.Maybe MutualTlsAuthenticationInput,
    -- | The user-friendly name of the certificate that will be used by regional
    -- endpoint for this domain name.
    regionalCertificateName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the public certificate issued by ACM to validate ownership of
    -- your custom domain. Only required when configuring mutual TLS and using
    -- an ACM imported or private CA certificate ARN as the
    -- regionalCertificateArn.
    ownershipVerificationCertificateArn :: Prelude.Maybe Prelude.Text,
    -- | The reference to an AWS-managed certificate that will be used by
    -- regional endpoint for this domain name. AWS Certificate Manager is the
    -- only supported source.
    regionalCertificateArn :: Prelude.Maybe Prelude.Text,
    -- | The user-friendly name of the certificate that will be used by
    -- edge-optimized endpoint for this domain name.
    certificateName :: Prelude.Maybe Prelude.Text,
    -- | [Deprecated] The body of the server certificate that will be used by
    -- edge-optimized endpoint for this domain name provided by your
    -- certificate authority.
    certificateBody :: Prelude.Maybe Prelude.Text,
    -- | The reference to an AWS-managed certificate that will be used by
    -- edge-optimized endpoint for this domain name. AWS Certificate Manager is
    -- the only supported source.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | [Deprecated] Your edge-optimized endpoint\'s domain name certificate\'s
    -- private key.
    certificatePrivateKey :: Prelude.Maybe Prelude.Text,
    -- | [Deprecated] The intermediate certificates and optionally the root
    -- certificate, one after the other without any blank lines, used by an
    -- edge-optimized endpoint for this domain name. If you include the root
    -- certificate, your certificate chain must start with intermediate
    -- certificates and end with the root certificate. Use the intermediate
    -- certificates that were provided by your certificate authority. Do not
    -- include any intermediaries that are not in the chain of trust path.
    certificateChain :: Prelude.Maybe Prelude.Text,
    -- | The endpoint configuration of this DomainName showing the endpoint types
    -- of the domain name.
    endpointConfiguration :: Prelude.Maybe EndpointConfiguration,
    -- | The Transport Layer Security (TLS) version + cipher suite for this
    -- DomainName. The valid values are @TLS_1_0@ and @TLS_1_2@.
    securityPolicy :: Prelude.Maybe SecurityPolicy,
    -- | The name of the DomainName resource.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDomainName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createDomainName_tags' - The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
--
-- 'mutualTlsAuthentication', 'createDomainName_mutualTlsAuthentication' - Undocumented member.
--
-- 'regionalCertificateName', 'createDomainName_regionalCertificateName' - The user-friendly name of the certificate that will be used by regional
-- endpoint for this domain name.
--
-- 'ownershipVerificationCertificateArn', 'createDomainName_ownershipVerificationCertificateArn' - The ARN of the public certificate issued by ACM to validate ownership of
-- your custom domain. Only required when configuring mutual TLS and using
-- an ACM imported or private CA certificate ARN as the
-- regionalCertificateArn.
--
-- 'regionalCertificateArn', 'createDomainName_regionalCertificateArn' - The reference to an AWS-managed certificate that will be used by
-- regional endpoint for this domain name. AWS Certificate Manager is the
-- only supported source.
--
-- 'certificateName', 'createDomainName_certificateName' - The user-friendly name of the certificate that will be used by
-- edge-optimized endpoint for this domain name.
--
-- 'certificateBody', 'createDomainName_certificateBody' - [Deprecated] The body of the server certificate that will be used by
-- edge-optimized endpoint for this domain name provided by your
-- certificate authority.
--
-- 'certificateArn', 'createDomainName_certificateArn' - The reference to an AWS-managed certificate that will be used by
-- edge-optimized endpoint for this domain name. AWS Certificate Manager is
-- the only supported source.
--
-- 'certificatePrivateKey', 'createDomainName_certificatePrivateKey' - [Deprecated] Your edge-optimized endpoint\'s domain name certificate\'s
-- private key.
--
-- 'certificateChain', 'createDomainName_certificateChain' - [Deprecated] The intermediate certificates and optionally the root
-- certificate, one after the other without any blank lines, used by an
-- edge-optimized endpoint for this domain name. If you include the root
-- certificate, your certificate chain must start with intermediate
-- certificates and end with the root certificate. Use the intermediate
-- certificates that were provided by your certificate authority. Do not
-- include any intermediaries that are not in the chain of trust path.
--
-- 'endpointConfiguration', 'createDomainName_endpointConfiguration' - The endpoint configuration of this DomainName showing the endpoint types
-- of the domain name.
--
-- 'securityPolicy', 'createDomainName_securityPolicy' - The Transport Layer Security (TLS) version + cipher suite for this
-- DomainName. The valid values are @TLS_1_0@ and @TLS_1_2@.
--
-- 'domainName', 'createDomainName_domainName' - The name of the DomainName resource.
newCreateDomainName ::
  -- | 'domainName'
  Prelude.Text ->
  CreateDomainName
newCreateDomainName pDomainName_ =
  CreateDomainName'
    { tags = Prelude.Nothing,
      mutualTlsAuthentication = Prelude.Nothing,
      regionalCertificateName = Prelude.Nothing,
      ownershipVerificationCertificateArn =
        Prelude.Nothing,
      regionalCertificateArn = Prelude.Nothing,
      certificateName = Prelude.Nothing,
      certificateBody = Prelude.Nothing,
      certificateArn = Prelude.Nothing,
      certificatePrivateKey = Prelude.Nothing,
      certificateChain = Prelude.Nothing,
      endpointConfiguration = Prelude.Nothing,
      securityPolicy = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
createDomainName_tags :: Lens.Lens' CreateDomainName (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDomainName_tags = Lens.lens (\CreateDomainName' {tags} -> tags) (\s@CreateDomainName' {} a -> s {tags = a} :: CreateDomainName) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createDomainName_mutualTlsAuthentication :: Lens.Lens' CreateDomainName (Prelude.Maybe MutualTlsAuthenticationInput)
createDomainName_mutualTlsAuthentication = Lens.lens (\CreateDomainName' {mutualTlsAuthentication} -> mutualTlsAuthentication) (\s@CreateDomainName' {} a -> s {mutualTlsAuthentication = a} :: CreateDomainName)

-- | The user-friendly name of the certificate that will be used by regional
-- endpoint for this domain name.
createDomainName_regionalCertificateName :: Lens.Lens' CreateDomainName (Prelude.Maybe Prelude.Text)
createDomainName_regionalCertificateName = Lens.lens (\CreateDomainName' {regionalCertificateName} -> regionalCertificateName) (\s@CreateDomainName' {} a -> s {regionalCertificateName = a} :: CreateDomainName)

-- | The ARN of the public certificate issued by ACM to validate ownership of
-- your custom domain. Only required when configuring mutual TLS and using
-- an ACM imported or private CA certificate ARN as the
-- regionalCertificateArn.
createDomainName_ownershipVerificationCertificateArn :: Lens.Lens' CreateDomainName (Prelude.Maybe Prelude.Text)
createDomainName_ownershipVerificationCertificateArn = Lens.lens (\CreateDomainName' {ownershipVerificationCertificateArn} -> ownershipVerificationCertificateArn) (\s@CreateDomainName' {} a -> s {ownershipVerificationCertificateArn = a} :: CreateDomainName)

-- | The reference to an AWS-managed certificate that will be used by
-- regional endpoint for this domain name. AWS Certificate Manager is the
-- only supported source.
createDomainName_regionalCertificateArn :: Lens.Lens' CreateDomainName (Prelude.Maybe Prelude.Text)
createDomainName_regionalCertificateArn = Lens.lens (\CreateDomainName' {regionalCertificateArn} -> regionalCertificateArn) (\s@CreateDomainName' {} a -> s {regionalCertificateArn = a} :: CreateDomainName)

-- | The user-friendly name of the certificate that will be used by
-- edge-optimized endpoint for this domain name.
createDomainName_certificateName :: Lens.Lens' CreateDomainName (Prelude.Maybe Prelude.Text)
createDomainName_certificateName = Lens.lens (\CreateDomainName' {certificateName} -> certificateName) (\s@CreateDomainName' {} a -> s {certificateName = a} :: CreateDomainName)

-- | [Deprecated] The body of the server certificate that will be used by
-- edge-optimized endpoint for this domain name provided by your
-- certificate authority.
createDomainName_certificateBody :: Lens.Lens' CreateDomainName (Prelude.Maybe Prelude.Text)
createDomainName_certificateBody = Lens.lens (\CreateDomainName' {certificateBody} -> certificateBody) (\s@CreateDomainName' {} a -> s {certificateBody = a} :: CreateDomainName)

-- | The reference to an AWS-managed certificate that will be used by
-- edge-optimized endpoint for this domain name. AWS Certificate Manager is
-- the only supported source.
createDomainName_certificateArn :: Lens.Lens' CreateDomainName (Prelude.Maybe Prelude.Text)
createDomainName_certificateArn = Lens.lens (\CreateDomainName' {certificateArn} -> certificateArn) (\s@CreateDomainName' {} a -> s {certificateArn = a} :: CreateDomainName)

-- | [Deprecated] Your edge-optimized endpoint\'s domain name certificate\'s
-- private key.
createDomainName_certificatePrivateKey :: Lens.Lens' CreateDomainName (Prelude.Maybe Prelude.Text)
createDomainName_certificatePrivateKey = Lens.lens (\CreateDomainName' {certificatePrivateKey} -> certificatePrivateKey) (\s@CreateDomainName' {} a -> s {certificatePrivateKey = a} :: CreateDomainName)

-- | [Deprecated] The intermediate certificates and optionally the root
-- certificate, one after the other without any blank lines, used by an
-- edge-optimized endpoint for this domain name. If you include the root
-- certificate, your certificate chain must start with intermediate
-- certificates and end with the root certificate. Use the intermediate
-- certificates that were provided by your certificate authority. Do not
-- include any intermediaries that are not in the chain of trust path.
createDomainName_certificateChain :: Lens.Lens' CreateDomainName (Prelude.Maybe Prelude.Text)
createDomainName_certificateChain = Lens.lens (\CreateDomainName' {certificateChain} -> certificateChain) (\s@CreateDomainName' {} a -> s {certificateChain = a} :: CreateDomainName)

-- | The endpoint configuration of this DomainName showing the endpoint types
-- of the domain name.
createDomainName_endpointConfiguration :: Lens.Lens' CreateDomainName (Prelude.Maybe EndpointConfiguration)
createDomainName_endpointConfiguration = Lens.lens (\CreateDomainName' {endpointConfiguration} -> endpointConfiguration) (\s@CreateDomainName' {} a -> s {endpointConfiguration = a} :: CreateDomainName)

-- | The Transport Layer Security (TLS) version + cipher suite for this
-- DomainName. The valid values are @TLS_1_0@ and @TLS_1_2@.
createDomainName_securityPolicy :: Lens.Lens' CreateDomainName (Prelude.Maybe SecurityPolicy)
createDomainName_securityPolicy = Lens.lens (\CreateDomainName' {securityPolicy} -> securityPolicy) (\s@CreateDomainName' {} a -> s {securityPolicy = a} :: CreateDomainName)

-- | The name of the DomainName resource.
createDomainName_domainName :: Lens.Lens' CreateDomainName Prelude.Text
createDomainName_domainName = Lens.lens (\CreateDomainName' {domainName} -> domainName) (\s@CreateDomainName' {} a -> s {domainName = a} :: CreateDomainName)

instance Core.AWSRequest CreateDomainName where
  type AWSResponse CreateDomainName = DomainName
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable CreateDomainName where
  hashWithSalt _salt CreateDomainName' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` mutualTlsAuthentication
      `Prelude.hashWithSalt` regionalCertificateName
      `Prelude.hashWithSalt` ownershipVerificationCertificateArn
      `Prelude.hashWithSalt` regionalCertificateArn
      `Prelude.hashWithSalt` certificateName
      `Prelude.hashWithSalt` certificateBody
      `Prelude.hashWithSalt` certificateArn
      `Prelude.hashWithSalt` certificatePrivateKey
      `Prelude.hashWithSalt` certificateChain
      `Prelude.hashWithSalt` endpointConfiguration
      `Prelude.hashWithSalt` securityPolicy
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData CreateDomainName where
  rnf CreateDomainName' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf mutualTlsAuthentication
      `Prelude.seq` Prelude.rnf regionalCertificateName
      `Prelude.seq` Prelude.rnf ownershipVerificationCertificateArn
      `Prelude.seq` Prelude.rnf regionalCertificateArn
      `Prelude.seq` Prelude.rnf certificateName
      `Prelude.seq` Prelude.rnf certificateBody
      `Prelude.seq` Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf certificatePrivateKey
      `Prelude.seq` Prelude.rnf certificateChain
      `Prelude.seq` Prelude.rnf endpointConfiguration
      `Prelude.seq` Prelude.rnf securityPolicy
      `Prelude.seq` Prelude.rnf domainName

instance Core.ToHeaders CreateDomainName where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToJSON CreateDomainName where
  toJSON CreateDomainName' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("mutualTlsAuthentication" Core..=)
              Prelude.<$> mutualTlsAuthentication,
            ("regionalCertificateName" Core..=)
              Prelude.<$> regionalCertificateName,
            ("ownershipVerificationCertificateArn" Core..=)
              Prelude.<$> ownershipVerificationCertificateArn,
            ("regionalCertificateArn" Core..=)
              Prelude.<$> regionalCertificateArn,
            ("certificateName" Core..=)
              Prelude.<$> certificateName,
            ("certificateBody" Core..=)
              Prelude.<$> certificateBody,
            ("certificateArn" Core..=)
              Prelude.<$> certificateArn,
            ("certificatePrivateKey" Core..=)
              Prelude.<$> certificatePrivateKey,
            ("certificateChain" Core..=)
              Prelude.<$> certificateChain,
            ("endpointConfiguration" Core..=)
              Prelude.<$> endpointConfiguration,
            ("securityPolicy" Core..=)
              Prelude.<$> securityPolicy,
            Prelude.Just ("domainName" Core..= domainName)
          ]
      )

instance Core.ToPath CreateDomainName where
  toPath = Prelude.const "/domainnames"

instance Core.ToQuery CreateDomainName where
  toQuery = Prelude.const Prelude.mempty
