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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    createDomainName_certificateArn,
    createDomainName_certificateBody,
    createDomainName_certificateChain,
    createDomainName_certificateName,
    createDomainName_certificatePrivateKey,
    createDomainName_endpointConfiguration,
    createDomainName_mutualTlsAuthentication,
    createDomainName_ownershipVerificationCertificateArn,
    createDomainName_regionalCertificateArn,
    createDomainName_regionalCertificateName,
    createDomainName_securityPolicy,
    createDomainName_tags,
    createDomainName_domainName,

    -- * Destructuring the Response
    DomainName (..),
    newDomainName,

    -- * Response Lenses
    domainName_certificateArn,
    domainName_certificateName,
    domainName_certificateUploadDate,
    domainName_distributionDomainName,
    domainName_distributionHostedZoneId,
    domainName_domainName,
    domainName_domainNameStatus,
    domainName_domainNameStatusMessage,
    domainName_endpointConfiguration,
    domainName_mutualTlsAuthentication,
    domainName_ownershipVerificationCertificateArn,
    domainName_regionalCertificateArn,
    domainName_regionalCertificateName,
    domainName_regionalDomainName,
    domainName_regionalHostedZoneId,
    domainName_securityPolicy,
    domainName_tags,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to create a new domain name.
--
-- /See:/ 'newCreateDomainName' smart constructor.
data CreateDomainName = CreateDomainName'
  { -- | The reference to an AWS-managed certificate that will be used by
    -- edge-optimized endpoint for this domain name. AWS Certificate Manager is
    -- the only supported source.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | [Deprecated] The body of the server certificate that will be used by
    -- edge-optimized endpoint for this domain name provided by your
    -- certificate authority.
    certificateBody :: Prelude.Maybe Prelude.Text,
    -- | [Deprecated] The intermediate certificates and optionally the root
    -- certificate, one after the other without any blank lines, used by an
    -- edge-optimized endpoint for this domain name. If you include the root
    -- certificate, your certificate chain must start with intermediate
    -- certificates and end with the root certificate. Use the intermediate
    -- certificates that were provided by your certificate authority. Do not
    -- include any intermediaries that are not in the chain of trust path.
    certificateChain :: Prelude.Maybe Prelude.Text,
    -- | The user-friendly name of the certificate that will be used by
    -- edge-optimized endpoint for this domain name.
    certificateName :: Prelude.Maybe Prelude.Text,
    -- | [Deprecated] Your edge-optimized endpoint\'s domain name certificate\'s
    -- private key.
    certificatePrivateKey :: Prelude.Maybe Prelude.Text,
    -- | The endpoint configuration of this DomainName showing the endpoint types
    -- of the domain name.
    endpointConfiguration :: Prelude.Maybe EndpointConfiguration,
    mutualTlsAuthentication :: Prelude.Maybe MutualTlsAuthenticationInput,
    -- | The ARN of the public certificate issued by ACM to validate ownership of
    -- your custom domain. Only required when configuring mutual TLS and using
    -- an ACM imported or private CA certificate ARN as the
    -- regionalCertificateArn.
    ownershipVerificationCertificateArn :: Prelude.Maybe Prelude.Text,
    -- | The reference to an AWS-managed certificate that will be used by
    -- regional endpoint for this domain name. AWS Certificate Manager is the
    -- only supported source.
    regionalCertificateArn :: Prelude.Maybe Prelude.Text,
    -- | The user-friendly name of the certificate that will be used by regional
    -- endpoint for this domain name.
    regionalCertificateName :: Prelude.Maybe Prelude.Text,
    -- | The Transport Layer Security (TLS) version + cipher suite for this
    -- DomainName. The valid values are @TLS_1_0@ and @TLS_1_2@.
    securityPolicy :: Prelude.Maybe SecurityPolicy,
    -- | The key-value map of strings. The valid character set is
    -- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
    -- start with @aws:@. The tag value can be up to 256 characters.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
-- 'certificateArn', 'createDomainName_certificateArn' - The reference to an AWS-managed certificate that will be used by
-- edge-optimized endpoint for this domain name. AWS Certificate Manager is
-- the only supported source.
--
-- 'certificateBody', 'createDomainName_certificateBody' - [Deprecated] The body of the server certificate that will be used by
-- edge-optimized endpoint for this domain name provided by your
-- certificate authority.
--
-- 'certificateChain', 'createDomainName_certificateChain' - [Deprecated] The intermediate certificates and optionally the root
-- certificate, one after the other without any blank lines, used by an
-- edge-optimized endpoint for this domain name. If you include the root
-- certificate, your certificate chain must start with intermediate
-- certificates and end with the root certificate. Use the intermediate
-- certificates that were provided by your certificate authority. Do not
-- include any intermediaries that are not in the chain of trust path.
--
-- 'certificateName', 'createDomainName_certificateName' - The user-friendly name of the certificate that will be used by
-- edge-optimized endpoint for this domain name.
--
-- 'certificatePrivateKey', 'createDomainName_certificatePrivateKey' - [Deprecated] Your edge-optimized endpoint\'s domain name certificate\'s
-- private key.
--
-- 'endpointConfiguration', 'createDomainName_endpointConfiguration' - The endpoint configuration of this DomainName showing the endpoint types
-- of the domain name.
--
-- 'mutualTlsAuthentication', 'createDomainName_mutualTlsAuthentication' - Undocumented member.
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
-- 'regionalCertificateName', 'createDomainName_regionalCertificateName' - The user-friendly name of the certificate that will be used by regional
-- endpoint for this domain name.
--
-- 'securityPolicy', 'createDomainName_securityPolicy' - The Transport Layer Security (TLS) version + cipher suite for this
-- DomainName. The valid values are @TLS_1_0@ and @TLS_1_2@.
--
-- 'tags', 'createDomainName_tags' - The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
--
-- 'domainName', 'createDomainName_domainName' - The name of the DomainName resource.
newCreateDomainName ::
  -- | 'domainName'
  Prelude.Text ->
  CreateDomainName
newCreateDomainName pDomainName_ =
  CreateDomainName'
    { certificateArn = Prelude.Nothing,
      certificateBody = Prelude.Nothing,
      certificateChain = Prelude.Nothing,
      certificateName = Prelude.Nothing,
      certificatePrivateKey = Prelude.Nothing,
      endpointConfiguration = Prelude.Nothing,
      mutualTlsAuthentication = Prelude.Nothing,
      ownershipVerificationCertificateArn =
        Prelude.Nothing,
      regionalCertificateArn = Prelude.Nothing,
      regionalCertificateName = Prelude.Nothing,
      securityPolicy = Prelude.Nothing,
      tags = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | The reference to an AWS-managed certificate that will be used by
-- edge-optimized endpoint for this domain name. AWS Certificate Manager is
-- the only supported source.
createDomainName_certificateArn :: Lens.Lens' CreateDomainName (Prelude.Maybe Prelude.Text)
createDomainName_certificateArn = Lens.lens (\CreateDomainName' {certificateArn} -> certificateArn) (\s@CreateDomainName' {} a -> s {certificateArn = a} :: CreateDomainName)

-- | [Deprecated] The body of the server certificate that will be used by
-- edge-optimized endpoint for this domain name provided by your
-- certificate authority.
createDomainName_certificateBody :: Lens.Lens' CreateDomainName (Prelude.Maybe Prelude.Text)
createDomainName_certificateBody = Lens.lens (\CreateDomainName' {certificateBody} -> certificateBody) (\s@CreateDomainName' {} a -> s {certificateBody = a} :: CreateDomainName)

-- | [Deprecated] The intermediate certificates and optionally the root
-- certificate, one after the other without any blank lines, used by an
-- edge-optimized endpoint for this domain name. If you include the root
-- certificate, your certificate chain must start with intermediate
-- certificates and end with the root certificate. Use the intermediate
-- certificates that were provided by your certificate authority. Do not
-- include any intermediaries that are not in the chain of trust path.
createDomainName_certificateChain :: Lens.Lens' CreateDomainName (Prelude.Maybe Prelude.Text)
createDomainName_certificateChain = Lens.lens (\CreateDomainName' {certificateChain} -> certificateChain) (\s@CreateDomainName' {} a -> s {certificateChain = a} :: CreateDomainName)

-- | The user-friendly name of the certificate that will be used by
-- edge-optimized endpoint for this domain name.
createDomainName_certificateName :: Lens.Lens' CreateDomainName (Prelude.Maybe Prelude.Text)
createDomainName_certificateName = Lens.lens (\CreateDomainName' {certificateName} -> certificateName) (\s@CreateDomainName' {} a -> s {certificateName = a} :: CreateDomainName)

-- | [Deprecated] Your edge-optimized endpoint\'s domain name certificate\'s
-- private key.
createDomainName_certificatePrivateKey :: Lens.Lens' CreateDomainName (Prelude.Maybe Prelude.Text)
createDomainName_certificatePrivateKey = Lens.lens (\CreateDomainName' {certificatePrivateKey} -> certificatePrivateKey) (\s@CreateDomainName' {} a -> s {certificatePrivateKey = a} :: CreateDomainName)

-- | The endpoint configuration of this DomainName showing the endpoint types
-- of the domain name.
createDomainName_endpointConfiguration :: Lens.Lens' CreateDomainName (Prelude.Maybe EndpointConfiguration)
createDomainName_endpointConfiguration = Lens.lens (\CreateDomainName' {endpointConfiguration} -> endpointConfiguration) (\s@CreateDomainName' {} a -> s {endpointConfiguration = a} :: CreateDomainName)

-- | Undocumented member.
createDomainName_mutualTlsAuthentication :: Lens.Lens' CreateDomainName (Prelude.Maybe MutualTlsAuthenticationInput)
createDomainName_mutualTlsAuthentication = Lens.lens (\CreateDomainName' {mutualTlsAuthentication} -> mutualTlsAuthentication) (\s@CreateDomainName' {} a -> s {mutualTlsAuthentication = a} :: CreateDomainName)

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

-- | The user-friendly name of the certificate that will be used by regional
-- endpoint for this domain name.
createDomainName_regionalCertificateName :: Lens.Lens' CreateDomainName (Prelude.Maybe Prelude.Text)
createDomainName_regionalCertificateName = Lens.lens (\CreateDomainName' {regionalCertificateName} -> regionalCertificateName) (\s@CreateDomainName' {} a -> s {regionalCertificateName = a} :: CreateDomainName)

-- | The Transport Layer Security (TLS) version + cipher suite for this
-- DomainName. The valid values are @TLS_1_0@ and @TLS_1_2@.
createDomainName_securityPolicy :: Lens.Lens' CreateDomainName (Prelude.Maybe SecurityPolicy)
createDomainName_securityPolicy = Lens.lens (\CreateDomainName' {securityPolicy} -> securityPolicy) (\s@CreateDomainName' {} a -> s {securityPolicy = a} :: CreateDomainName)

-- | The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
createDomainName_tags :: Lens.Lens' CreateDomainName (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDomainName_tags = Lens.lens (\CreateDomainName' {tags} -> tags) (\s@CreateDomainName' {} a -> s {tags = a} :: CreateDomainName) Prelude.. Lens.mapping Lens.coerced

-- | The name of the DomainName resource.
createDomainName_domainName :: Lens.Lens' CreateDomainName Prelude.Text
createDomainName_domainName = Lens.lens (\CreateDomainName' {domainName} -> domainName) (\s@CreateDomainName' {} a -> s {domainName = a} :: CreateDomainName)

instance Core.AWSRequest CreateDomainName where
  type AWSResponse CreateDomainName = DomainName
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable CreateDomainName where
  hashWithSalt _salt CreateDomainName' {..} =
    _salt
      `Prelude.hashWithSalt` certificateArn
      `Prelude.hashWithSalt` certificateBody
      `Prelude.hashWithSalt` certificateChain
      `Prelude.hashWithSalt` certificateName
      `Prelude.hashWithSalt` certificatePrivateKey
      `Prelude.hashWithSalt` endpointConfiguration
      `Prelude.hashWithSalt` mutualTlsAuthentication
      `Prelude.hashWithSalt` ownershipVerificationCertificateArn
      `Prelude.hashWithSalt` regionalCertificateArn
      `Prelude.hashWithSalt` regionalCertificateName
      `Prelude.hashWithSalt` securityPolicy
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData CreateDomainName where
  rnf CreateDomainName' {..} =
    Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf certificateBody
      `Prelude.seq` Prelude.rnf certificateChain
      `Prelude.seq` Prelude.rnf certificateName
      `Prelude.seq` Prelude.rnf certificatePrivateKey
      `Prelude.seq` Prelude.rnf endpointConfiguration
      `Prelude.seq` Prelude.rnf mutualTlsAuthentication
      `Prelude.seq` Prelude.rnf ownershipVerificationCertificateArn
      `Prelude.seq` Prelude.rnf regionalCertificateArn
      `Prelude.seq` Prelude.rnf regionalCertificateName
      `Prelude.seq` Prelude.rnf securityPolicy
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders CreateDomainName where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToJSON CreateDomainName where
  toJSON CreateDomainName' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("certificateArn" Data..=)
              Prelude.<$> certificateArn,
            ("certificateBody" Data..=)
              Prelude.<$> certificateBody,
            ("certificateChain" Data..=)
              Prelude.<$> certificateChain,
            ("certificateName" Data..=)
              Prelude.<$> certificateName,
            ("certificatePrivateKey" Data..=)
              Prelude.<$> certificatePrivateKey,
            ("endpointConfiguration" Data..=)
              Prelude.<$> endpointConfiguration,
            ("mutualTlsAuthentication" Data..=)
              Prelude.<$> mutualTlsAuthentication,
            ("ownershipVerificationCertificateArn" Data..=)
              Prelude.<$> ownershipVerificationCertificateArn,
            ("regionalCertificateArn" Data..=)
              Prelude.<$> regionalCertificateArn,
            ("regionalCertificateName" Data..=)
              Prelude.<$> regionalCertificateName,
            ("securityPolicy" Data..=)
              Prelude.<$> securityPolicy,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("domainName" Data..= domainName)
          ]
      )

instance Data.ToPath CreateDomainName where
  toPath = Prelude.const "/domainnames"

instance Data.ToQuery CreateDomainName where
  toQuery = Prelude.const Prelude.mempty
