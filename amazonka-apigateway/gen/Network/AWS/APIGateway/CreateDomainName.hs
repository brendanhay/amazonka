{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.APIGateway.CreateDomainName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new domain name.
module Network.AWS.APIGateway.CreateDomainName
  ( -- * Creating a Request
    CreateDomainName (..),
    newCreateDomainName,

    -- * Request Lenses
    createDomainName_certificatePrivateKey,
    createDomainName_regionalCertificateName,
    createDomainName_mutualTlsAuthentication,
    createDomainName_endpointConfiguration,
    createDomainName_certificateArn,
    createDomainName_tags,
    createDomainName_securityPolicy,
    createDomainName_certificateChain,
    createDomainName_certificateBody,
    createDomainName_regionalCertificateArn,
    createDomainName_certificateName,
    createDomainName_domainName,

    -- * Destructuring the Response
    DomainName (..),
    newDomainName,

    -- * Response Lenses
    domainName_regionalHostedZoneId,
    domainName_regionalCertificateName,
    domainName_mutualTlsAuthentication,
    domainName_endpointConfiguration,
    domainName_distributionHostedZoneId,
    domainName_certificateArn,
    domainName_domainNameStatusMessage,
    domainName_distributionDomainName,
    domainName_certificateUploadDate,
    domainName_domainName,
    domainName_tags,
    domainName_securityPolicy,
    domainName_domainNameStatus,
    domainName_regionalCertificateArn,
    domainName_certificateName,
    domainName_regionalDomainName,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to create a new domain name.
--
-- /See:/ 'newCreateDomainName' smart constructor.
data CreateDomainName = CreateDomainName'
  { -- | [Deprecated] Your edge-optimized endpoint\'s domain name certificate\'s
    -- private key.
    certificatePrivateKey :: Prelude.Maybe Prelude.Text,
    -- | The user-friendly name of the certificate that will be used by regional
    -- endpoint for this domain name.
    regionalCertificateName :: Prelude.Maybe Prelude.Text,
    mutualTlsAuthentication :: Prelude.Maybe MutualTlsAuthenticationInput,
    -- | The endpoint configuration of this DomainName showing the endpoint types
    -- of the domain name.
    endpointConfiguration :: Prelude.Maybe EndpointConfiguration,
    -- | The reference to an AWS-managed certificate that will be used by
    -- edge-optimized endpoint for this domain name. AWS Certificate Manager is
    -- the only supported source.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The key-value map of strings. The valid character set is
    -- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
    -- start with @aws:@. The tag value can be up to 256 characters.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Transport Layer Security (TLS) version + cipher suite for this
    -- DomainName. The valid values are @TLS_1_0@ and @TLS_1_2@.
    securityPolicy :: Prelude.Maybe SecurityPolicy,
    -- | [Deprecated] The intermediate certificates and optionally the root
    -- certificate, one after the other without any blank lines, used by an
    -- edge-optimized endpoint for this domain name. If you include the root
    -- certificate, your certificate chain must start with intermediate
    -- certificates and end with the root certificate. Use the intermediate
    -- certificates that were provided by your certificate authority. Do not
    -- include any intermediaries that are not in the chain of trust path.
    certificateChain :: Prelude.Maybe Prelude.Text,
    -- | [Deprecated] The body of the server certificate that will be used by
    -- edge-optimized endpoint for this domain name provided by your
    -- certificate authority.
    certificateBody :: Prelude.Maybe Prelude.Text,
    -- | The reference to an AWS-managed certificate that will be used by
    -- regional endpoint for this domain name. AWS Certificate Manager is the
    -- only supported source.
    regionalCertificateArn :: Prelude.Maybe Prelude.Text,
    -- | The user-friendly name of the certificate that will be used by
    -- edge-optimized endpoint for this domain name.
    certificateName :: Prelude.Maybe Prelude.Text,
    -- | [Required] The name of the DomainName resource.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateDomainName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificatePrivateKey', 'createDomainName_certificatePrivateKey' - [Deprecated] Your edge-optimized endpoint\'s domain name certificate\'s
-- private key.
--
-- 'regionalCertificateName', 'createDomainName_regionalCertificateName' - The user-friendly name of the certificate that will be used by regional
-- endpoint for this domain name.
--
-- 'mutualTlsAuthentication', 'createDomainName_mutualTlsAuthentication' - Undocumented member.
--
-- 'endpointConfiguration', 'createDomainName_endpointConfiguration' - The endpoint configuration of this DomainName showing the endpoint types
-- of the domain name.
--
-- 'certificateArn', 'createDomainName_certificateArn' - The reference to an AWS-managed certificate that will be used by
-- edge-optimized endpoint for this domain name. AWS Certificate Manager is
-- the only supported source.
--
-- 'tags', 'createDomainName_tags' - The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
--
-- 'securityPolicy', 'createDomainName_securityPolicy' - The Transport Layer Security (TLS) version + cipher suite for this
-- DomainName. The valid values are @TLS_1_0@ and @TLS_1_2@.
--
-- 'certificateChain', 'createDomainName_certificateChain' - [Deprecated] The intermediate certificates and optionally the root
-- certificate, one after the other without any blank lines, used by an
-- edge-optimized endpoint for this domain name. If you include the root
-- certificate, your certificate chain must start with intermediate
-- certificates and end with the root certificate. Use the intermediate
-- certificates that were provided by your certificate authority. Do not
-- include any intermediaries that are not in the chain of trust path.
--
-- 'certificateBody', 'createDomainName_certificateBody' - [Deprecated] The body of the server certificate that will be used by
-- edge-optimized endpoint for this domain name provided by your
-- certificate authority.
--
-- 'regionalCertificateArn', 'createDomainName_regionalCertificateArn' - The reference to an AWS-managed certificate that will be used by
-- regional endpoint for this domain name. AWS Certificate Manager is the
-- only supported source.
--
-- 'certificateName', 'createDomainName_certificateName' - The user-friendly name of the certificate that will be used by
-- edge-optimized endpoint for this domain name.
--
-- 'domainName', 'createDomainName_domainName' - [Required] The name of the DomainName resource.
newCreateDomainName ::
  -- | 'domainName'
  Prelude.Text ->
  CreateDomainName
newCreateDomainName pDomainName_ =
  CreateDomainName'
    { certificatePrivateKey =
        Prelude.Nothing,
      regionalCertificateName = Prelude.Nothing,
      mutualTlsAuthentication = Prelude.Nothing,
      endpointConfiguration = Prelude.Nothing,
      certificateArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      securityPolicy = Prelude.Nothing,
      certificateChain = Prelude.Nothing,
      certificateBody = Prelude.Nothing,
      regionalCertificateArn = Prelude.Nothing,
      certificateName = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | [Deprecated] Your edge-optimized endpoint\'s domain name certificate\'s
-- private key.
createDomainName_certificatePrivateKey :: Lens.Lens' CreateDomainName (Prelude.Maybe Prelude.Text)
createDomainName_certificatePrivateKey = Lens.lens (\CreateDomainName' {certificatePrivateKey} -> certificatePrivateKey) (\s@CreateDomainName' {} a -> s {certificatePrivateKey = a} :: CreateDomainName)

-- | The user-friendly name of the certificate that will be used by regional
-- endpoint for this domain name.
createDomainName_regionalCertificateName :: Lens.Lens' CreateDomainName (Prelude.Maybe Prelude.Text)
createDomainName_regionalCertificateName = Lens.lens (\CreateDomainName' {regionalCertificateName} -> regionalCertificateName) (\s@CreateDomainName' {} a -> s {regionalCertificateName = a} :: CreateDomainName)

-- | Undocumented member.
createDomainName_mutualTlsAuthentication :: Lens.Lens' CreateDomainName (Prelude.Maybe MutualTlsAuthenticationInput)
createDomainName_mutualTlsAuthentication = Lens.lens (\CreateDomainName' {mutualTlsAuthentication} -> mutualTlsAuthentication) (\s@CreateDomainName' {} a -> s {mutualTlsAuthentication = a} :: CreateDomainName)

-- | The endpoint configuration of this DomainName showing the endpoint types
-- of the domain name.
createDomainName_endpointConfiguration :: Lens.Lens' CreateDomainName (Prelude.Maybe EndpointConfiguration)
createDomainName_endpointConfiguration = Lens.lens (\CreateDomainName' {endpointConfiguration} -> endpointConfiguration) (\s@CreateDomainName' {} a -> s {endpointConfiguration = a} :: CreateDomainName)

-- | The reference to an AWS-managed certificate that will be used by
-- edge-optimized endpoint for this domain name. AWS Certificate Manager is
-- the only supported source.
createDomainName_certificateArn :: Lens.Lens' CreateDomainName (Prelude.Maybe Prelude.Text)
createDomainName_certificateArn = Lens.lens (\CreateDomainName' {certificateArn} -> certificateArn) (\s@CreateDomainName' {} a -> s {certificateArn = a} :: CreateDomainName)

-- | The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
createDomainName_tags :: Lens.Lens' CreateDomainName (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDomainName_tags = Lens.lens (\CreateDomainName' {tags} -> tags) (\s@CreateDomainName' {} a -> s {tags = a} :: CreateDomainName) Prelude.. Lens.mapping Prelude._Coerce

-- | The Transport Layer Security (TLS) version + cipher suite for this
-- DomainName. The valid values are @TLS_1_0@ and @TLS_1_2@.
createDomainName_securityPolicy :: Lens.Lens' CreateDomainName (Prelude.Maybe SecurityPolicy)
createDomainName_securityPolicy = Lens.lens (\CreateDomainName' {securityPolicy} -> securityPolicy) (\s@CreateDomainName' {} a -> s {securityPolicy = a} :: CreateDomainName)

-- | [Deprecated] The intermediate certificates and optionally the root
-- certificate, one after the other without any blank lines, used by an
-- edge-optimized endpoint for this domain name. If you include the root
-- certificate, your certificate chain must start with intermediate
-- certificates and end with the root certificate. Use the intermediate
-- certificates that were provided by your certificate authority. Do not
-- include any intermediaries that are not in the chain of trust path.
createDomainName_certificateChain :: Lens.Lens' CreateDomainName (Prelude.Maybe Prelude.Text)
createDomainName_certificateChain = Lens.lens (\CreateDomainName' {certificateChain} -> certificateChain) (\s@CreateDomainName' {} a -> s {certificateChain = a} :: CreateDomainName)

-- | [Deprecated] The body of the server certificate that will be used by
-- edge-optimized endpoint for this domain name provided by your
-- certificate authority.
createDomainName_certificateBody :: Lens.Lens' CreateDomainName (Prelude.Maybe Prelude.Text)
createDomainName_certificateBody = Lens.lens (\CreateDomainName' {certificateBody} -> certificateBody) (\s@CreateDomainName' {} a -> s {certificateBody = a} :: CreateDomainName)

-- | The reference to an AWS-managed certificate that will be used by
-- regional endpoint for this domain name. AWS Certificate Manager is the
-- only supported source.
createDomainName_regionalCertificateArn :: Lens.Lens' CreateDomainName (Prelude.Maybe Prelude.Text)
createDomainName_regionalCertificateArn = Lens.lens (\CreateDomainName' {regionalCertificateArn} -> regionalCertificateArn) (\s@CreateDomainName' {} a -> s {regionalCertificateArn = a} :: CreateDomainName)

-- | The user-friendly name of the certificate that will be used by
-- edge-optimized endpoint for this domain name.
createDomainName_certificateName :: Lens.Lens' CreateDomainName (Prelude.Maybe Prelude.Text)
createDomainName_certificateName = Lens.lens (\CreateDomainName' {certificateName} -> certificateName) (\s@CreateDomainName' {} a -> s {certificateName = a} :: CreateDomainName)

-- | [Required] The name of the DomainName resource.
createDomainName_domainName :: Lens.Lens' CreateDomainName Prelude.Text
createDomainName_domainName = Lens.lens (\CreateDomainName' {domainName} -> domainName) (\s@CreateDomainName' {} a -> s {domainName = a} :: CreateDomainName)

instance Prelude.AWSRequest CreateDomainName where
  type Rs CreateDomainName = DomainName
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Prelude.eitherParseJSON x)

instance Prelude.Hashable CreateDomainName

instance Prelude.NFData CreateDomainName

instance Prelude.ToHeaders CreateDomainName where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToJSON CreateDomainName where
  toJSON CreateDomainName' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("certificatePrivateKey" Prelude..=)
              Prelude.<$> certificatePrivateKey,
            ("regionalCertificateName" Prelude..=)
              Prelude.<$> regionalCertificateName,
            ("mutualTlsAuthentication" Prelude..=)
              Prelude.<$> mutualTlsAuthentication,
            ("endpointConfiguration" Prelude..=)
              Prelude.<$> endpointConfiguration,
            ("certificateArn" Prelude..=)
              Prelude.<$> certificateArn,
            ("tags" Prelude..=) Prelude.<$> tags,
            ("securityPolicy" Prelude..=)
              Prelude.<$> securityPolicy,
            ("certificateChain" Prelude..=)
              Prelude.<$> certificateChain,
            ("certificateBody" Prelude..=)
              Prelude.<$> certificateBody,
            ("regionalCertificateArn" Prelude..=)
              Prelude.<$> regionalCertificateArn,
            ("certificateName" Prelude..=)
              Prelude.<$> certificateName,
            Prelude.Just ("domainName" Prelude..= domainName)
          ]
      )

instance Prelude.ToPath CreateDomainName where
  toPath = Prelude.const "/domainnames"

instance Prelude.ToQuery CreateDomainName where
  toQuery = Prelude.const Prelude.mempty
