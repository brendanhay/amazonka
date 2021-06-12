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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.DomainDescriptionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.DomainDescriptionType where

import Network.AWS.CognitoIdentityProvider.Types.CustomDomainConfigType
import Network.AWS.CognitoIdentityProvider.Types.DomainStatusType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A container for information about a domain.
--
-- /See:/ 'newDomainDescriptionType' smart constructor.
data DomainDescriptionType = DomainDescriptionType'
  { -- | The domain status.
    status :: Core.Maybe DomainStatusType,
    -- | The configuration for a custom domain that hosts the sign-up and sign-in
    -- webpages for your application.
    customDomainConfig :: Core.Maybe CustomDomainConfigType,
    -- | The AWS account ID for the user pool owner.
    aWSAccountId :: Core.Maybe Core.Text,
    -- | The S3 bucket where the static files for this domain are stored.
    s3Bucket :: Core.Maybe Core.Text,
    -- | The user pool ID.
    userPoolId :: Core.Maybe Core.Text,
    -- | The domain string.
    domain :: Core.Maybe Core.Text,
    -- | The ARN of the CloudFront distribution.
    cloudFrontDistribution :: Core.Maybe Core.Text,
    -- | The app version.
    version :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DomainDescriptionType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'domainDescriptionType_status' - The domain status.
--
-- 'customDomainConfig', 'domainDescriptionType_customDomainConfig' - The configuration for a custom domain that hosts the sign-up and sign-in
-- webpages for your application.
--
-- 'aWSAccountId', 'domainDescriptionType_aWSAccountId' - The AWS account ID for the user pool owner.
--
-- 's3Bucket', 'domainDescriptionType_s3Bucket' - The S3 bucket where the static files for this domain are stored.
--
-- 'userPoolId', 'domainDescriptionType_userPoolId' - The user pool ID.
--
-- 'domain', 'domainDescriptionType_domain' - The domain string.
--
-- 'cloudFrontDistribution', 'domainDescriptionType_cloudFrontDistribution' - The ARN of the CloudFront distribution.
--
-- 'version', 'domainDescriptionType_version' - The app version.
newDomainDescriptionType ::
  DomainDescriptionType
newDomainDescriptionType =
  DomainDescriptionType'
    { status = Core.Nothing,
      customDomainConfig = Core.Nothing,
      aWSAccountId = Core.Nothing,
      s3Bucket = Core.Nothing,
      userPoolId = Core.Nothing,
      domain = Core.Nothing,
      cloudFrontDistribution = Core.Nothing,
      version = Core.Nothing
    }

-- | The domain status.
domainDescriptionType_status :: Lens.Lens' DomainDescriptionType (Core.Maybe DomainStatusType)
domainDescriptionType_status = Lens.lens (\DomainDescriptionType' {status} -> status) (\s@DomainDescriptionType' {} a -> s {status = a} :: DomainDescriptionType)

-- | The configuration for a custom domain that hosts the sign-up and sign-in
-- webpages for your application.
domainDescriptionType_customDomainConfig :: Lens.Lens' DomainDescriptionType (Core.Maybe CustomDomainConfigType)
domainDescriptionType_customDomainConfig = Lens.lens (\DomainDescriptionType' {customDomainConfig} -> customDomainConfig) (\s@DomainDescriptionType' {} a -> s {customDomainConfig = a} :: DomainDescriptionType)

-- | The AWS account ID for the user pool owner.
domainDescriptionType_aWSAccountId :: Lens.Lens' DomainDescriptionType (Core.Maybe Core.Text)
domainDescriptionType_aWSAccountId = Lens.lens (\DomainDescriptionType' {aWSAccountId} -> aWSAccountId) (\s@DomainDescriptionType' {} a -> s {aWSAccountId = a} :: DomainDescriptionType)

-- | The S3 bucket where the static files for this domain are stored.
domainDescriptionType_s3Bucket :: Lens.Lens' DomainDescriptionType (Core.Maybe Core.Text)
domainDescriptionType_s3Bucket = Lens.lens (\DomainDescriptionType' {s3Bucket} -> s3Bucket) (\s@DomainDescriptionType' {} a -> s {s3Bucket = a} :: DomainDescriptionType)

-- | The user pool ID.
domainDescriptionType_userPoolId :: Lens.Lens' DomainDescriptionType (Core.Maybe Core.Text)
domainDescriptionType_userPoolId = Lens.lens (\DomainDescriptionType' {userPoolId} -> userPoolId) (\s@DomainDescriptionType' {} a -> s {userPoolId = a} :: DomainDescriptionType)

-- | The domain string.
domainDescriptionType_domain :: Lens.Lens' DomainDescriptionType (Core.Maybe Core.Text)
domainDescriptionType_domain = Lens.lens (\DomainDescriptionType' {domain} -> domain) (\s@DomainDescriptionType' {} a -> s {domain = a} :: DomainDescriptionType)

-- | The ARN of the CloudFront distribution.
domainDescriptionType_cloudFrontDistribution :: Lens.Lens' DomainDescriptionType (Core.Maybe Core.Text)
domainDescriptionType_cloudFrontDistribution = Lens.lens (\DomainDescriptionType' {cloudFrontDistribution} -> cloudFrontDistribution) (\s@DomainDescriptionType' {} a -> s {cloudFrontDistribution = a} :: DomainDescriptionType)

-- | The app version.
domainDescriptionType_version :: Lens.Lens' DomainDescriptionType (Core.Maybe Core.Text)
domainDescriptionType_version = Lens.lens (\DomainDescriptionType' {version} -> version) (\s@DomainDescriptionType' {} a -> s {version = a} :: DomainDescriptionType)

instance Core.FromJSON DomainDescriptionType where
  parseJSON =
    Core.withObject
      "DomainDescriptionType"
      ( \x ->
          DomainDescriptionType'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "CustomDomainConfig")
            Core.<*> (x Core..:? "AWSAccountId")
            Core.<*> (x Core..:? "S3Bucket")
            Core.<*> (x Core..:? "UserPoolId")
            Core.<*> (x Core..:? "Domain")
            Core.<*> (x Core..:? "CloudFrontDistribution")
            Core.<*> (x Core..:? "Version")
      )

instance Core.Hashable DomainDescriptionType

instance Core.NFData DomainDescriptionType
