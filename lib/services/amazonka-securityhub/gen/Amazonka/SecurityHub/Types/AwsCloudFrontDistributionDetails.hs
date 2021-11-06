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
-- Module      : Amazonka.SecurityHub.Types.AwsCloudFrontDistributionDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCloudFrontDistributionDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionCacheBehaviors
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionDefaultCacheBehavior
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionLogging
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginGroups
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOrigins
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionViewerCertificate

-- | A distribution configuration.
--
-- /See:/ 'newAwsCloudFrontDistributionDetails' smart constructor.
data AwsCloudFrontDistributionDetails = AwsCloudFrontDistributionDetails'
  { -- | Indicates the current status of the distribution.
    status :: Prelude.Maybe Prelude.Text,
    -- | The entity tag is a hash of the object.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | Provides information about the origin groups in the distribution.
    originGroups :: Prelude.Maybe AwsCloudFrontDistributionOriginGroups,
    -- | The object that CloudFront sends in response to requests from the origin
    -- (for example, index.html) when a viewer requests the root URL for the
    -- distribution (http:\/\/www.example.com) instead of an object in your
    -- distribution (http:\/\/www.example.com\/product-description.html).
    defaultRootObject :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier that specifies the WAF web ACL, if any, to associate
    -- with this distribution.
    webAclId :: Prelude.Maybe Prelude.Text,
    -- | Indicates when that the distribution was last modified.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    lastModifiedTime :: Prelude.Maybe Prelude.Text,
    -- | Provides information about the TLS\/SSL configuration that the
    -- distribution uses to communicate with viewers.
    viewerCertificate :: Prelude.Maybe AwsCloudFrontDistributionViewerCertificate,
    -- | The domain name corresponding to the distribution.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | A complex type that contains information about origins for this
    -- distribution.
    origins :: Prelude.Maybe AwsCloudFrontDistributionOrigins,
    -- | A complex type that controls whether access logs are written for the
    -- distribution.
    logging :: Prelude.Maybe AwsCloudFrontDistributionLogging,
    -- | Provides information about the cache configuration for the distribution.
    cacheBehaviors :: Prelude.Maybe AwsCloudFrontDistributionCacheBehaviors,
    -- | The default cache behavior for the configuration.
    defaultCacheBehavior :: Prelude.Maybe AwsCloudFrontDistributionDefaultCacheBehavior
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCloudFrontDistributionDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'awsCloudFrontDistributionDetails_status' - Indicates the current status of the distribution.
--
-- 'eTag', 'awsCloudFrontDistributionDetails_eTag' - The entity tag is a hash of the object.
--
-- 'originGroups', 'awsCloudFrontDistributionDetails_originGroups' - Provides information about the origin groups in the distribution.
--
-- 'defaultRootObject', 'awsCloudFrontDistributionDetails_defaultRootObject' - The object that CloudFront sends in response to requests from the origin
-- (for example, index.html) when a viewer requests the root URL for the
-- distribution (http:\/\/www.example.com) instead of an object in your
-- distribution (http:\/\/www.example.com\/product-description.html).
--
-- 'webAclId', 'awsCloudFrontDistributionDetails_webAclId' - A unique identifier that specifies the WAF web ACL, if any, to associate
-- with this distribution.
--
-- 'lastModifiedTime', 'awsCloudFrontDistributionDetails_lastModifiedTime' - Indicates when that the distribution was last modified.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'viewerCertificate', 'awsCloudFrontDistributionDetails_viewerCertificate' - Provides information about the TLS\/SSL configuration that the
-- distribution uses to communicate with viewers.
--
-- 'domainName', 'awsCloudFrontDistributionDetails_domainName' - The domain name corresponding to the distribution.
--
-- 'origins', 'awsCloudFrontDistributionDetails_origins' - A complex type that contains information about origins for this
-- distribution.
--
-- 'logging', 'awsCloudFrontDistributionDetails_logging' - A complex type that controls whether access logs are written for the
-- distribution.
--
-- 'cacheBehaviors', 'awsCloudFrontDistributionDetails_cacheBehaviors' - Provides information about the cache configuration for the distribution.
--
-- 'defaultCacheBehavior', 'awsCloudFrontDistributionDetails_defaultCacheBehavior' - The default cache behavior for the configuration.
newAwsCloudFrontDistributionDetails ::
  AwsCloudFrontDistributionDetails
newAwsCloudFrontDistributionDetails =
  AwsCloudFrontDistributionDetails'
    { status =
        Prelude.Nothing,
      eTag = Prelude.Nothing,
      originGroups = Prelude.Nothing,
      defaultRootObject = Prelude.Nothing,
      webAclId = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      viewerCertificate = Prelude.Nothing,
      domainName = Prelude.Nothing,
      origins = Prelude.Nothing,
      logging = Prelude.Nothing,
      cacheBehaviors = Prelude.Nothing,
      defaultCacheBehavior = Prelude.Nothing
    }

-- | Indicates the current status of the distribution.
awsCloudFrontDistributionDetails_status :: Lens.Lens' AwsCloudFrontDistributionDetails (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionDetails_status = Lens.lens (\AwsCloudFrontDistributionDetails' {status} -> status) (\s@AwsCloudFrontDistributionDetails' {} a -> s {status = a} :: AwsCloudFrontDistributionDetails)

-- | The entity tag is a hash of the object.
awsCloudFrontDistributionDetails_eTag :: Lens.Lens' AwsCloudFrontDistributionDetails (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionDetails_eTag = Lens.lens (\AwsCloudFrontDistributionDetails' {eTag} -> eTag) (\s@AwsCloudFrontDistributionDetails' {} a -> s {eTag = a} :: AwsCloudFrontDistributionDetails)

-- | Provides information about the origin groups in the distribution.
awsCloudFrontDistributionDetails_originGroups :: Lens.Lens' AwsCloudFrontDistributionDetails (Prelude.Maybe AwsCloudFrontDistributionOriginGroups)
awsCloudFrontDistributionDetails_originGroups = Lens.lens (\AwsCloudFrontDistributionDetails' {originGroups} -> originGroups) (\s@AwsCloudFrontDistributionDetails' {} a -> s {originGroups = a} :: AwsCloudFrontDistributionDetails)

-- | The object that CloudFront sends in response to requests from the origin
-- (for example, index.html) when a viewer requests the root URL for the
-- distribution (http:\/\/www.example.com) instead of an object in your
-- distribution (http:\/\/www.example.com\/product-description.html).
awsCloudFrontDistributionDetails_defaultRootObject :: Lens.Lens' AwsCloudFrontDistributionDetails (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionDetails_defaultRootObject = Lens.lens (\AwsCloudFrontDistributionDetails' {defaultRootObject} -> defaultRootObject) (\s@AwsCloudFrontDistributionDetails' {} a -> s {defaultRootObject = a} :: AwsCloudFrontDistributionDetails)

-- | A unique identifier that specifies the WAF web ACL, if any, to associate
-- with this distribution.
awsCloudFrontDistributionDetails_webAclId :: Lens.Lens' AwsCloudFrontDistributionDetails (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionDetails_webAclId = Lens.lens (\AwsCloudFrontDistributionDetails' {webAclId} -> webAclId) (\s@AwsCloudFrontDistributionDetails' {} a -> s {webAclId = a} :: AwsCloudFrontDistributionDetails)

-- | Indicates when that the distribution was last modified.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsCloudFrontDistributionDetails_lastModifiedTime :: Lens.Lens' AwsCloudFrontDistributionDetails (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionDetails_lastModifiedTime = Lens.lens (\AwsCloudFrontDistributionDetails' {lastModifiedTime} -> lastModifiedTime) (\s@AwsCloudFrontDistributionDetails' {} a -> s {lastModifiedTime = a} :: AwsCloudFrontDistributionDetails)

-- | Provides information about the TLS\/SSL configuration that the
-- distribution uses to communicate with viewers.
awsCloudFrontDistributionDetails_viewerCertificate :: Lens.Lens' AwsCloudFrontDistributionDetails (Prelude.Maybe AwsCloudFrontDistributionViewerCertificate)
awsCloudFrontDistributionDetails_viewerCertificate = Lens.lens (\AwsCloudFrontDistributionDetails' {viewerCertificate} -> viewerCertificate) (\s@AwsCloudFrontDistributionDetails' {} a -> s {viewerCertificate = a} :: AwsCloudFrontDistributionDetails)

-- | The domain name corresponding to the distribution.
awsCloudFrontDistributionDetails_domainName :: Lens.Lens' AwsCloudFrontDistributionDetails (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionDetails_domainName = Lens.lens (\AwsCloudFrontDistributionDetails' {domainName} -> domainName) (\s@AwsCloudFrontDistributionDetails' {} a -> s {domainName = a} :: AwsCloudFrontDistributionDetails)

-- | A complex type that contains information about origins for this
-- distribution.
awsCloudFrontDistributionDetails_origins :: Lens.Lens' AwsCloudFrontDistributionDetails (Prelude.Maybe AwsCloudFrontDistributionOrigins)
awsCloudFrontDistributionDetails_origins = Lens.lens (\AwsCloudFrontDistributionDetails' {origins} -> origins) (\s@AwsCloudFrontDistributionDetails' {} a -> s {origins = a} :: AwsCloudFrontDistributionDetails)

-- | A complex type that controls whether access logs are written for the
-- distribution.
awsCloudFrontDistributionDetails_logging :: Lens.Lens' AwsCloudFrontDistributionDetails (Prelude.Maybe AwsCloudFrontDistributionLogging)
awsCloudFrontDistributionDetails_logging = Lens.lens (\AwsCloudFrontDistributionDetails' {logging} -> logging) (\s@AwsCloudFrontDistributionDetails' {} a -> s {logging = a} :: AwsCloudFrontDistributionDetails)

-- | Provides information about the cache configuration for the distribution.
awsCloudFrontDistributionDetails_cacheBehaviors :: Lens.Lens' AwsCloudFrontDistributionDetails (Prelude.Maybe AwsCloudFrontDistributionCacheBehaviors)
awsCloudFrontDistributionDetails_cacheBehaviors = Lens.lens (\AwsCloudFrontDistributionDetails' {cacheBehaviors} -> cacheBehaviors) (\s@AwsCloudFrontDistributionDetails' {} a -> s {cacheBehaviors = a} :: AwsCloudFrontDistributionDetails)

-- | The default cache behavior for the configuration.
awsCloudFrontDistributionDetails_defaultCacheBehavior :: Lens.Lens' AwsCloudFrontDistributionDetails (Prelude.Maybe AwsCloudFrontDistributionDefaultCacheBehavior)
awsCloudFrontDistributionDetails_defaultCacheBehavior = Lens.lens (\AwsCloudFrontDistributionDetails' {defaultCacheBehavior} -> defaultCacheBehavior) (\s@AwsCloudFrontDistributionDetails' {} a -> s {defaultCacheBehavior = a} :: AwsCloudFrontDistributionDetails)

instance
  Core.FromJSON
    AwsCloudFrontDistributionDetails
  where
  parseJSON =
    Core.withObject
      "AwsCloudFrontDistributionDetails"
      ( \x ->
          AwsCloudFrontDistributionDetails'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "ETag")
            Prelude.<*> (x Core..:? "OriginGroups")
            Prelude.<*> (x Core..:? "DefaultRootObject")
            Prelude.<*> (x Core..:? "WebAclId")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "ViewerCertificate")
            Prelude.<*> (x Core..:? "DomainName")
            Prelude.<*> (x Core..:? "Origins")
            Prelude.<*> (x Core..:? "Logging")
            Prelude.<*> (x Core..:? "CacheBehaviors")
            Prelude.<*> (x Core..:? "DefaultCacheBehavior")
      )

instance
  Prelude.Hashable
    AwsCloudFrontDistributionDetails

instance
  Prelude.NFData
    AwsCloudFrontDistributionDetails

instance Core.ToJSON AwsCloudFrontDistributionDetails where
  toJSON AwsCloudFrontDistributionDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Status" Core..=) Prelude.<$> status,
            ("ETag" Core..=) Prelude.<$> eTag,
            ("OriginGroups" Core..=) Prelude.<$> originGroups,
            ("DefaultRootObject" Core..=)
              Prelude.<$> defaultRootObject,
            ("WebAclId" Core..=) Prelude.<$> webAclId,
            ("LastModifiedTime" Core..=)
              Prelude.<$> lastModifiedTime,
            ("ViewerCertificate" Core..=)
              Prelude.<$> viewerCertificate,
            ("DomainName" Core..=) Prelude.<$> domainName,
            ("Origins" Core..=) Prelude.<$> origins,
            ("Logging" Core..=) Prelude.<$> logging,
            ("CacheBehaviors" Core..=)
              Prelude.<$> cacheBehaviors,
            ("DefaultCacheBehavior" Core..=)
              Prelude.<$> defaultCacheBehavior
          ]
      )
