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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCloudFrontDistributionDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionCacheBehaviors
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionDefaultCacheBehavior
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionLogging
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginGroups
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOrigins
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionViewerCertificate

-- | A CloudFront distribution configuration.
--
-- /See:/ 'newAwsCloudFrontDistributionDetails' smart constructor.
data AwsCloudFrontDistributionDetails = AwsCloudFrontDistributionDetails'
  { -- | The domain name corresponding to the distribution.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | Indicates the current status of the distribution.
    status :: Prelude.Maybe Prelude.Text,
    -- | The object that CloudFront sends in response to requests from the origin
    -- (for example, index.html) when a viewer requests the root URL for the
    -- distribution (http:\/\/www.example.com) instead of an object in your
    -- distribution (http:\/\/www.example.com\/product-description.html).
    defaultRootObject :: Prelude.Maybe Prelude.Text,
    -- | Provides information about the TLS\/SSL configuration that the
    -- distribution uses to communicate with viewers.
    viewerCertificate :: Prelude.Maybe AwsCloudFrontDistributionViewerCertificate,
    -- | Indicates when that the distribution was last modified.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    lastModifiedTime :: Prelude.Maybe Prelude.Text,
    -- | A complex type that controls whether access logs are written for the
    -- distribution.
    logging :: Prelude.Maybe AwsCloudFrontDistributionLogging,
    -- | A unique identifier that specifies the WAF web ACL, if any, to associate
    -- with this distribution.
    webAclId :: Prelude.Maybe Prelude.Text,
    -- | Provides information about the origin groups in the distribution.
    originGroups :: Prelude.Maybe AwsCloudFrontDistributionOriginGroups,
    -- | A complex type that contains information about origins for this
    -- distribution.
    origins :: Prelude.Maybe AwsCloudFrontDistributionOrigins,
    -- | Provides information about the cache configuration for the distribution.
    cacheBehaviors :: Prelude.Maybe AwsCloudFrontDistributionCacheBehaviors,
    -- | The entity tag is a hash of the object.
    eTag :: Prelude.Maybe Prelude.Text,
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
-- 'domainName', 'awsCloudFrontDistributionDetails_domainName' - The domain name corresponding to the distribution.
--
-- 'status', 'awsCloudFrontDistributionDetails_status' - Indicates the current status of the distribution.
--
-- 'defaultRootObject', 'awsCloudFrontDistributionDetails_defaultRootObject' - The object that CloudFront sends in response to requests from the origin
-- (for example, index.html) when a viewer requests the root URL for the
-- distribution (http:\/\/www.example.com) instead of an object in your
-- distribution (http:\/\/www.example.com\/product-description.html).
--
-- 'viewerCertificate', 'awsCloudFrontDistributionDetails_viewerCertificate' - Provides information about the TLS\/SSL configuration that the
-- distribution uses to communicate with viewers.
--
-- 'lastModifiedTime', 'awsCloudFrontDistributionDetails_lastModifiedTime' - Indicates when that the distribution was last modified.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'logging', 'awsCloudFrontDistributionDetails_logging' - A complex type that controls whether access logs are written for the
-- distribution.
--
-- 'webAclId', 'awsCloudFrontDistributionDetails_webAclId' - A unique identifier that specifies the WAF web ACL, if any, to associate
-- with this distribution.
--
-- 'originGroups', 'awsCloudFrontDistributionDetails_originGroups' - Provides information about the origin groups in the distribution.
--
-- 'origins', 'awsCloudFrontDistributionDetails_origins' - A complex type that contains information about origins for this
-- distribution.
--
-- 'cacheBehaviors', 'awsCloudFrontDistributionDetails_cacheBehaviors' - Provides information about the cache configuration for the distribution.
--
-- 'eTag', 'awsCloudFrontDistributionDetails_eTag' - The entity tag is a hash of the object.
--
-- 'defaultCacheBehavior', 'awsCloudFrontDistributionDetails_defaultCacheBehavior' - The default cache behavior for the configuration.
newAwsCloudFrontDistributionDetails ::
  AwsCloudFrontDistributionDetails
newAwsCloudFrontDistributionDetails =
  AwsCloudFrontDistributionDetails'
    { domainName =
        Prelude.Nothing,
      status = Prelude.Nothing,
      defaultRootObject = Prelude.Nothing,
      viewerCertificate = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      logging = Prelude.Nothing,
      webAclId = Prelude.Nothing,
      originGroups = Prelude.Nothing,
      origins = Prelude.Nothing,
      cacheBehaviors = Prelude.Nothing,
      eTag = Prelude.Nothing,
      defaultCacheBehavior = Prelude.Nothing
    }

-- | The domain name corresponding to the distribution.
awsCloudFrontDistributionDetails_domainName :: Lens.Lens' AwsCloudFrontDistributionDetails (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionDetails_domainName = Lens.lens (\AwsCloudFrontDistributionDetails' {domainName} -> domainName) (\s@AwsCloudFrontDistributionDetails' {} a -> s {domainName = a} :: AwsCloudFrontDistributionDetails)

-- | Indicates the current status of the distribution.
awsCloudFrontDistributionDetails_status :: Lens.Lens' AwsCloudFrontDistributionDetails (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionDetails_status = Lens.lens (\AwsCloudFrontDistributionDetails' {status} -> status) (\s@AwsCloudFrontDistributionDetails' {} a -> s {status = a} :: AwsCloudFrontDistributionDetails)

-- | The object that CloudFront sends in response to requests from the origin
-- (for example, index.html) when a viewer requests the root URL for the
-- distribution (http:\/\/www.example.com) instead of an object in your
-- distribution (http:\/\/www.example.com\/product-description.html).
awsCloudFrontDistributionDetails_defaultRootObject :: Lens.Lens' AwsCloudFrontDistributionDetails (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionDetails_defaultRootObject = Lens.lens (\AwsCloudFrontDistributionDetails' {defaultRootObject} -> defaultRootObject) (\s@AwsCloudFrontDistributionDetails' {} a -> s {defaultRootObject = a} :: AwsCloudFrontDistributionDetails)

-- | Provides information about the TLS\/SSL configuration that the
-- distribution uses to communicate with viewers.
awsCloudFrontDistributionDetails_viewerCertificate :: Lens.Lens' AwsCloudFrontDistributionDetails (Prelude.Maybe AwsCloudFrontDistributionViewerCertificate)
awsCloudFrontDistributionDetails_viewerCertificate = Lens.lens (\AwsCloudFrontDistributionDetails' {viewerCertificate} -> viewerCertificate) (\s@AwsCloudFrontDistributionDetails' {} a -> s {viewerCertificate = a} :: AwsCloudFrontDistributionDetails)

-- | Indicates when that the distribution was last modified.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsCloudFrontDistributionDetails_lastModifiedTime :: Lens.Lens' AwsCloudFrontDistributionDetails (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionDetails_lastModifiedTime = Lens.lens (\AwsCloudFrontDistributionDetails' {lastModifiedTime} -> lastModifiedTime) (\s@AwsCloudFrontDistributionDetails' {} a -> s {lastModifiedTime = a} :: AwsCloudFrontDistributionDetails)

-- | A complex type that controls whether access logs are written for the
-- distribution.
awsCloudFrontDistributionDetails_logging :: Lens.Lens' AwsCloudFrontDistributionDetails (Prelude.Maybe AwsCloudFrontDistributionLogging)
awsCloudFrontDistributionDetails_logging = Lens.lens (\AwsCloudFrontDistributionDetails' {logging} -> logging) (\s@AwsCloudFrontDistributionDetails' {} a -> s {logging = a} :: AwsCloudFrontDistributionDetails)

-- | A unique identifier that specifies the WAF web ACL, if any, to associate
-- with this distribution.
awsCloudFrontDistributionDetails_webAclId :: Lens.Lens' AwsCloudFrontDistributionDetails (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionDetails_webAclId = Lens.lens (\AwsCloudFrontDistributionDetails' {webAclId} -> webAclId) (\s@AwsCloudFrontDistributionDetails' {} a -> s {webAclId = a} :: AwsCloudFrontDistributionDetails)

-- | Provides information about the origin groups in the distribution.
awsCloudFrontDistributionDetails_originGroups :: Lens.Lens' AwsCloudFrontDistributionDetails (Prelude.Maybe AwsCloudFrontDistributionOriginGroups)
awsCloudFrontDistributionDetails_originGroups = Lens.lens (\AwsCloudFrontDistributionDetails' {originGroups} -> originGroups) (\s@AwsCloudFrontDistributionDetails' {} a -> s {originGroups = a} :: AwsCloudFrontDistributionDetails)

-- | A complex type that contains information about origins for this
-- distribution.
awsCloudFrontDistributionDetails_origins :: Lens.Lens' AwsCloudFrontDistributionDetails (Prelude.Maybe AwsCloudFrontDistributionOrigins)
awsCloudFrontDistributionDetails_origins = Lens.lens (\AwsCloudFrontDistributionDetails' {origins} -> origins) (\s@AwsCloudFrontDistributionDetails' {} a -> s {origins = a} :: AwsCloudFrontDistributionDetails)

-- | Provides information about the cache configuration for the distribution.
awsCloudFrontDistributionDetails_cacheBehaviors :: Lens.Lens' AwsCloudFrontDistributionDetails (Prelude.Maybe AwsCloudFrontDistributionCacheBehaviors)
awsCloudFrontDistributionDetails_cacheBehaviors = Lens.lens (\AwsCloudFrontDistributionDetails' {cacheBehaviors} -> cacheBehaviors) (\s@AwsCloudFrontDistributionDetails' {} a -> s {cacheBehaviors = a} :: AwsCloudFrontDistributionDetails)

-- | The entity tag is a hash of the object.
awsCloudFrontDistributionDetails_eTag :: Lens.Lens' AwsCloudFrontDistributionDetails (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionDetails_eTag = Lens.lens (\AwsCloudFrontDistributionDetails' {eTag} -> eTag) (\s@AwsCloudFrontDistributionDetails' {} a -> s {eTag = a} :: AwsCloudFrontDistributionDetails)

-- | The default cache behavior for the configuration.
awsCloudFrontDistributionDetails_defaultCacheBehavior :: Lens.Lens' AwsCloudFrontDistributionDetails (Prelude.Maybe AwsCloudFrontDistributionDefaultCacheBehavior)
awsCloudFrontDistributionDetails_defaultCacheBehavior = Lens.lens (\AwsCloudFrontDistributionDetails' {defaultCacheBehavior} -> defaultCacheBehavior) (\s@AwsCloudFrontDistributionDetails' {} a -> s {defaultCacheBehavior = a} :: AwsCloudFrontDistributionDetails)

instance
  Data.FromJSON
    AwsCloudFrontDistributionDetails
  where
  parseJSON =
    Data.withObject
      "AwsCloudFrontDistributionDetails"
      ( \x ->
          AwsCloudFrontDistributionDetails'
            Prelude.<$> (x Data..:? "DomainName")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "DefaultRootObject")
            Prelude.<*> (x Data..:? "ViewerCertificate")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "Logging")
            Prelude.<*> (x Data..:? "WebAclId")
            Prelude.<*> (x Data..:? "OriginGroups")
            Prelude.<*> (x Data..:? "Origins")
            Prelude.<*> (x Data..:? "CacheBehaviors")
            Prelude.<*> (x Data..:? "ETag")
            Prelude.<*> (x Data..:? "DefaultCacheBehavior")
      )

instance
  Prelude.Hashable
    AwsCloudFrontDistributionDetails
  where
  hashWithSalt
    _salt
    AwsCloudFrontDistributionDetails' {..} =
      _salt `Prelude.hashWithSalt` domainName
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` defaultRootObject
        `Prelude.hashWithSalt` viewerCertificate
        `Prelude.hashWithSalt` lastModifiedTime
        `Prelude.hashWithSalt` logging
        `Prelude.hashWithSalt` webAclId
        `Prelude.hashWithSalt` originGroups
        `Prelude.hashWithSalt` origins
        `Prelude.hashWithSalt` cacheBehaviors
        `Prelude.hashWithSalt` eTag
        `Prelude.hashWithSalt` defaultCacheBehavior

instance
  Prelude.NFData
    AwsCloudFrontDistributionDetails
  where
  rnf AwsCloudFrontDistributionDetails' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf defaultRootObject
      `Prelude.seq` Prelude.rnf viewerCertificate
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf logging
      `Prelude.seq` Prelude.rnf webAclId
      `Prelude.seq` Prelude.rnf originGroups
      `Prelude.seq` Prelude.rnf origins
      `Prelude.seq` Prelude.rnf cacheBehaviors
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf defaultCacheBehavior

instance Data.ToJSON AwsCloudFrontDistributionDetails where
  toJSON AwsCloudFrontDistributionDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DomainName" Data..=) Prelude.<$> domainName,
            ("Status" Data..=) Prelude.<$> status,
            ("DefaultRootObject" Data..=)
              Prelude.<$> defaultRootObject,
            ("ViewerCertificate" Data..=)
              Prelude.<$> viewerCertificate,
            ("LastModifiedTime" Data..=)
              Prelude.<$> lastModifiedTime,
            ("Logging" Data..=) Prelude.<$> logging,
            ("WebAclId" Data..=) Prelude.<$> webAclId,
            ("OriginGroups" Data..=) Prelude.<$> originGroups,
            ("Origins" Data..=) Prelude.<$> origins,
            ("CacheBehaviors" Data..=)
              Prelude.<$> cacheBehaviors,
            ("ETag" Data..=) Prelude.<$> eTag,
            ("DefaultCacheBehavior" Data..=)
              Prelude.<$> defaultCacheBehavior
          ]
      )
