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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
  { -- | Provides information about the cache configuration for the distribution.
    cacheBehaviors :: Prelude.Maybe AwsCloudFrontDistributionCacheBehaviors,
    -- | The default cache behavior for the configuration.
    defaultCacheBehavior :: Prelude.Maybe AwsCloudFrontDistributionDefaultCacheBehavior,
    -- | The object that CloudFront sends in response to requests from the origin
    -- (for example, index.html) when a viewer requests the root URL for the
    -- distribution (http:\/\/www.example.com) instead of an object in your
    -- distribution (http:\/\/www.example.com\/product-description.html).
    defaultRootObject :: Prelude.Maybe Prelude.Text,
    -- | The domain name corresponding to the distribution.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The entity tag is a hash of the object.
    eTag :: Prelude.Maybe Prelude.Text,
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
    -- | Provides information about the origin groups in the distribution.
    originGroups :: Prelude.Maybe AwsCloudFrontDistributionOriginGroups,
    -- | A complex type that contains information about origins for this
    -- distribution.
    origins :: Prelude.Maybe AwsCloudFrontDistributionOrigins,
    -- | Indicates the current status of the distribution.
    status :: Prelude.Maybe Prelude.Text,
    -- | Provides information about the TLS\/SSL configuration that the
    -- distribution uses to communicate with viewers.
    viewerCertificate :: Prelude.Maybe AwsCloudFrontDistributionViewerCertificate,
    -- | A unique identifier that specifies the WAF web ACL, if any, to associate
    -- with this distribution.
    webAclId :: Prelude.Maybe Prelude.Text
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
-- 'cacheBehaviors', 'awsCloudFrontDistributionDetails_cacheBehaviors' - Provides information about the cache configuration for the distribution.
--
-- 'defaultCacheBehavior', 'awsCloudFrontDistributionDetails_defaultCacheBehavior' - The default cache behavior for the configuration.
--
-- 'defaultRootObject', 'awsCloudFrontDistributionDetails_defaultRootObject' - The object that CloudFront sends in response to requests from the origin
-- (for example, index.html) when a viewer requests the root URL for the
-- distribution (http:\/\/www.example.com) instead of an object in your
-- distribution (http:\/\/www.example.com\/product-description.html).
--
-- 'domainName', 'awsCloudFrontDistributionDetails_domainName' - The domain name corresponding to the distribution.
--
-- 'eTag', 'awsCloudFrontDistributionDetails_eTag' - The entity tag is a hash of the object.
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
-- 'originGroups', 'awsCloudFrontDistributionDetails_originGroups' - Provides information about the origin groups in the distribution.
--
-- 'origins', 'awsCloudFrontDistributionDetails_origins' - A complex type that contains information about origins for this
-- distribution.
--
-- 'status', 'awsCloudFrontDistributionDetails_status' - Indicates the current status of the distribution.
--
-- 'viewerCertificate', 'awsCloudFrontDistributionDetails_viewerCertificate' - Provides information about the TLS\/SSL configuration that the
-- distribution uses to communicate with viewers.
--
-- 'webAclId', 'awsCloudFrontDistributionDetails_webAclId' - A unique identifier that specifies the WAF web ACL, if any, to associate
-- with this distribution.
newAwsCloudFrontDistributionDetails ::
  AwsCloudFrontDistributionDetails
newAwsCloudFrontDistributionDetails =
  AwsCloudFrontDistributionDetails'
    { cacheBehaviors =
        Prelude.Nothing,
      defaultCacheBehavior = Prelude.Nothing,
      defaultRootObject = Prelude.Nothing,
      domainName = Prelude.Nothing,
      eTag = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      logging = Prelude.Nothing,
      originGroups = Prelude.Nothing,
      origins = Prelude.Nothing,
      status = Prelude.Nothing,
      viewerCertificate = Prelude.Nothing,
      webAclId = Prelude.Nothing
    }

-- | Provides information about the cache configuration for the distribution.
awsCloudFrontDistributionDetails_cacheBehaviors :: Lens.Lens' AwsCloudFrontDistributionDetails (Prelude.Maybe AwsCloudFrontDistributionCacheBehaviors)
awsCloudFrontDistributionDetails_cacheBehaviors = Lens.lens (\AwsCloudFrontDistributionDetails' {cacheBehaviors} -> cacheBehaviors) (\s@AwsCloudFrontDistributionDetails' {} a -> s {cacheBehaviors = a} :: AwsCloudFrontDistributionDetails)

-- | The default cache behavior for the configuration.
awsCloudFrontDistributionDetails_defaultCacheBehavior :: Lens.Lens' AwsCloudFrontDistributionDetails (Prelude.Maybe AwsCloudFrontDistributionDefaultCacheBehavior)
awsCloudFrontDistributionDetails_defaultCacheBehavior = Lens.lens (\AwsCloudFrontDistributionDetails' {defaultCacheBehavior} -> defaultCacheBehavior) (\s@AwsCloudFrontDistributionDetails' {} a -> s {defaultCacheBehavior = a} :: AwsCloudFrontDistributionDetails)

-- | The object that CloudFront sends in response to requests from the origin
-- (for example, index.html) when a viewer requests the root URL for the
-- distribution (http:\/\/www.example.com) instead of an object in your
-- distribution (http:\/\/www.example.com\/product-description.html).
awsCloudFrontDistributionDetails_defaultRootObject :: Lens.Lens' AwsCloudFrontDistributionDetails (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionDetails_defaultRootObject = Lens.lens (\AwsCloudFrontDistributionDetails' {defaultRootObject} -> defaultRootObject) (\s@AwsCloudFrontDistributionDetails' {} a -> s {defaultRootObject = a} :: AwsCloudFrontDistributionDetails)

-- | The domain name corresponding to the distribution.
awsCloudFrontDistributionDetails_domainName :: Lens.Lens' AwsCloudFrontDistributionDetails (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionDetails_domainName = Lens.lens (\AwsCloudFrontDistributionDetails' {domainName} -> domainName) (\s@AwsCloudFrontDistributionDetails' {} a -> s {domainName = a} :: AwsCloudFrontDistributionDetails)

-- | The entity tag is a hash of the object.
awsCloudFrontDistributionDetails_eTag :: Lens.Lens' AwsCloudFrontDistributionDetails (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionDetails_eTag = Lens.lens (\AwsCloudFrontDistributionDetails' {eTag} -> eTag) (\s@AwsCloudFrontDistributionDetails' {} a -> s {eTag = a} :: AwsCloudFrontDistributionDetails)

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

-- | Provides information about the origin groups in the distribution.
awsCloudFrontDistributionDetails_originGroups :: Lens.Lens' AwsCloudFrontDistributionDetails (Prelude.Maybe AwsCloudFrontDistributionOriginGroups)
awsCloudFrontDistributionDetails_originGroups = Lens.lens (\AwsCloudFrontDistributionDetails' {originGroups} -> originGroups) (\s@AwsCloudFrontDistributionDetails' {} a -> s {originGroups = a} :: AwsCloudFrontDistributionDetails)

-- | A complex type that contains information about origins for this
-- distribution.
awsCloudFrontDistributionDetails_origins :: Lens.Lens' AwsCloudFrontDistributionDetails (Prelude.Maybe AwsCloudFrontDistributionOrigins)
awsCloudFrontDistributionDetails_origins = Lens.lens (\AwsCloudFrontDistributionDetails' {origins} -> origins) (\s@AwsCloudFrontDistributionDetails' {} a -> s {origins = a} :: AwsCloudFrontDistributionDetails)

-- | Indicates the current status of the distribution.
awsCloudFrontDistributionDetails_status :: Lens.Lens' AwsCloudFrontDistributionDetails (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionDetails_status = Lens.lens (\AwsCloudFrontDistributionDetails' {status} -> status) (\s@AwsCloudFrontDistributionDetails' {} a -> s {status = a} :: AwsCloudFrontDistributionDetails)

-- | Provides information about the TLS\/SSL configuration that the
-- distribution uses to communicate with viewers.
awsCloudFrontDistributionDetails_viewerCertificate :: Lens.Lens' AwsCloudFrontDistributionDetails (Prelude.Maybe AwsCloudFrontDistributionViewerCertificate)
awsCloudFrontDistributionDetails_viewerCertificate = Lens.lens (\AwsCloudFrontDistributionDetails' {viewerCertificate} -> viewerCertificate) (\s@AwsCloudFrontDistributionDetails' {} a -> s {viewerCertificate = a} :: AwsCloudFrontDistributionDetails)

-- | A unique identifier that specifies the WAF web ACL, if any, to associate
-- with this distribution.
awsCloudFrontDistributionDetails_webAclId :: Lens.Lens' AwsCloudFrontDistributionDetails (Prelude.Maybe Prelude.Text)
awsCloudFrontDistributionDetails_webAclId = Lens.lens (\AwsCloudFrontDistributionDetails' {webAclId} -> webAclId) (\s@AwsCloudFrontDistributionDetails' {} a -> s {webAclId = a} :: AwsCloudFrontDistributionDetails)

instance
  Data.FromJSON
    AwsCloudFrontDistributionDetails
  where
  parseJSON =
    Data.withObject
      "AwsCloudFrontDistributionDetails"
      ( \x ->
          AwsCloudFrontDistributionDetails'
            Prelude.<$> (x Data..:? "CacheBehaviors")
            Prelude.<*> (x Data..:? "DefaultCacheBehavior")
            Prelude.<*> (x Data..:? "DefaultRootObject")
            Prelude.<*> (x Data..:? "DomainName")
            Prelude.<*> (x Data..:? "ETag")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "Logging")
            Prelude.<*> (x Data..:? "OriginGroups")
            Prelude.<*> (x Data..:? "Origins")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "ViewerCertificate")
            Prelude.<*> (x Data..:? "WebAclId")
      )

instance
  Prelude.Hashable
    AwsCloudFrontDistributionDetails
  where
  hashWithSalt
    _salt
    AwsCloudFrontDistributionDetails' {..} =
      _salt
        `Prelude.hashWithSalt` cacheBehaviors
        `Prelude.hashWithSalt` defaultCacheBehavior
        `Prelude.hashWithSalt` defaultRootObject
        `Prelude.hashWithSalt` domainName
        `Prelude.hashWithSalt` eTag
        `Prelude.hashWithSalt` lastModifiedTime
        `Prelude.hashWithSalt` logging
        `Prelude.hashWithSalt` originGroups
        `Prelude.hashWithSalt` origins
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` viewerCertificate
        `Prelude.hashWithSalt` webAclId

instance
  Prelude.NFData
    AwsCloudFrontDistributionDetails
  where
  rnf AwsCloudFrontDistributionDetails' {..} =
    Prelude.rnf cacheBehaviors `Prelude.seq`
      Prelude.rnf defaultCacheBehavior `Prelude.seq`
        Prelude.rnf defaultRootObject `Prelude.seq`
          Prelude.rnf domainName `Prelude.seq`
            Prelude.rnf eTag `Prelude.seq`
              Prelude.rnf lastModifiedTime `Prelude.seq`
                Prelude.rnf logging `Prelude.seq`
                  Prelude.rnf originGroups `Prelude.seq`
                    Prelude.rnf origins `Prelude.seq`
                      Prelude.rnf status `Prelude.seq`
                        Prelude.rnf viewerCertificate `Prelude.seq`
                          Prelude.rnf webAclId

instance Data.ToJSON AwsCloudFrontDistributionDetails where
  toJSON AwsCloudFrontDistributionDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CacheBehaviors" Data..=)
              Prelude.<$> cacheBehaviors,
            ("DefaultCacheBehavior" Data..=)
              Prelude.<$> defaultCacheBehavior,
            ("DefaultRootObject" Data..=)
              Prelude.<$> defaultRootObject,
            ("DomainName" Data..=) Prelude.<$> domainName,
            ("ETag" Data..=) Prelude.<$> eTag,
            ("LastModifiedTime" Data..=)
              Prelude.<$> lastModifiedTime,
            ("Logging" Data..=) Prelude.<$> logging,
            ("OriginGroups" Data..=) Prelude.<$> originGroups,
            ("Origins" Data..=) Prelude.<$> origins,
            ("Status" Data..=) Prelude.<$> status,
            ("ViewerCertificate" Data..=)
              Prelude.<$> viewerCertificate,
            ("WebAclId" Data..=) Prelude.<$> webAclId
          ]
      )
