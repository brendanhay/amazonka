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
-- Module      : Network.AWS.CloudFront.Types.Origin
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Origin where

import Network.AWS.CloudFront.Types.CustomHeaders
import Network.AWS.CloudFront.Types.CustomOriginConfig
import Network.AWS.CloudFront.Types.OriginShield
import Network.AWS.CloudFront.Types.S3OriginConfig
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An origin.
--
-- An origin is the location where content is stored, and from which
-- CloudFront gets content to serve to viewers. To specify an origin:
--
-- -   Use @S3OriginConfig@ to specify an Amazon S3 bucket that is not
--     configured with static website hosting.
--
-- -   Use @CustomOriginConfig@ to specify all other kinds of origins,
--     including:
--
--     -   An Amazon S3 bucket that is configured with static website
--         hosting
--
--     -   An Elastic Load Balancing load balancer
--
--     -   An AWS Elemental MediaPackage endpoint
--
--     -   An AWS Elemental MediaStore container
--
--     -   Any other HTTP server, running on an Amazon EC2 instance or any
--         other kind of host
--
-- For the current maximum number of origins that you can specify per
-- distribution, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html#limits-web-distributions General Quotas on Web Distributions>
-- in the /Amazon CloudFront Developer Guide/ (quotas were formerly
-- referred to as limits).
--
-- /See:/ 'newOrigin' smart constructor.
data Origin = Origin'
  { -- | An optional path that CloudFront appends to the origin domain name when
    -- CloudFront requests content from the origin.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginPath Origin Path>
    -- in the /Amazon CloudFront Developer Guide/.
    originPath :: Core.Maybe Core.Text,
    -- | The number of times that CloudFront attempts to connect to the origin.
    -- The minimum number is 1, the maximum is 3, and the default (if you don’t
    -- specify otherwise) is 3.
    --
    -- For a custom origin (including an Amazon S3 bucket that’s configured
    -- with static website hosting), this value also specifies the number of
    -- times that CloudFront attempts to get a response from the origin, in the
    -- case of an
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginResponseTimeout Origin Response Timeout>.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#origin-connection-attempts Origin Connection Attempts>
    -- in the /Amazon CloudFront Developer Guide/.
    connectionAttempts :: Core.Maybe Core.Int,
    -- | The number of seconds that CloudFront waits when trying to establish a
    -- connection to the origin. The minimum timeout is 1 second, the maximum
    -- is 10 seconds, and the default (if you don’t specify otherwise) is 10
    -- seconds.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#origin-connection-timeout Origin Connection Timeout>
    -- in the /Amazon CloudFront Developer Guide/.
    connectionTimeout :: Core.Maybe Core.Int,
    -- | A list of HTTP header names and values that CloudFront adds to the
    -- requests that it sends to the origin.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/add-origin-custom-headers.html Adding Custom Headers to Origin Requests>
    -- in the /Amazon CloudFront Developer Guide/.
    customHeaders :: Core.Maybe CustomHeaders,
    -- | Use this type to specify an origin that is an Amazon S3 bucket that is
    -- not configured with static website hosting. To specify any other type of
    -- origin, including an Amazon S3 bucket that is configured with static
    -- website hosting, use the @CustomOriginConfig@ type instead.
    s3OriginConfig :: Core.Maybe S3OriginConfig,
    -- | CloudFront Origin Shield. Using Origin Shield can help reduce the load
    -- on your origin.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/origin-shield.html Using Origin Shield>
    -- in the /Amazon CloudFront Developer Guide/.
    originShield :: Core.Maybe OriginShield,
    -- | Use this type to specify an origin that is not an Amazon S3 bucket, with
    -- one exception. If the Amazon S3 bucket is configured with static website
    -- hosting, use this type. If the Amazon S3 bucket is not configured with
    -- static website hosting, use the @S3OriginConfig@ type instead.
    customOriginConfig :: Core.Maybe CustomOriginConfig,
    -- | A unique identifier for the origin. This value must be unique within the
    -- distribution.
    --
    -- Use this value to specify the @TargetOriginId@ in a @CacheBehavior@ or
    -- @DefaultCacheBehavior@.
    id :: Core.Text,
    -- | The domain name for the origin.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesDomainName Origin Domain Name>
    -- in the /Amazon CloudFront Developer Guide/.
    domainName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Origin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'originPath', 'origin_originPath' - An optional path that CloudFront appends to the origin domain name when
-- CloudFront requests content from the origin.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginPath Origin Path>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'connectionAttempts', 'origin_connectionAttempts' - The number of times that CloudFront attempts to connect to the origin.
-- The minimum number is 1, the maximum is 3, and the default (if you don’t
-- specify otherwise) is 3.
--
-- For a custom origin (including an Amazon S3 bucket that’s configured
-- with static website hosting), this value also specifies the number of
-- times that CloudFront attempts to get a response from the origin, in the
-- case of an
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginResponseTimeout Origin Response Timeout>.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#origin-connection-attempts Origin Connection Attempts>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'connectionTimeout', 'origin_connectionTimeout' - The number of seconds that CloudFront waits when trying to establish a
-- connection to the origin. The minimum timeout is 1 second, the maximum
-- is 10 seconds, and the default (if you don’t specify otherwise) is 10
-- seconds.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#origin-connection-timeout Origin Connection Timeout>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'customHeaders', 'origin_customHeaders' - A list of HTTP header names and values that CloudFront adds to the
-- requests that it sends to the origin.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/add-origin-custom-headers.html Adding Custom Headers to Origin Requests>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 's3OriginConfig', 'origin_s3OriginConfig' - Use this type to specify an origin that is an Amazon S3 bucket that is
-- not configured with static website hosting. To specify any other type of
-- origin, including an Amazon S3 bucket that is configured with static
-- website hosting, use the @CustomOriginConfig@ type instead.
--
-- 'originShield', 'origin_originShield' - CloudFront Origin Shield. Using Origin Shield can help reduce the load
-- on your origin.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/origin-shield.html Using Origin Shield>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'customOriginConfig', 'origin_customOriginConfig' - Use this type to specify an origin that is not an Amazon S3 bucket, with
-- one exception. If the Amazon S3 bucket is configured with static website
-- hosting, use this type. If the Amazon S3 bucket is not configured with
-- static website hosting, use the @S3OriginConfig@ type instead.
--
-- 'id', 'origin_id' - A unique identifier for the origin. This value must be unique within the
-- distribution.
--
-- Use this value to specify the @TargetOriginId@ in a @CacheBehavior@ or
-- @DefaultCacheBehavior@.
--
-- 'domainName', 'origin_domainName' - The domain name for the origin.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesDomainName Origin Domain Name>
-- in the /Amazon CloudFront Developer Guide/.
newOrigin ::
  -- | 'id'
  Core.Text ->
  -- | 'domainName'
  Core.Text ->
  Origin
newOrigin pId_ pDomainName_ =
  Origin'
    { originPath = Core.Nothing,
      connectionAttempts = Core.Nothing,
      connectionTimeout = Core.Nothing,
      customHeaders = Core.Nothing,
      s3OriginConfig = Core.Nothing,
      originShield = Core.Nothing,
      customOriginConfig = Core.Nothing,
      id = pId_,
      domainName = pDomainName_
    }

-- | An optional path that CloudFront appends to the origin domain name when
-- CloudFront requests content from the origin.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginPath Origin Path>
-- in the /Amazon CloudFront Developer Guide/.
origin_originPath :: Lens.Lens' Origin (Core.Maybe Core.Text)
origin_originPath = Lens.lens (\Origin' {originPath} -> originPath) (\s@Origin' {} a -> s {originPath = a} :: Origin)

-- | The number of times that CloudFront attempts to connect to the origin.
-- The minimum number is 1, the maximum is 3, and the default (if you don’t
-- specify otherwise) is 3.
--
-- For a custom origin (including an Amazon S3 bucket that’s configured
-- with static website hosting), this value also specifies the number of
-- times that CloudFront attempts to get a response from the origin, in the
-- case of an
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginResponseTimeout Origin Response Timeout>.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#origin-connection-attempts Origin Connection Attempts>
-- in the /Amazon CloudFront Developer Guide/.
origin_connectionAttempts :: Lens.Lens' Origin (Core.Maybe Core.Int)
origin_connectionAttempts = Lens.lens (\Origin' {connectionAttempts} -> connectionAttempts) (\s@Origin' {} a -> s {connectionAttempts = a} :: Origin)

-- | The number of seconds that CloudFront waits when trying to establish a
-- connection to the origin. The minimum timeout is 1 second, the maximum
-- is 10 seconds, and the default (if you don’t specify otherwise) is 10
-- seconds.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#origin-connection-timeout Origin Connection Timeout>
-- in the /Amazon CloudFront Developer Guide/.
origin_connectionTimeout :: Lens.Lens' Origin (Core.Maybe Core.Int)
origin_connectionTimeout = Lens.lens (\Origin' {connectionTimeout} -> connectionTimeout) (\s@Origin' {} a -> s {connectionTimeout = a} :: Origin)

-- | A list of HTTP header names and values that CloudFront adds to the
-- requests that it sends to the origin.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/add-origin-custom-headers.html Adding Custom Headers to Origin Requests>
-- in the /Amazon CloudFront Developer Guide/.
origin_customHeaders :: Lens.Lens' Origin (Core.Maybe CustomHeaders)
origin_customHeaders = Lens.lens (\Origin' {customHeaders} -> customHeaders) (\s@Origin' {} a -> s {customHeaders = a} :: Origin)

-- | Use this type to specify an origin that is an Amazon S3 bucket that is
-- not configured with static website hosting. To specify any other type of
-- origin, including an Amazon S3 bucket that is configured with static
-- website hosting, use the @CustomOriginConfig@ type instead.
origin_s3OriginConfig :: Lens.Lens' Origin (Core.Maybe S3OriginConfig)
origin_s3OriginConfig = Lens.lens (\Origin' {s3OriginConfig} -> s3OriginConfig) (\s@Origin' {} a -> s {s3OriginConfig = a} :: Origin)

-- | CloudFront Origin Shield. Using Origin Shield can help reduce the load
-- on your origin.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/origin-shield.html Using Origin Shield>
-- in the /Amazon CloudFront Developer Guide/.
origin_originShield :: Lens.Lens' Origin (Core.Maybe OriginShield)
origin_originShield = Lens.lens (\Origin' {originShield} -> originShield) (\s@Origin' {} a -> s {originShield = a} :: Origin)

-- | Use this type to specify an origin that is not an Amazon S3 bucket, with
-- one exception. If the Amazon S3 bucket is configured with static website
-- hosting, use this type. If the Amazon S3 bucket is not configured with
-- static website hosting, use the @S3OriginConfig@ type instead.
origin_customOriginConfig :: Lens.Lens' Origin (Core.Maybe CustomOriginConfig)
origin_customOriginConfig = Lens.lens (\Origin' {customOriginConfig} -> customOriginConfig) (\s@Origin' {} a -> s {customOriginConfig = a} :: Origin)

-- | A unique identifier for the origin. This value must be unique within the
-- distribution.
--
-- Use this value to specify the @TargetOriginId@ in a @CacheBehavior@ or
-- @DefaultCacheBehavior@.
origin_id :: Lens.Lens' Origin Core.Text
origin_id = Lens.lens (\Origin' {id} -> id) (\s@Origin' {} a -> s {id = a} :: Origin)

-- | The domain name for the origin.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesDomainName Origin Domain Name>
-- in the /Amazon CloudFront Developer Guide/.
origin_domainName :: Lens.Lens' Origin Core.Text
origin_domainName = Lens.lens (\Origin' {domainName} -> domainName) (\s@Origin' {} a -> s {domainName = a} :: Origin)

instance Core.FromXML Origin where
  parseXML x =
    Origin'
      Core.<$> (x Core..@? "OriginPath")
      Core.<*> (x Core..@? "ConnectionAttempts")
      Core.<*> (x Core..@? "ConnectionTimeout")
      Core.<*> (x Core..@? "CustomHeaders")
      Core.<*> (x Core..@? "S3OriginConfig")
      Core.<*> (x Core..@? "OriginShield")
      Core.<*> (x Core..@? "CustomOriginConfig")
      Core.<*> (x Core..@ "Id")
      Core.<*> (x Core..@ "DomainName")

instance Core.Hashable Origin

instance Core.NFData Origin

instance Core.ToXML Origin where
  toXML Origin' {..} =
    Core.mconcat
      [ "OriginPath" Core.@= originPath,
        "ConnectionAttempts" Core.@= connectionAttempts,
        "ConnectionTimeout" Core.@= connectionTimeout,
        "CustomHeaders" Core.@= customHeaders,
        "S3OriginConfig" Core.@= s3OriginConfig,
        "OriginShield" Core.@= originShield,
        "CustomOriginConfig" Core.@= customOriginConfig,
        "Id" Core.@= id,
        "DomainName" Core.@= domainName
      ]
