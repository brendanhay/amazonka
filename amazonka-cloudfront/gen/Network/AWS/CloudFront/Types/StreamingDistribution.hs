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
-- Module      : Network.AWS.CloudFront.Types.StreamingDistribution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.StreamingDistribution where

import Network.AWS.CloudFront.Types.ActiveTrustedSigners
import Network.AWS.CloudFront.Types.StreamingDistributionConfig
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A streaming distribution tells CloudFront where you want RTMP content to
-- be delivered from, and the details about how to track and manage content
-- delivery.
--
-- /See:/ 'newStreamingDistribution' smart constructor.
data StreamingDistribution = StreamingDistribution'
  { -- | The date and time that the distribution was last modified.
    lastModifiedTime :: Core.Maybe Core.ISO8601,
    -- | The identifier for the RTMP distribution. For example:
    -- @EGTXBD79EXAMPLE@.
    id :: Core.Text,
    -- | The ARN (Amazon Resource Name) for the distribution. For example:
    -- @arn:aws:cloudfront::123456789012:distribution\/EDFDVBD632BHDS5@, where
    -- @123456789012@ is your AWS account ID.
    arn :: Core.Text,
    -- | The current status of the RTMP distribution. When the status is
    -- @Deployed@, the distribution\'s information is propagated to all
    -- CloudFront edge locations.
    status :: Core.Text,
    -- | The domain name that corresponds to the streaming distribution, for
    -- example, @s5c39gqb8ow64r.cloudfront.net@.
    domainName :: Core.Text,
    -- | A complex type that lists the AWS accounts, if any, that you included in
    -- the @TrustedSigners@ complex type for this distribution. These are the
    -- accounts that you want to allow to create signed URLs for private
    -- content.
    --
    -- The @Signer@ complex type lists the AWS account number of the trusted
    -- signer or @self@ if the signer is the AWS account that created the
    -- distribution. The @Signer@ element also includes the IDs of any active
    -- CloudFront key pairs that are associated with the trusted signer\'s AWS
    -- account. If no @KeyPairId@ element appears for a @Signer@, that signer
    -- can\'t create signed URLs.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront>
    -- in the /Amazon CloudFront Developer Guide/.
    activeTrustedSigners :: ActiveTrustedSigners,
    -- | The current configuration information for the RTMP distribution.
    streamingDistributionConfig :: StreamingDistributionConfig
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StreamingDistribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedTime', 'streamingDistribution_lastModifiedTime' - The date and time that the distribution was last modified.
--
-- 'id', 'streamingDistribution_id' - The identifier for the RTMP distribution. For example:
-- @EGTXBD79EXAMPLE@.
--
-- 'arn', 'streamingDistribution_arn' - The ARN (Amazon Resource Name) for the distribution. For example:
-- @arn:aws:cloudfront::123456789012:distribution\/EDFDVBD632BHDS5@, where
-- @123456789012@ is your AWS account ID.
--
-- 'status', 'streamingDistribution_status' - The current status of the RTMP distribution. When the status is
-- @Deployed@, the distribution\'s information is propagated to all
-- CloudFront edge locations.
--
-- 'domainName', 'streamingDistribution_domainName' - The domain name that corresponds to the streaming distribution, for
-- example, @s5c39gqb8ow64r.cloudfront.net@.
--
-- 'activeTrustedSigners', 'streamingDistribution_activeTrustedSigners' - A complex type that lists the AWS accounts, if any, that you included in
-- the @TrustedSigners@ complex type for this distribution. These are the
-- accounts that you want to allow to create signed URLs for private
-- content.
--
-- The @Signer@ complex type lists the AWS account number of the trusted
-- signer or @self@ if the signer is the AWS account that created the
-- distribution. The @Signer@ element also includes the IDs of any active
-- CloudFront key pairs that are associated with the trusted signer\'s AWS
-- account. If no @KeyPairId@ element appears for a @Signer@, that signer
-- can\'t create signed URLs.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'streamingDistributionConfig', 'streamingDistribution_streamingDistributionConfig' - The current configuration information for the RTMP distribution.
newStreamingDistribution ::
  -- | 'id'
  Core.Text ->
  -- | 'arn'
  Core.Text ->
  -- | 'status'
  Core.Text ->
  -- | 'domainName'
  Core.Text ->
  -- | 'activeTrustedSigners'
  ActiveTrustedSigners ->
  -- | 'streamingDistributionConfig'
  StreamingDistributionConfig ->
  StreamingDistribution
newStreamingDistribution
  pId_
  pARN_
  pStatus_
  pDomainName_
  pActiveTrustedSigners_
  pStreamingDistributionConfig_ =
    StreamingDistribution'
      { lastModifiedTime =
          Core.Nothing,
        id = pId_,
        arn = pARN_,
        status = pStatus_,
        domainName = pDomainName_,
        activeTrustedSigners = pActiveTrustedSigners_,
        streamingDistributionConfig =
          pStreamingDistributionConfig_
      }

-- | The date and time that the distribution was last modified.
streamingDistribution_lastModifiedTime :: Lens.Lens' StreamingDistribution (Core.Maybe Core.UTCTime)
streamingDistribution_lastModifiedTime = Lens.lens (\StreamingDistribution' {lastModifiedTime} -> lastModifiedTime) (\s@StreamingDistribution' {} a -> s {lastModifiedTime = a} :: StreamingDistribution) Core.. Lens.mapping Core._Time

-- | The identifier for the RTMP distribution. For example:
-- @EGTXBD79EXAMPLE@.
streamingDistribution_id :: Lens.Lens' StreamingDistribution Core.Text
streamingDistribution_id = Lens.lens (\StreamingDistribution' {id} -> id) (\s@StreamingDistribution' {} a -> s {id = a} :: StreamingDistribution)

-- | The ARN (Amazon Resource Name) for the distribution. For example:
-- @arn:aws:cloudfront::123456789012:distribution\/EDFDVBD632BHDS5@, where
-- @123456789012@ is your AWS account ID.
streamingDistribution_arn :: Lens.Lens' StreamingDistribution Core.Text
streamingDistribution_arn = Lens.lens (\StreamingDistribution' {arn} -> arn) (\s@StreamingDistribution' {} a -> s {arn = a} :: StreamingDistribution)

-- | The current status of the RTMP distribution. When the status is
-- @Deployed@, the distribution\'s information is propagated to all
-- CloudFront edge locations.
streamingDistribution_status :: Lens.Lens' StreamingDistribution Core.Text
streamingDistribution_status = Lens.lens (\StreamingDistribution' {status} -> status) (\s@StreamingDistribution' {} a -> s {status = a} :: StreamingDistribution)

-- | The domain name that corresponds to the streaming distribution, for
-- example, @s5c39gqb8ow64r.cloudfront.net@.
streamingDistribution_domainName :: Lens.Lens' StreamingDistribution Core.Text
streamingDistribution_domainName = Lens.lens (\StreamingDistribution' {domainName} -> domainName) (\s@StreamingDistribution' {} a -> s {domainName = a} :: StreamingDistribution)

-- | A complex type that lists the AWS accounts, if any, that you included in
-- the @TrustedSigners@ complex type for this distribution. These are the
-- accounts that you want to allow to create signed URLs for private
-- content.
--
-- The @Signer@ complex type lists the AWS account number of the trusted
-- signer or @self@ if the signer is the AWS account that created the
-- distribution. The @Signer@ element also includes the IDs of any active
-- CloudFront key pairs that are associated with the trusted signer\'s AWS
-- account. If no @KeyPairId@ element appears for a @Signer@, that signer
-- can\'t create signed URLs.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront>
-- in the /Amazon CloudFront Developer Guide/.
streamingDistribution_activeTrustedSigners :: Lens.Lens' StreamingDistribution ActiveTrustedSigners
streamingDistribution_activeTrustedSigners = Lens.lens (\StreamingDistribution' {activeTrustedSigners} -> activeTrustedSigners) (\s@StreamingDistribution' {} a -> s {activeTrustedSigners = a} :: StreamingDistribution)

-- | The current configuration information for the RTMP distribution.
streamingDistribution_streamingDistributionConfig :: Lens.Lens' StreamingDistribution StreamingDistributionConfig
streamingDistribution_streamingDistributionConfig = Lens.lens (\StreamingDistribution' {streamingDistributionConfig} -> streamingDistributionConfig) (\s@StreamingDistribution' {} a -> s {streamingDistributionConfig = a} :: StreamingDistribution)

instance Core.FromXML StreamingDistribution where
  parseXML x =
    StreamingDistribution'
      Core.<$> (x Core..@? "LastModifiedTime")
      Core.<*> (x Core..@ "Id")
      Core.<*> (x Core..@ "ARN")
      Core.<*> (x Core..@ "Status")
      Core.<*> (x Core..@ "DomainName")
      Core.<*> (x Core..@ "ActiveTrustedSigners")
      Core.<*> (x Core..@ "StreamingDistributionConfig")

instance Core.Hashable StreamingDistribution

instance Core.NFData StreamingDistribution
