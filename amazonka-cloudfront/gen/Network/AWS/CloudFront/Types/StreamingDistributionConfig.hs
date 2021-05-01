{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudFront.Types.StreamingDistributionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.StreamingDistributionConfig where

import Network.AWS.CloudFront.Types.Aliases
import Network.AWS.CloudFront.Types.PriceClass
import Network.AWS.CloudFront.Types.S3Origin
import Network.AWS.CloudFront.Types.StreamingLoggingConfig
import Network.AWS.CloudFront.Types.TrustedSigners
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The RTMP distribution\'s configuration information.
--
-- /See:/ 'newStreamingDistributionConfig' smart constructor.
data StreamingDistributionConfig = StreamingDistributionConfig'
  { -- | A complex type that contains information about price class for this
    -- streaming distribution.
    priceClass :: Prelude.Maybe PriceClass,
    -- | A complex type that controls whether access logs are written for the
    -- streaming distribution.
    logging :: Prelude.Maybe StreamingLoggingConfig,
    -- | A complex type that contains information about CNAMEs (alternate domain
    -- names), if any, for this streaming distribution.
    aliases :: Prelude.Maybe Aliases,
    -- | A unique value (for example, a date-time stamp) that ensures that the
    -- request can\'t be replayed.
    --
    -- If the value of @CallerReference@ is new (regardless of the content of
    -- the @StreamingDistributionConfig@ object), CloudFront creates a new
    -- distribution.
    --
    -- If @CallerReference@ is a value that you already sent in a previous
    -- request to create a distribution, CloudFront returns a
    -- @DistributionAlreadyExists@ error.
    callerReference :: Prelude.Text,
    -- | A complex type that contains information about the Amazon S3 bucket from
    -- which you want CloudFront to get your media files for distribution.
    s3Origin :: S3Origin,
    -- | Any comments you want to include about the streaming distribution.
    comment :: Prelude.Text,
    -- | A complex type that specifies any AWS accounts that you want to permit
    -- to create signed URLs for private content. If you want the distribution
    -- to use signed URLs, include this element; if you want the distribution
    -- to use public URLs, remove this element. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront>
    -- in the /Amazon CloudFront Developer Guide/.
    trustedSigners :: TrustedSigners,
    -- | Whether the streaming distribution is enabled to accept user requests
    -- for content.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StreamingDistributionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'priceClass', 'streamingDistributionConfig_priceClass' - A complex type that contains information about price class for this
-- streaming distribution.
--
-- 'logging', 'streamingDistributionConfig_logging' - A complex type that controls whether access logs are written for the
-- streaming distribution.
--
-- 'aliases', 'streamingDistributionConfig_aliases' - A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this streaming distribution.
--
-- 'callerReference', 'streamingDistributionConfig_callerReference' - A unique value (for example, a date-time stamp) that ensures that the
-- request can\'t be replayed.
--
-- If the value of @CallerReference@ is new (regardless of the content of
-- the @StreamingDistributionConfig@ object), CloudFront creates a new
-- distribution.
--
-- If @CallerReference@ is a value that you already sent in a previous
-- request to create a distribution, CloudFront returns a
-- @DistributionAlreadyExists@ error.
--
-- 's3Origin', 'streamingDistributionConfig_s3Origin' - A complex type that contains information about the Amazon S3 bucket from
-- which you want CloudFront to get your media files for distribution.
--
-- 'comment', 'streamingDistributionConfig_comment' - Any comments you want to include about the streaming distribution.
--
-- 'trustedSigners', 'streamingDistributionConfig_trustedSigners' - A complex type that specifies any AWS accounts that you want to permit
-- to create signed URLs for private content. If you want the distribution
-- to use signed URLs, include this element; if you want the distribution
-- to use public URLs, remove this element. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'enabled', 'streamingDistributionConfig_enabled' - Whether the streaming distribution is enabled to accept user requests
-- for content.
newStreamingDistributionConfig ::
  -- | 'callerReference'
  Prelude.Text ->
  -- | 's3Origin'
  S3Origin ->
  -- | 'comment'
  Prelude.Text ->
  -- | 'trustedSigners'
  TrustedSigners ->
  -- | 'enabled'
  Prelude.Bool ->
  StreamingDistributionConfig
newStreamingDistributionConfig
  pCallerReference_
  pS3Origin_
  pComment_
  pTrustedSigners_
  pEnabled_ =
    StreamingDistributionConfig'
      { priceClass =
          Prelude.Nothing,
        logging = Prelude.Nothing,
        aliases = Prelude.Nothing,
        callerReference = pCallerReference_,
        s3Origin = pS3Origin_,
        comment = pComment_,
        trustedSigners = pTrustedSigners_,
        enabled = pEnabled_
      }

-- | A complex type that contains information about price class for this
-- streaming distribution.
streamingDistributionConfig_priceClass :: Lens.Lens' StreamingDistributionConfig (Prelude.Maybe PriceClass)
streamingDistributionConfig_priceClass = Lens.lens (\StreamingDistributionConfig' {priceClass} -> priceClass) (\s@StreamingDistributionConfig' {} a -> s {priceClass = a} :: StreamingDistributionConfig)

-- | A complex type that controls whether access logs are written for the
-- streaming distribution.
streamingDistributionConfig_logging :: Lens.Lens' StreamingDistributionConfig (Prelude.Maybe StreamingLoggingConfig)
streamingDistributionConfig_logging = Lens.lens (\StreamingDistributionConfig' {logging} -> logging) (\s@StreamingDistributionConfig' {} a -> s {logging = a} :: StreamingDistributionConfig)

-- | A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this streaming distribution.
streamingDistributionConfig_aliases :: Lens.Lens' StreamingDistributionConfig (Prelude.Maybe Aliases)
streamingDistributionConfig_aliases = Lens.lens (\StreamingDistributionConfig' {aliases} -> aliases) (\s@StreamingDistributionConfig' {} a -> s {aliases = a} :: StreamingDistributionConfig)

-- | A unique value (for example, a date-time stamp) that ensures that the
-- request can\'t be replayed.
--
-- If the value of @CallerReference@ is new (regardless of the content of
-- the @StreamingDistributionConfig@ object), CloudFront creates a new
-- distribution.
--
-- If @CallerReference@ is a value that you already sent in a previous
-- request to create a distribution, CloudFront returns a
-- @DistributionAlreadyExists@ error.
streamingDistributionConfig_callerReference :: Lens.Lens' StreamingDistributionConfig Prelude.Text
streamingDistributionConfig_callerReference = Lens.lens (\StreamingDistributionConfig' {callerReference} -> callerReference) (\s@StreamingDistributionConfig' {} a -> s {callerReference = a} :: StreamingDistributionConfig)

-- | A complex type that contains information about the Amazon S3 bucket from
-- which you want CloudFront to get your media files for distribution.
streamingDistributionConfig_s3Origin :: Lens.Lens' StreamingDistributionConfig S3Origin
streamingDistributionConfig_s3Origin = Lens.lens (\StreamingDistributionConfig' {s3Origin} -> s3Origin) (\s@StreamingDistributionConfig' {} a -> s {s3Origin = a} :: StreamingDistributionConfig)

-- | Any comments you want to include about the streaming distribution.
streamingDistributionConfig_comment :: Lens.Lens' StreamingDistributionConfig Prelude.Text
streamingDistributionConfig_comment = Lens.lens (\StreamingDistributionConfig' {comment} -> comment) (\s@StreamingDistributionConfig' {} a -> s {comment = a} :: StreamingDistributionConfig)

-- | A complex type that specifies any AWS accounts that you want to permit
-- to create signed URLs for private content. If you want the distribution
-- to use signed URLs, include this element; if you want the distribution
-- to use public URLs, remove this element. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront>
-- in the /Amazon CloudFront Developer Guide/.
streamingDistributionConfig_trustedSigners :: Lens.Lens' StreamingDistributionConfig TrustedSigners
streamingDistributionConfig_trustedSigners = Lens.lens (\StreamingDistributionConfig' {trustedSigners} -> trustedSigners) (\s@StreamingDistributionConfig' {} a -> s {trustedSigners = a} :: StreamingDistributionConfig)

-- | Whether the streaming distribution is enabled to accept user requests
-- for content.
streamingDistributionConfig_enabled :: Lens.Lens' StreamingDistributionConfig Prelude.Bool
streamingDistributionConfig_enabled = Lens.lens (\StreamingDistributionConfig' {enabled} -> enabled) (\s@StreamingDistributionConfig' {} a -> s {enabled = a} :: StreamingDistributionConfig)

instance Prelude.FromXML StreamingDistributionConfig where
  parseXML x =
    StreamingDistributionConfig'
      Prelude.<$> (x Prelude..@? "PriceClass")
      Prelude.<*> (x Prelude..@? "Logging")
      Prelude.<*> (x Prelude..@? "Aliases")
      Prelude.<*> (x Prelude..@ "CallerReference")
      Prelude.<*> (x Prelude..@ "S3Origin")
      Prelude.<*> (x Prelude..@ "Comment")
      Prelude.<*> (x Prelude..@ "TrustedSigners")
      Prelude.<*> (x Prelude..@ "Enabled")

instance Prelude.Hashable StreamingDistributionConfig

instance Prelude.NFData StreamingDistributionConfig

instance Prelude.ToXML StreamingDistributionConfig where
  toXML StreamingDistributionConfig' {..} =
    Prelude.mconcat
      [ "PriceClass" Prelude.@= priceClass,
        "Logging" Prelude.@= logging,
        "Aliases" Prelude.@= aliases,
        "CallerReference" Prelude.@= callerReference,
        "S3Origin" Prelude.@= s3Origin,
        "Comment" Prelude.@= comment,
        "TrustedSigners" Prelude.@= trustedSigners,
        "Enabled" Prelude.@= enabled
      ]
