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
-- Module      : Amazonka.CloudFront.Types.StreamingDistributionSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.StreamingDistributionSummary where

import Amazonka.CloudFront.Types.Aliases
import Amazonka.CloudFront.Types.PriceClass
import Amazonka.CloudFront.Types.S3Origin
import Amazonka.CloudFront.Types.TrustedSigners
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A summary of the information for a CloudFront streaming distribution.
--
-- /See:/ 'newStreamingDistributionSummary' smart constructor.
data StreamingDistributionSummary = StreamingDistributionSummary'
  { -- | The identifier for the distribution, for example, @EDFDVBD632BHDS5@.
    id :: Prelude.Text,
    -- | The ARN (Amazon Resource Name) for the streaming distribution. For
    -- example:
    -- @arn:aws:cloudfront::123456789012:streaming-distribution\/EDFDVBD632BHDS5@,
    -- where @123456789012@ is your Amazon Web Services account ID.
    arn :: Prelude.Text,
    -- | Indicates the current status of the distribution. When the status is
    -- @Deployed@, the distribution\'s information is fully propagated
    -- throughout the Amazon CloudFront system.
    status :: Prelude.Text,
    -- | The date and time the distribution was last modified.
    lastModifiedTime :: Data.ISO8601,
    -- | The domain name corresponding to the distribution, for example,
    -- @d111111abcdef8.cloudfront.net@.
    domainName :: Prelude.Text,
    -- | A complex type that contains information about the Amazon S3 bucket from
    -- which you want CloudFront to get your media files for distribution.
    s3Origin :: S3Origin,
    -- | A complex type that contains information about CNAMEs (alternate domain
    -- names), if any, for this streaming distribution.
    aliases :: Aliases,
    -- | A complex type that specifies the Amazon Web Services accounts, if any,
    -- that you want to allow to create signed URLs for private content. If you
    -- want to require signed URLs in requests for objects in the target origin
    -- that match the @PathPattern@ for this cache behavior, specify @true@ for
    -- @Enabled@, and specify the applicable values for @Quantity@ and
    -- @Items@.If you don\'t want to require signed URLs in requests for
    -- objects that match @PathPattern@, specify @false@ for @Enabled@ and @0@
    -- for @Quantity@. Omit @Items@. To add, change, or remove one or more
    -- trusted signers, change @Enabled@ to @true@ (if it\'s currently
    -- @false@), change @Quantity@ as applicable, and specify all of the
    -- trusted signers that you want to include in the updated distribution.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront>
    -- in the /Amazon CloudFront Developer Guide/.
    trustedSigners :: TrustedSigners,
    -- | The comment originally specified when this distribution was created.
    comment :: Prelude.Text,
    -- | A complex type that contains information about price class for this
    -- streaming distribution.
    priceClass :: PriceClass,
    -- | Whether the distribution is enabled to accept end user requests for
    -- content.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamingDistributionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'streamingDistributionSummary_id' - The identifier for the distribution, for example, @EDFDVBD632BHDS5@.
--
-- 'arn', 'streamingDistributionSummary_arn' - The ARN (Amazon Resource Name) for the streaming distribution. For
-- example:
-- @arn:aws:cloudfront::123456789012:streaming-distribution\/EDFDVBD632BHDS5@,
-- where @123456789012@ is your Amazon Web Services account ID.
--
-- 'status', 'streamingDistributionSummary_status' - Indicates the current status of the distribution. When the status is
-- @Deployed@, the distribution\'s information is fully propagated
-- throughout the Amazon CloudFront system.
--
-- 'lastModifiedTime', 'streamingDistributionSummary_lastModifiedTime' - The date and time the distribution was last modified.
--
-- 'domainName', 'streamingDistributionSummary_domainName' - The domain name corresponding to the distribution, for example,
-- @d111111abcdef8.cloudfront.net@.
--
-- 's3Origin', 'streamingDistributionSummary_s3Origin' - A complex type that contains information about the Amazon S3 bucket from
-- which you want CloudFront to get your media files for distribution.
--
-- 'aliases', 'streamingDistributionSummary_aliases' - A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this streaming distribution.
--
-- 'trustedSigners', 'streamingDistributionSummary_trustedSigners' - A complex type that specifies the Amazon Web Services accounts, if any,
-- that you want to allow to create signed URLs for private content. If you
-- want to require signed URLs in requests for objects in the target origin
-- that match the @PathPattern@ for this cache behavior, specify @true@ for
-- @Enabled@, and specify the applicable values for @Quantity@ and
-- @Items@.If you don\'t want to require signed URLs in requests for
-- objects that match @PathPattern@, specify @false@ for @Enabled@ and @0@
-- for @Quantity@. Omit @Items@. To add, change, or remove one or more
-- trusted signers, change @Enabled@ to @true@ (if it\'s currently
-- @false@), change @Quantity@ as applicable, and specify all of the
-- trusted signers that you want to include in the updated distribution.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'comment', 'streamingDistributionSummary_comment' - The comment originally specified when this distribution was created.
--
-- 'priceClass', 'streamingDistributionSummary_priceClass' - A complex type that contains information about price class for this
-- streaming distribution.
--
-- 'enabled', 'streamingDistributionSummary_enabled' - Whether the distribution is enabled to accept end user requests for
-- content.
newStreamingDistributionSummary ::
  -- | 'id'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'status'
  Prelude.Text ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  -- | 'domainName'
  Prelude.Text ->
  -- | 's3Origin'
  S3Origin ->
  -- | 'aliases'
  Aliases ->
  -- | 'trustedSigners'
  TrustedSigners ->
  -- | 'comment'
  Prelude.Text ->
  -- | 'priceClass'
  PriceClass ->
  -- | 'enabled'
  Prelude.Bool ->
  StreamingDistributionSummary
newStreamingDistributionSummary
  pId_
  pARN_
  pStatus_
  pLastModifiedTime_
  pDomainName_
  pS3Origin_
  pAliases_
  pTrustedSigners_
  pComment_
  pPriceClass_
  pEnabled_ =
    StreamingDistributionSummary'
      { id = pId_,
        arn = pARN_,
        status = pStatus_,
        lastModifiedTime =
          Data._Time Lens.# pLastModifiedTime_,
        domainName = pDomainName_,
        s3Origin = pS3Origin_,
        aliases = pAliases_,
        trustedSigners = pTrustedSigners_,
        comment = pComment_,
        priceClass = pPriceClass_,
        enabled = pEnabled_
      }

-- | The identifier for the distribution, for example, @EDFDVBD632BHDS5@.
streamingDistributionSummary_id :: Lens.Lens' StreamingDistributionSummary Prelude.Text
streamingDistributionSummary_id = Lens.lens (\StreamingDistributionSummary' {id} -> id) (\s@StreamingDistributionSummary' {} a -> s {id = a} :: StreamingDistributionSummary)

-- | The ARN (Amazon Resource Name) for the streaming distribution. For
-- example:
-- @arn:aws:cloudfront::123456789012:streaming-distribution\/EDFDVBD632BHDS5@,
-- where @123456789012@ is your Amazon Web Services account ID.
streamingDistributionSummary_arn :: Lens.Lens' StreamingDistributionSummary Prelude.Text
streamingDistributionSummary_arn = Lens.lens (\StreamingDistributionSummary' {arn} -> arn) (\s@StreamingDistributionSummary' {} a -> s {arn = a} :: StreamingDistributionSummary)

-- | Indicates the current status of the distribution. When the status is
-- @Deployed@, the distribution\'s information is fully propagated
-- throughout the Amazon CloudFront system.
streamingDistributionSummary_status :: Lens.Lens' StreamingDistributionSummary Prelude.Text
streamingDistributionSummary_status = Lens.lens (\StreamingDistributionSummary' {status} -> status) (\s@StreamingDistributionSummary' {} a -> s {status = a} :: StreamingDistributionSummary)

-- | The date and time the distribution was last modified.
streamingDistributionSummary_lastModifiedTime :: Lens.Lens' StreamingDistributionSummary Prelude.UTCTime
streamingDistributionSummary_lastModifiedTime = Lens.lens (\StreamingDistributionSummary' {lastModifiedTime} -> lastModifiedTime) (\s@StreamingDistributionSummary' {} a -> s {lastModifiedTime = a} :: StreamingDistributionSummary) Prelude.. Data._Time

-- | The domain name corresponding to the distribution, for example,
-- @d111111abcdef8.cloudfront.net@.
streamingDistributionSummary_domainName :: Lens.Lens' StreamingDistributionSummary Prelude.Text
streamingDistributionSummary_domainName = Lens.lens (\StreamingDistributionSummary' {domainName} -> domainName) (\s@StreamingDistributionSummary' {} a -> s {domainName = a} :: StreamingDistributionSummary)

-- | A complex type that contains information about the Amazon S3 bucket from
-- which you want CloudFront to get your media files for distribution.
streamingDistributionSummary_s3Origin :: Lens.Lens' StreamingDistributionSummary S3Origin
streamingDistributionSummary_s3Origin = Lens.lens (\StreamingDistributionSummary' {s3Origin} -> s3Origin) (\s@StreamingDistributionSummary' {} a -> s {s3Origin = a} :: StreamingDistributionSummary)

-- | A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this streaming distribution.
streamingDistributionSummary_aliases :: Lens.Lens' StreamingDistributionSummary Aliases
streamingDistributionSummary_aliases = Lens.lens (\StreamingDistributionSummary' {aliases} -> aliases) (\s@StreamingDistributionSummary' {} a -> s {aliases = a} :: StreamingDistributionSummary)

-- | A complex type that specifies the Amazon Web Services accounts, if any,
-- that you want to allow to create signed URLs for private content. If you
-- want to require signed URLs in requests for objects in the target origin
-- that match the @PathPattern@ for this cache behavior, specify @true@ for
-- @Enabled@, and specify the applicable values for @Quantity@ and
-- @Items@.If you don\'t want to require signed URLs in requests for
-- objects that match @PathPattern@, specify @false@ for @Enabled@ and @0@
-- for @Quantity@. Omit @Items@. To add, change, or remove one or more
-- trusted signers, change @Enabled@ to @true@ (if it\'s currently
-- @false@), change @Quantity@ as applicable, and specify all of the
-- trusted signers that you want to include in the updated distribution.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront>
-- in the /Amazon CloudFront Developer Guide/.
streamingDistributionSummary_trustedSigners :: Lens.Lens' StreamingDistributionSummary TrustedSigners
streamingDistributionSummary_trustedSigners = Lens.lens (\StreamingDistributionSummary' {trustedSigners} -> trustedSigners) (\s@StreamingDistributionSummary' {} a -> s {trustedSigners = a} :: StreamingDistributionSummary)

-- | The comment originally specified when this distribution was created.
streamingDistributionSummary_comment :: Lens.Lens' StreamingDistributionSummary Prelude.Text
streamingDistributionSummary_comment = Lens.lens (\StreamingDistributionSummary' {comment} -> comment) (\s@StreamingDistributionSummary' {} a -> s {comment = a} :: StreamingDistributionSummary)

-- | A complex type that contains information about price class for this
-- streaming distribution.
streamingDistributionSummary_priceClass :: Lens.Lens' StreamingDistributionSummary PriceClass
streamingDistributionSummary_priceClass = Lens.lens (\StreamingDistributionSummary' {priceClass} -> priceClass) (\s@StreamingDistributionSummary' {} a -> s {priceClass = a} :: StreamingDistributionSummary)

-- | Whether the distribution is enabled to accept end user requests for
-- content.
streamingDistributionSummary_enabled :: Lens.Lens' StreamingDistributionSummary Prelude.Bool
streamingDistributionSummary_enabled = Lens.lens (\StreamingDistributionSummary' {enabled} -> enabled) (\s@StreamingDistributionSummary' {} a -> s {enabled = a} :: StreamingDistributionSummary)

instance Data.FromXML StreamingDistributionSummary where
  parseXML x =
    StreamingDistributionSummary'
      Prelude.<$> (x Data..@ "Id")
      Prelude.<*> (x Data..@ "ARN")
      Prelude.<*> (x Data..@ "Status")
      Prelude.<*> (x Data..@ "LastModifiedTime")
      Prelude.<*> (x Data..@ "DomainName")
      Prelude.<*> (x Data..@ "S3Origin")
      Prelude.<*> (x Data..@ "Aliases")
      Prelude.<*> (x Data..@ "TrustedSigners")
      Prelude.<*> (x Data..@ "Comment")
      Prelude.<*> (x Data..@ "PriceClass")
      Prelude.<*> (x Data..@ "Enabled")

instance
  Prelude.Hashable
    StreamingDistributionSummary
  where
  hashWithSalt _salt StreamingDistributionSummary' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` s3Origin
      `Prelude.hashWithSalt` aliases
      `Prelude.hashWithSalt` trustedSigners
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` priceClass
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData StreamingDistributionSummary where
  rnf StreamingDistributionSummary' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf s3Origin
      `Prelude.seq` Prelude.rnf aliases
      `Prelude.seq` Prelude.rnf trustedSigners
      `Prelude.seq` Prelude.rnf comment
      `Prelude.seq` Prelude.rnf priceClass
      `Prelude.seq` Prelude.rnf enabled
