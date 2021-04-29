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
-- Module      : Network.AWS.CloudFront.Types.S3OriginConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.S3OriginConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A complex type that contains information about the Amazon S3 origin. If
-- the origin is a custom origin or an S3 bucket that is configured as a
-- website endpoint, use the @CustomOriginConfig@ element instead.
--
-- /See:/ 'newS3OriginConfig' smart constructor.
data S3OriginConfig = S3OriginConfig'
  { -- | The CloudFront origin access identity to associate with the origin. Use
    -- an origin access identity to configure the origin so that viewers can
    -- /only/ access objects in an Amazon S3 bucket through CloudFront. The
    -- format of the value is:
    --
    -- origin-access-identity\/cloudfront\//ID-of-origin-access-identity/
    --
    -- where @ ID-of-origin-access-identity @ is the value that CloudFront
    -- returned in the @ID@ element when you created the origin access
    -- identity.
    --
    -- If you want viewers to be able to access objects using either the
    -- CloudFront URL or the Amazon S3 URL, specify an empty
    -- @OriginAccessIdentity@ element.
    --
    -- To delete the origin access identity from an existing distribution,
    -- update the distribution configuration and include an empty
    -- @OriginAccessIdentity@ element.
    --
    -- To replace the origin access identity, update the distribution
    -- configuration and specify the new origin access identity.
    --
    -- For more information about the origin access identity, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront>
    -- in the /Amazon CloudFront Developer Guide/.
    originAccessIdentity :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'S3OriginConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'originAccessIdentity', 's3OriginConfig_originAccessIdentity' - The CloudFront origin access identity to associate with the origin. Use
-- an origin access identity to configure the origin so that viewers can
-- /only/ access objects in an Amazon S3 bucket through CloudFront. The
-- format of the value is:
--
-- origin-access-identity\/cloudfront\//ID-of-origin-access-identity/
--
-- where @ ID-of-origin-access-identity @ is the value that CloudFront
-- returned in the @ID@ element when you created the origin access
-- identity.
--
-- If you want viewers to be able to access objects using either the
-- CloudFront URL or the Amazon S3 URL, specify an empty
-- @OriginAccessIdentity@ element.
--
-- To delete the origin access identity from an existing distribution,
-- update the distribution configuration and include an empty
-- @OriginAccessIdentity@ element.
--
-- To replace the origin access identity, update the distribution
-- configuration and specify the new origin access identity.
--
-- For more information about the origin access identity, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront>
-- in the /Amazon CloudFront Developer Guide/.
newS3OriginConfig ::
  -- | 'originAccessIdentity'
  Prelude.Text ->
  S3OriginConfig
newS3OriginConfig pOriginAccessIdentity_ =
  S3OriginConfig'
    { originAccessIdentity =
        pOriginAccessIdentity_
    }

-- | The CloudFront origin access identity to associate with the origin. Use
-- an origin access identity to configure the origin so that viewers can
-- /only/ access objects in an Amazon S3 bucket through CloudFront. The
-- format of the value is:
--
-- origin-access-identity\/cloudfront\//ID-of-origin-access-identity/
--
-- where @ ID-of-origin-access-identity @ is the value that CloudFront
-- returned in the @ID@ element when you created the origin access
-- identity.
--
-- If you want viewers to be able to access objects using either the
-- CloudFront URL or the Amazon S3 URL, specify an empty
-- @OriginAccessIdentity@ element.
--
-- To delete the origin access identity from an existing distribution,
-- update the distribution configuration and include an empty
-- @OriginAccessIdentity@ element.
--
-- To replace the origin access identity, update the distribution
-- configuration and specify the new origin access identity.
--
-- For more information about the origin access identity, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront>
-- in the /Amazon CloudFront Developer Guide/.
s3OriginConfig_originAccessIdentity :: Lens.Lens' S3OriginConfig Prelude.Text
s3OriginConfig_originAccessIdentity = Lens.lens (\S3OriginConfig' {originAccessIdentity} -> originAccessIdentity) (\s@S3OriginConfig' {} a -> s {originAccessIdentity = a} :: S3OriginConfig)

instance Prelude.FromXML S3OriginConfig where
  parseXML x =
    S3OriginConfig'
      Prelude.<$> (x Prelude..@ "OriginAccessIdentity")

instance Prelude.Hashable S3OriginConfig

instance Prelude.NFData S3OriginConfig

instance Prelude.ToXML S3OriginConfig where
  toXML S3OriginConfig' {..} =
    Prelude.mconcat
      [ "OriginAccessIdentity"
          Prelude.@= originAccessIdentity
      ]
