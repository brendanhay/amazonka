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
-- Module      : Network.AWS.CloudFront.Types.S3Origin
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.S3Origin where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A complex type that contains information about the Amazon S3 bucket from
-- which you want CloudFront to get your media files for distribution.
--
-- /See:/ 'newS3Origin' smart constructor.
data S3Origin = S3Origin'
  { -- | The DNS name of the Amazon S3 origin.
    domainName :: Prelude.Text,
    -- | The CloudFront origin access identity to associate with the
    -- distribution. Use an origin access identity to configure the
    -- distribution so that end users can only access objects in an Amazon S3
    -- bucket through CloudFront.
    --
    -- If you want end users to be able to access objects using either the
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
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-restricting-access-to-s3.html Using an Origin Access Identity to Restrict Access to Your Amazon S3 Content>
    -- in the /Amazon CloudFront Developer Guide/.
    originAccessIdentity :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'S3Origin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 's3Origin_domainName' - The DNS name of the Amazon S3 origin.
--
-- 'originAccessIdentity', 's3Origin_originAccessIdentity' - The CloudFront origin access identity to associate with the
-- distribution. Use an origin access identity to configure the
-- distribution so that end users can only access objects in an Amazon S3
-- bucket through CloudFront.
--
-- If you want end users to be able to access objects using either the
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
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-restricting-access-to-s3.html Using an Origin Access Identity to Restrict Access to Your Amazon S3 Content>
-- in the /Amazon CloudFront Developer Guide/.
newS3Origin ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'originAccessIdentity'
  Prelude.Text ->
  S3Origin
newS3Origin pDomainName_ pOriginAccessIdentity_ =
  S3Origin'
    { domainName = pDomainName_,
      originAccessIdentity = pOriginAccessIdentity_
    }

-- | The DNS name of the Amazon S3 origin.
s3Origin_domainName :: Lens.Lens' S3Origin Prelude.Text
s3Origin_domainName = Lens.lens (\S3Origin' {domainName} -> domainName) (\s@S3Origin' {} a -> s {domainName = a} :: S3Origin)

-- | The CloudFront origin access identity to associate with the
-- distribution. Use an origin access identity to configure the
-- distribution so that end users can only access objects in an Amazon S3
-- bucket through CloudFront.
--
-- If you want end users to be able to access objects using either the
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
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-restricting-access-to-s3.html Using an Origin Access Identity to Restrict Access to Your Amazon S3 Content>
-- in the /Amazon CloudFront Developer Guide/.
s3Origin_originAccessIdentity :: Lens.Lens' S3Origin Prelude.Text
s3Origin_originAccessIdentity = Lens.lens (\S3Origin' {originAccessIdentity} -> originAccessIdentity) (\s@S3Origin' {} a -> s {originAccessIdentity = a} :: S3Origin)

instance Prelude.FromXML S3Origin where
  parseXML x =
    S3Origin'
      Prelude.<$> (x Prelude..@ "DomainName")
      Prelude.<*> (x Prelude..@ "OriginAccessIdentity")

instance Prelude.Hashable S3Origin

instance Prelude.NFData S3Origin

instance Prelude.ToXML S3Origin where
  toXML S3Origin' {..} =
    Prelude.mconcat
      [ "DomainName" Prelude.@= domainName,
        "OriginAccessIdentity"
          Prelude.@= originAccessIdentity
      ]
