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
-- Module      : Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentity where

import Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentityConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | CloudFront origin access identity.
--
-- /See:/ 'newCloudFrontOriginAccessIdentity' smart constructor.
data CloudFrontOriginAccessIdentity = CloudFrontOriginAccessIdentity'
  { -- | The current configuration information for the identity.
    cloudFrontOriginAccessIdentityConfig :: Prelude.Maybe CloudFrontOriginAccessIdentityConfig,
    -- | The ID for the origin access identity, for example, @E74FTE3AJFJ256A@.
    id :: Prelude.Text,
    -- | The Amazon S3 canonical user ID for the origin access identity, used
    -- when giving the origin access identity read permission to an object in
    -- Amazon S3.
    s3CanonicalUserId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CloudFrontOriginAccessIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudFrontOriginAccessIdentityConfig', 'cloudFrontOriginAccessIdentity_cloudFrontOriginAccessIdentityConfig' - The current configuration information for the identity.
--
-- 'id', 'cloudFrontOriginAccessIdentity_id' - The ID for the origin access identity, for example, @E74FTE3AJFJ256A@.
--
-- 's3CanonicalUserId', 'cloudFrontOriginAccessIdentity_s3CanonicalUserId' - The Amazon S3 canonical user ID for the origin access identity, used
-- when giving the origin access identity read permission to an object in
-- Amazon S3.
newCloudFrontOriginAccessIdentity ::
  -- | 'id'
  Prelude.Text ->
  -- | 's3CanonicalUserId'
  Prelude.Text ->
  CloudFrontOriginAccessIdentity
newCloudFrontOriginAccessIdentity
  pId_
  pS3CanonicalUserId_ =
    CloudFrontOriginAccessIdentity'
      { cloudFrontOriginAccessIdentityConfig =
          Prelude.Nothing,
        id = pId_,
        s3CanonicalUserId = pS3CanonicalUserId_
      }

-- | The current configuration information for the identity.
cloudFrontOriginAccessIdentity_cloudFrontOriginAccessIdentityConfig :: Lens.Lens' CloudFrontOriginAccessIdentity (Prelude.Maybe CloudFrontOriginAccessIdentityConfig)
cloudFrontOriginAccessIdentity_cloudFrontOriginAccessIdentityConfig = Lens.lens (\CloudFrontOriginAccessIdentity' {cloudFrontOriginAccessIdentityConfig} -> cloudFrontOriginAccessIdentityConfig) (\s@CloudFrontOriginAccessIdentity' {} a -> s {cloudFrontOriginAccessIdentityConfig = a} :: CloudFrontOriginAccessIdentity)

-- | The ID for the origin access identity, for example, @E74FTE3AJFJ256A@.
cloudFrontOriginAccessIdentity_id :: Lens.Lens' CloudFrontOriginAccessIdentity Prelude.Text
cloudFrontOriginAccessIdentity_id = Lens.lens (\CloudFrontOriginAccessIdentity' {id} -> id) (\s@CloudFrontOriginAccessIdentity' {} a -> s {id = a} :: CloudFrontOriginAccessIdentity)

-- | The Amazon S3 canonical user ID for the origin access identity, used
-- when giving the origin access identity read permission to an object in
-- Amazon S3.
cloudFrontOriginAccessIdentity_s3CanonicalUserId :: Lens.Lens' CloudFrontOriginAccessIdentity Prelude.Text
cloudFrontOriginAccessIdentity_s3CanonicalUserId = Lens.lens (\CloudFrontOriginAccessIdentity' {s3CanonicalUserId} -> s3CanonicalUserId) (\s@CloudFrontOriginAccessIdentity' {} a -> s {s3CanonicalUserId = a} :: CloudFrontOriginAccessIdentity)

instance
  Prelude.FromXML
    CloudFrontOriginAccessIdentity
  where
  parseXML x =
    CloudFrontOriginAccessIdentity'
      Prelude.<$> ( x
                      Prelude..@? "CloudFrontOriginAccessIdentityConfig"
                  )
      Prelude.<*> (x Prelude..@ "Id")
      Prelude.<*> (x Prelude..@ "S3CanonicalUserId")

instance
  Prelude.Hashable
    CloudFrontOriginAccessIdentity

instance
  Prelude.NFData
    CloudFrontOriginAccessIdentity
