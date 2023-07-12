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
-- Module      : Amazonka.CloudFront.Types.CloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.CloudFrontOriginAccessIdentity where

import Amazonka.CloudFront.Types.CloudFrontOriginAccessIdentityConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromXML CloudFrontOriginAccessIdentity where
  parseXML x =
    CloudFrontOriginAccessIdentity'
      Prelude.<$> (x Data..@? "CloudFrontOriginAccessIdentityConfig")
      Prelude.<*> (x Data..@ "Id")
      Prelude.<*> (x Data..@ "S3CanonicalUserId")

instance
  Prelude.Hashable
    CloudFrontOriginAccessIdentity
  where
  hashWithSalt
    _salt
    CloudFrontOriginAccessIdentity' {..} =
      _salt
        `Prelude.hashWithSalt` cloudFrontOriginAccessIdentityConfig
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` s3CanonicalUserId

instance
  Prelude.NFData
    CloudFrontOriginAccessIdentity
  where
  rnf CloudFrontOriginAccessIdentity' {..} =
    Prelude.rnf cloudFrontOriginAccessIdentityConfig
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf s3CanonicalUserId
