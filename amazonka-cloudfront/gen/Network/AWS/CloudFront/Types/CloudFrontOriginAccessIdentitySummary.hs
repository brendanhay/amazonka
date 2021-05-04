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
-- Module      : Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentitySummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentitySummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Summary of the information about a CloudFront origin access identity.
--
-- /See:/ 'newCloudFrontOriginAccessIdentitySummary' smart constructor.
data CloudFrontOriginAccessIdentitySummary = CloudFrontOriginAccessIdentitySummary'
  { -- | The ID for the origin access identity. For example: @E74FTE3AJFJ256A@.
    id :: Prelude.Text,
    -- | The Amazon S3 canonical user ID for the origin access identity, which
    -- you use when giving the origin access identity read permission to an
    -- object in Amazon S3.
    s3CanonicalUserId :: Prelude.Text,
    -- | The comment for this origin access identity, as originally specified
    -- when created.
    comment :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CloudFrontOriginAccessIdentitySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'cloudFrontOriginAccessIdentitySummary_id' - The ID for the origin access identity. For example: @E74FTE3AJFJ256A@.
--
-- 's3CanonicalUserId', 'cloudFrontOriginAccessIdentitySummary_s3CanonicalUserId' - The Amazon S3 canonical user ID for the origin access identity, which
-- you use when giving the origin access identity read permission to an
-- object in Amazon S3.
--
-- 'comment', 'cloudFrontOriginAccessIdentitySummary_comment' - The comment for this origin access identity, as originally specified
-- when created.
newCloudFrontOriginAccessIdentitySummary ::
  -- | 'id'
  Prelude.Text ->
  -- | 's3CanonicalUserId'
  Prelude.Text ->
  -- | 'comment'
  Prelude.Text ->
  CloudFrontOriginAccessIdentitySummary
newCloudFrontOriginAccessIdentitySummary
  pId_
  pS3CanonicalUserId_
  pComment_ =
    CloudFrontOriginAccessIdentitySummary'
      { id = pId_,
        s3CanonicalUserId =
          pS3CanonicalUserId_,
        comment = pComment_
      }

-- | The ID for the origin access identity. For example: @E74FTE3AJFJ256A@.
cloudFrontOriginAccessIdentitySummary_id :: Lens.Lens' CloudFrontOriginAccessIdentitySummary Prelude.Text
cloudFrontOriginAccessIdentitySummary_id = Lens.lens (\CloudFrontOriginAccessIdentitySummary' {id} -> id) (\s@CloudFrontOriginAccessIdentitySummary' {} a -> s {id = a} :: CloudFrontOriginAccessIdentitySummary)

-- | The Amazon S3 canonical user ID for the origin access identity, which
-- you use when giving the origin access identity read permission to an
-- object in Amazon S3.
cloudFrontOriginAccessIdentitySummary_s3CanonicalUserId :: Lens.Lens' CloudFrontOriginAccessIdentitySummary Prelude.Text
cloudFrontOriginAccessIdentitySummary_s3CanonicalUserId = Lens.lens (\CloudFrontOriginAccessIdentitySummary' {s3CanonicalUserId} -> s3CanonicalUserId) (\s@CloudFrontOriginAccessIdentitySummary' {} a -> s {s3CanonicalUserId = a} :: CloudFrontOriginAccessIdentitySummary)

-- | The comment for this origin access identity, as originally specified
-- when created.
cloudFrontOriginAccessIdentitySummary_comment :: Lens.Lens' CloudFrontOriginAccessIdentitySummary Prelude.Text
cloudFrontOriginAccessIdentitySummary_comment = Lens.lens (\CloudFrontOriginAccessIdentitySummary' {comment} -> comment) (\s@CloudFrontOriginAccessIdentitySummary' {} a -> s {comment = a} :: CloudFrontOriginAccessIdentitySummary)

instance
  Prelude.FromXML
    CloudFrontOriginAccessIdentitySummary
  where
  parseXML x =
    CloudFrontOriginAccessIdentitySummary'
      Prelude.<$> (x Prelude..@ "Id")
      Prelude.<*> (x Prelude..@ "S3CanonicalUserId")
      Prelude.<*> (x Prelude..@ "Comment")

instance
  Prelude.Hashable
    CloudFrontOriginAccessIdentitySummary

instance
  Prelude.NFData
    CloudFrontOriginAccessIdentitySummary
