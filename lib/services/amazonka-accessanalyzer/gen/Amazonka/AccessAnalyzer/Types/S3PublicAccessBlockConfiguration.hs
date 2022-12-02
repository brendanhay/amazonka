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
-- Module      : Amazonka.AccessAnalyzer.Types.S3PublicAccessBlockConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.S3PublicAccessBlockConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The @PublicAccessBlock@ configuration to apply to this Amazon S3 bucket.
-- If the proposed configuration is for an existing Amazon S3 bucket and
-- the configuration is not specified, the access preview uses the existing
-- setting. If the proposed configuration is for a new bucket and the
-- configuration is not specified, the access preview uses @false@. If the
-- proposed configuration is for a new access point or multi-region access
-- point and the access point BPA configuration is not specified, the
-- access preview uses @true@. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3-bucket-publicaccessblockconfiguration.html PublicAccessBlockConfiguration>.
--
-- /See:/ 'newS3PublicAccessBlockConfiguration' smart constructor.
data S3PublicAccessBlockConfiguration = S3PublicAccessBlockConfiguration'
  { -- | Specifies whether Amazon S3 should ignore public ACLs for this bucket
    -- and objects in this bucket.
    ignorePublicAcls :: Prelude.Bool,
    -- | Specifies whether Amazon S3 should restrict public bucket policies for
    -- this bucket.
    restrictPublicBuckets :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3PublicAccessBlockConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ignorePublicAcls', 's3PublicAccessBlockConfiguration_ignorePublicAcls' - Specifies whether Amazon S3 should ignore public ACLs for this bucket
-- and objects in this bucket.
--
-- 'restrictPublicBuckets', 's3PublicAccessBlockConfiguration_restrictPublicBuckets' - Specifies whether Amazon S3 should restrict public bucket policies for
-- this bucket.
newS3PublicAccessBlockConfiguration ::
  -- | 'ignorePublicAcls'
  Prelude.Bool ->
  -- | 'restrictPublicBuckets'
  Prelude.Bool ->
  S3PublicAccessBlockConfiguration
newS3PublicAccessBlockConfiguration
  pIgnorePublicAcls_
  pRestrictPublicBuckets_ =
    S3PublicAccessBlockConfiguration'
      { ignorePublicAcls =
          pIgnorePublicAcls_,
        restrictPublicBuckets =
          pRestrictPublicBuckets_
      }

-- | Specifies whether Amazon S3 should ignore public ACLs for this bucket
-- and objects in this bucket.
s3PublicAccessBlockConfiguration_ignorePublicAcls :: Lens.Lens' S3PublicAccessBlockConfiguration Prelude.Bool
s3PublicAccessBlockConfiguration_ignorePublicAcls = Lens.lens (\S3PublicAccessBlockConfiguration' {ignorePublicAcls} -> ignorePublicAcls) (\s@S3PublicAccessBlockConfiguration' {} a -> s {ignorePublicAcls = a} :: S3PublicAccessBlockConfiguration)

-- | Specifies whether Amazon S3 should restrict public bucket policies for
-- this bucket.
s3PublicAccessBlockConfiguration_restrictPublicBuckets :: Lens.Lens' S3PublicAccessBlockConfiguration Prelude.Bool
s3PublicAccessBlockConfiguration_restrictPublicBuckets = Lens.lens (\S3PublicAccessBlockConfiguration' {restrictPublicBuckets} -> restrictPublicBuckets) (\s@S3PublicAccessBlockConfiguration' {} a -> s {restrictPublicBuckets = a} :: S3PublicAccessBlockConfiguration)

instance
  Data.FromJSON
    S3PublicAccessBlockConfiguration
  where
  parseJSON =
    Data.withObject
      "S3PublicAccessBlockConfiguration"
      ( \x ->
          S3PublicAccessBlockConfiguration'
            Prelude.<$> (x Data..: "ignorePublicAcls")
            Prelude.<*> (x Data..: "restrictPublicBuckets")
      )

instance
  Prelude.Hashable
    S3PublicAccessBlockConfiguration
  where
  hashWithSalt
    _salt
    S3PublicAccessBlockConfiguration' {..} =
      _salt `Prelude.hashWithSalt` ignorePublicAcls
        `Prelude.hashWithSalt` restrictPublicBuckets

instance
  Prelude.NFData
    S3PublicAccessBlockConfiguration
  where
  rnf S3PublicAccessBlockConfiguration' {..} =
    Prelude.rnf ignorePublicAcls
      `Prelude.seq` Prelude.rnf restrictPublicBuckets

instance Data.ToJSON S3PublicAccessBlockConfiguration where
  toJSON S3PublicAccessBlockConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ignorePublicAcls" Data..= ignorePublicAcls),
            Prelude.Just
              ( "restrictPublicBuckets"
                  Data..= restrictPublicBuckets
              )
          ]
      )
