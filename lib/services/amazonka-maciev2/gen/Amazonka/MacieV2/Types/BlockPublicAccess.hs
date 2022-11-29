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
-- Module      : Amazonka.MacieV2.Types.BlockPublicAccess
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.BlockPublicAccess where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the block public access settings for an S3
-- bucket. These settings can apply to a bucket at the account level or
-- bucket level. For detailed information about each setting, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/access-control-block-public-access.html Blocking public access to your Amazon S3 storage>
-- in the /Amazon Simple Storage Service User Guide/.
--
-- /See:/ 'newBlockPublicAccess' smart constructor.
data BlockPublicAccess = BlockPublicAccess'
  { -- | Specifies whether Amazon S3 restricts public bucket policies for the
    -- bucket.
    restrictPublicBuckets :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether Amazon S3 ignores public ACLs for the bucket and
    -- objects in the bucket.
    ignorePublicAcls :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether Amazon S3 blocks public bucket policies for the
    -- bucket.
    blockPublicPolicy :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether Amazon S3 blocks public access control lists (ACLs)
    -- for the bucket and objects in the bucket.
    blockPublicAcls :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BlockPublicAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restrictPublicBuckets', 'blockPublicAccess_restrictPublicBuckets' - Specifies whether Amazon S3 restricts public bucket policies for the
-- bucket.
--
-- 'ignorePublicAcls', 'blockPublicAccess_ignorePublicAcls' - Specifies whether Amazon S3 ignores public ACLs for the bucket and
-- objects in the bucket.
--
-- 'blockPublicPolicy', 'blockPublicAccess_blockPublicPolicy' - Specifies whether Amazon S3 blocks public bucket policies for the
-- bucket.
--
-- 'blockPublicAcls', 'blockPublicAccess_blockPublicAcls' - Specifies whether Amazon S3 blocks public access control lists (ACLs)
-- for the bucket and objects in the bucket.
newBlockPublicAccess ::
  BlockPublicAccess
newBlockPublicAccess =
  BlockPublicAccess'
    { restrictPublicBuckets =
        Prelude.Nothing,
      ignorePublicAcls = Prelude.Nothing,
      blockPublicPolicy = Prelude.Nothing,
      blockPublicAcls = Prelude.Nothing
    }

-- | Specifies whether Amazon S3 restricts public bucket policies for the
-- bucket.
blockPublicAccess_restrictPublicBuckets :: Lens.Lens' BlockPublicAccess (Prelude.Maybe Prelude.Bool)
blockPublicAccess_restrictPublicBuckets = Lens.lens (\BlockPublicAccess' {restrictPublicBuckets} -> restrictPublicBuckets) (\s@BlockPublicAccess' {} a -> s {restrictPublicBuckets = a} :: BlockPublicAccess)

-- | Specifies whether Amazon S3 ignores public ACLs for the bucket and
-- objects in the bucket.
blockPublicAccess_ignorePublicAcls :: Lens.Lens' BlockPublicAccess (Prelude.Maybe Prelude.Bool)
blockPublicAccess_ignorePublicAcls = Lens.lens (\BlockPublicAccess' {ignorePublicAcls} -> ignorePublicAcls) (\s@BlockPublicAccess' {} a -> s {ignorePublicAcls = a} :: BlockPublicAccess)

-- | Specifies whether Amazon S3 blocks public bucket policies for the
-- bucket.
blockPublicAccess_blockPublicPolicy :: Lens.Lens' BlockPublicAccess (Prelude.Maybe Prelude.Bool)
blockPublicAccess_blockPublicPolicy = Lens.lens (\BlockPublicAccess' {blockPublicPolicy} -> blockPublicPolicy) (\s@BlockPublicAccess' {} a -> s {blockPublicPolicy = a} :: BlockPublicAccess)

-- | Specifies whether Amazon S3 blocks public access control lists (ACLs)
-- for the bucket and objects in the bucket.
blockPublicAccess_blockPublicAcls :: Lens.Lens' BlockPublicAccess (Prelude.Maybe Prelude.Bool)
blockPublicAccess_blockPublicAcls = Lens.lens (\BlockPublicAccess' {blockPublicAcls} -> blockPublicAcls) (\s@BlockPublicAccess' {} a -> s {blockPublicAcls = a} :: BlockPublicAccess)

instance Core.FromJSON BlockPublicAccess where
  parseJSON =
    Core.withObject
      "BlockPublicAccess"
      ( \x ->
          BlockPublicAccess'
            Prelude.<$> (x Core..:? "restrictPublicBuckets")
            Prelude.<*> (x Core..:? "ignorePublicAcls")
            Prelude.<*> (x Core..:? "blockPublicPolicy")
            Prelude.<*> (x Core..:? "blockPublicAcls")
      )

instance Prelude.Hashable BlockPublicAccess where
  hashWithSalt _salt BlockPublicAccess' {..} =
    _salt `Prelude.hashWithSalt` restrictPublicBuckets
      `Prelude.hashWithSalt` ignorePublicAcls
      `Prelude.hashWithSalt` blockPublicPolicy
      `Prelude.hashWithSalt` blockPublicAcls

instance Prelude.NFData BlockPublicAccess where
  rnf BlockPublicAccess' {..} =
    Prelude.rnf restrictPublicBuckets
      `Prelude.seq` Prelude.rnf ignorePublicAcls
      `Prelude.seq` Prelude.rnf blockPublicPolicy
      `Prelude.seq` Prelude.rnf blockPublicAcls
