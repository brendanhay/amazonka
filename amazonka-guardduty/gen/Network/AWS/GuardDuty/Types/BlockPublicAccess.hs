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
-- Module      : Network.AWS.GuardDuty.Types.BlockPublicAccess
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.BlockPublicAccess where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information on how the bucker owner\'s S3 Block Public Access
-- settings are being applied to the S3 bucket. See
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html S3 Block Public Access>
-- for more information.
--
-- /See:/ 'newBlockPublicAccess' smart constructor.
data BlockPublicAccess = BlockPublicAccess'
  { -- | Indicates if S3 Block Public Access is set to @IgnorePublicAcls@.
    ignorePublicAcls :: Prelude.Maybe Prelude.Bool,
    -- | Indicates if S3 Block Public Access is set to @BlockPublicAcls@.
    blockPublicAcls :: Prelude.Maybe Prelude.Bool,
    -- | Indicates if S3 Block Public Access is set to @RestrictPublicBuckets@.
    restrictPublicBuckets :: Prelude.Maybe Prelude.Bool,
    -- | Indicates if S3 Block Public Access is set to @BlockPublicPolicy@.
    blockPublicPolicy :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BlockPublicAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ignorePublicAcls', 'blockPublicAccess_ignorePublicAcls' - Indicates if S3 Block Public Access is set to @IgnorePublicAcls@.
--
-- 'blockPublicAcls', 'blockPublicAccess_blockPublicAcls' - Indicates if S3 Block Public Access is set to @BlockPublicAcls@.
--
-- 'restrictPublicBuckets', 'blockPublicAccess_restrictPublicBuckets' - Indicates if S3 Block Public Access is set to @RestrictPublicBuckets@.
--
-- 'blockPublicPolicy', 'blockPublicAccess_blockPublicPolicy' - Indicates if S3 Block Public Access is set to @BlockPublicPolicy@.
newBlockPublicAccess ::
  BlockPublicAccess
newBlockPublicAccess =
  BlockPublicAccess'
    { ignorePublicAcls =
        Prelude.Nothing,
      blockPublicAcls = Prelude.Nothing,
      restrictPublicBuckets = Prelude.Nothing,
      blockPublicPolicy = Prelude.Nothing
    }

-- | Indicates if S3 Block Public Access is set to @IgnorePublicAcls@.
blockPublicAccess_ignorePublicAcls :: Lens.Lens' BlockPublicAccess (Prelude.Maybe Prelude.Bool)
blockPublicAccess_ignorePublicAcls = Lens.lens (\BlockPublicAccess' {ignorePublicAcls} -> ignorePublicAcls) (\s@BlockPublicAccess' {} a -> s {ignorePublicAcls = a} :: BlockPublicAccess)

-- | Indicates if S3 Block Public Access is set to @BlockPublicAcls@.
blockPublicAccess_blockPublicAcls :: Lens.Lens' BlockPublicAccess (Prelude.Maybe Prelude.Bool)
blockPublicAccess_blockPublicAcls = Lens.lens (\BlockPublicAccess' {blockPublicAcls} -> blockPublicAcls) (\s@BlockPublicAccess' {} a -> s {blockPublicAcls = a} :: BlockPublicAccess)

-- | Indicates if S3 Block Public Access is set to @RestrictPublicBuckets@.
blockPublicAccess_restrictPublicBuckets :: Lens.Lens' BlockPublicAccess (Prelude.Maybe Prelude.Bool)
blockPublicAccess_restrictPublicBuckets = Lens.lens (\BlockPublicAccess' {restrictPublicBuckets} -> restrictPublicBuckets) (\s@BlockPublicAccess' {} a -> s {restrictPublicBuckets = a} :: BlockPublicAccess)

-- | Indicates if S3 Block Public Access is set to @BlockPublicPolicy@.
blockPublicAccess_blockPublicPolicy :: Lens.Lens' BlockPublicAccess (Prelude.Maybe Prelude.Bool)
blockPublicAccess_blockPublicPolicy = Lens.lens (\BlockPublicAccess' {blockPublicPolicy} -> blockPublicPolicy) (\s@BlockPublicAccess' {} a -> s {blockPublicPolicy = a} :: BlockPublicAccess)

instance Prelude.FromJSON BlockPublicAccess where
  parseJSON =
    Prelude.withObject
      "BlockPublicAccess"
      ( \x ->
          BlockPublicAccess'
            Prelude.<$> (x Prelude..:? "ignorePublicAcls")
            Prelude.<*> (x Prelude..:? "blockPublicAcls")
            Prelude.<*> (x Prelude..:? "restrictPublicBuckets")
            Prelude.<*> (x Prelude..:? "blockPublicPolicy")
      )

instance Prelude.Hashable BlockPublicAccess

instance Prelude.NFData BlockPublicAccess
