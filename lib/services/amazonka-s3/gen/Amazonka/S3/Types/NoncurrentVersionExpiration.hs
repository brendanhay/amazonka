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
-- Module      : Amazonka.S3.Types.NoncurrentVersionExpiration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.NoncurrentVersionExpiration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

-- | Specifies when noncurrent object versions expire. Upon expiration,
-- Amazon S3 permanently deletes the noncurrent object versions. You set
-- this lifecycle configuration action on a bucket that has versioning
-- enabled (or suspended) to request that Amazon S3 delete noncurrent
-- object versions at a specific period in the object\'s lifetime.
--
-- /See:/ 'newNoncurrentVersionExpiration' smart constructor.
data NoncurrentVersionExpiration = NoncurrentVersionExpiration'
  { -- | Specifies how many noncurrent versions Amazon S3 will retain. If there
    -- are this many more recent noncurrent versions, Amazon S3 will take the
    -- associated action. For more information about noncurrent versions, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/intro-lifecycle-rules.html Lifecycle configuration elements>
    -- in the /Amazon S3 User Guide/.
    newerNoncurrentVersions' :: Prelude.Maybe Prelude.Int,
    -- | Specifies the number of days an object is noncurrent before Amazon S3
    -- can perform the associated action. The value must be a non-zero positive
    -- integer. For information about the noncurrent days calculations, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html#non-current-days-calculations How Amazon S3 Calculates When an Object Became Noncurrent>
    -- in the /Amazon S3 User Guide/.
    noncurrentDays :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NoncurrentVersionExpiration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'newerNoncurrentVersions'', 'noncurrentVersionExpiration_newerNoncurrentVersions' - Specifies how many noncurrent versions Amazon S3 will retain. If there
-- are this many more recent noncurrent versions, Amazon S3 will take the
-- associated action. For more information about noncurrent versions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/intro-lifecycle-rules.html Lifecycle configuration elements>
-- in the /Amazon S3 User Guide/.
--
-- 'noncurrentDays', 'noncurrentVersionExpiration_noncurrentDays' - Specifies the number of days an object is noncurrent before Amazon S3
-- can perform the associated action. The value must be a non-zero positive
-- integer. For information about the noncurrent days calculations, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html#non-current-days-calculations How Amazon S3 Calculates When an Object Became Noncurrent>
-- in the /Amazon S3 User Guide/.
newNoncurrentVersionExpiration ::
  -- | 'noncurrentDays'
  Prelude.Int ->
  NoncurrentVersionExpiration
newNoncurrentVersionExpiration pNoncurrentDays_ =
  NoncurrentVersionExpiration'
    { newerNoncurrentVersions' =
        Prelude.Nothing,
      noncurrentDays = pNoncurrentDays_
    }

-- | Specifies how many noncurrent versions Amazon S3 will retain. If there
-- are this many more recent noncurrent versions, Amazon S3 will take the
-- associated action. For more information about noncurrent versions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/intro-lifecycle-rules.html Lifecycle configuration elements>
-- in the /Amazon S3 User Guide/.
noncurrentVersionExpiration_newerNoncurrentVersions :: Lens.Lens' NoncurrentVersionExpiration (Prelude.Maybe Prelude.Int)
noncurrentVersionExpiration_newerNoncurrentVersions = Lens.lens (\NoncurrentVersionExpiration' {newerNoncurrentVersions'} -> newerNoncurrentVersions') (\s@NoncurrentVersionExpiration' {} a -> s {newerNoncurrentVersions' = a} :: NoncurrentVersionExpiration)

-- | Specifies the number of days an object is noncurrent before Amazon S3
-- can perform the associated action. The value must be a non-zero positive
-- integer. For information about the noncurrent days calculations, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html#non-current-days-calculations How Amazon S3 Calculates When an Object Became Noncurrent>
-- in the /Amazon S3 User Guide/.
noncurrentVersionExpiration_noncurrentDays :: Lens.Lens' NoncurrentVersionExpiration Prelude.Int
noncurrentVersionExpiration_noncurrentDays = Lens.lens (\NoncurrentVersionExpiration' {noncurrentDays} -> noncurrentDays) (\s@NoncurrentVersionExpiration' {} a -> s {noncurrentDays = a} :: NoncurrentVersionExpiration)

instance Core.FromXML NoncurrentVersionExpiration where
  parseXML x =
    NoncurrentVersionExpiration'
      Prelude.<$> (x Core..@? "NewerNoncurrentVersions")
      Prelude.<*> (x Core..@ "NoncurrentDays")

instance Prelude.Hashable NoncurrentVersionExpiration where
  hashWithSalt _salt NoncurrentVersionExpiration' {..} =
    _salt
      `Prelude.hashWithSalt` newerNoncurrentVersions'
      `Prelude.hashWithSalt` noncurrentDays

instance Prelude.NFData NoncurrentVersionExpiration where
  rnf NoncurrentVersionExpiration' {..} =
    Prelude.rnf newerNoncurrentVersions'
      `Prelude.seq` Prelude.rnf noncurrentDays

instance Core.ToXML NoncurrentVersionExpiration where
  toXML NoncurrentVersionExpiration' {..} =
    Prelude.mconcat
      [ "NewerNoncurrentVersions"
          Core.@= newerNoncurrentVersions',
        "NoncurrentDays" Core.@= noncurrentDays
      ]
