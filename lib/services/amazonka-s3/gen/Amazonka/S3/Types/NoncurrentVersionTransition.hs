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
-- Module      : Amazonka.S3.Types.NoncurrentVersionTransition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.NoncurrentVersionTransition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.TransitionStorageClass

-- | Container for the transition rule that describes when noncurrent objects
-- transition to the @STANDARD_IA@, @ONEZONE_IA@, @INTELLIGENT_TIERING@,
-- @GLACIER_IR@, @GLACIER@, or @DEEP_ARCHIVE@ storage class. If your bucket
-- is versioning-enabled (or versioning is suspended), you can set this
-- action to request that Amazon S3 transition noncurrent object versions
-- to the @STANDARD_IA@, @ONEZONE_IA@, @INTELLIGENT_TIERING@, @GLACIER_IR@,
-- @GLACIER@, or @DEEP_ARCHIVE@ storage class at a specific period in the
-- object\'s lifetime.
--
-- /See:/ 'newNoncurrentVersionTransition' smart constructor.
data NoncurrentVersionTransition = NoncurrentVersionTransition'
  { -- | Specifies how many noncurrent versions Amazon S3 will retain. If there
    -- are this many more recent noncurrent versions, Amazon S3 will take the
    -- associated action. For more information about noncurrent versions, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/intro-lifecycle-rules.html Lifecycle configuration elements>
    -- in the /Amazon S3 User Guide/.
    newerNoncurrentVersions' :: Prelude.Maybe Prelude.Int,
    -- | Specifies the number of days an object is noncurrent before Amazon S3
    -- can perform the associated action. For information about the noncurrent
    -- days calculations, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html#non-current-days-calculations How Amazon S3 Calculates How Long an Object Has Been Noncurrent>
    -- in the /Amazon S3 User Guide/.
    noncurrentDays :: Prelude.Int,
    -- | The class of storage used to store the object.
    storageClass :: TransitionStorageClass
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NoncurrentVersionTransition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'newerNoncurrentVersions'', 'noncurrentVersionTransition_newerNoncurrentVersions' - Specifies how many noncurrent versions Amazon S3 will retain. If there
-- are this many more recent noncurrent versions, Amazon S3 will take the
-- associated action. For more information about noncurrent versions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/intro-lifecycle-rules.html Lifecycle configuration elements>
-- in the /Amazon S3 User Guide/.
--
-- 'noncurrentDays', 'noncurrentVersionTransition_noncurrentDays' - Specifies the number of days an object is noncurrent before Amazon S3
-- can perform the associated action. For information about the noncurrent
-- days calculations, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html#non-current-days-calculations How Amazon S3 Calculates How Long an Object Has Been Noncurrent>
-- in the /Amazon S3 User Guide/.
--
-- 'storageClass', 'noncurrentVersionTransition_storageClass' - The class of storage used to store the object.
newNoncurrentVersionTransition ::
  -- | 'noncurrentDays'
  Prelude.Int ->
  -- | 'storageClass'
  TransitionStorageClass ->
  NoncurrentVersionTransition
newNoncurrentVersionTransition
  pNoncurrentDays_
  pStorageClass_ =
    NoncurrentVersionTransition'
      { newerNoncurrentVersions' =
          Prelude.Nothing,
        noncurrentDays = pNoncurrentDays_,
        storageClass = pStorageClass_
      }

-- | Specifies how many noncurrent versions Amazon S3 will retain. If there
-- are this many more recent noncurrent versions, Amazon S3 will take the
-- associated action. For more information about noncurrent versions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/intro-lifecycle-rules.html Lifecycle configuration elements>
-- in the /Amazon S3 User Guide/.
noncurrentVersionTransition_newerNoncurrentVersions :: Lens.Lens' NoncurrentVersionTransition (Prelude.Maybe Prelude.Int)
noncurrentVersionTransition_newerNoncurrentVersions = Lens.lens (\NoncurrentVersionTransition' {newerNoncurrentVersions'} -> newerNoncurrentVersions') (\s@NoncurrentVersionTransition' {} a -> s {newerNoncurrentVersions' = a} :: NoncurrentVersionTransition)

-- | Specifies the number of days an object is noncurrent before Amazon S3
-- can perform the associated action. For information about the noncurrent
-- days calculations, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html#non-current-days-calculations How Amazon S3 Calculates How Long an Object Has Been Noncurrent>
-- in the /Amazon S3 User Guide/.
noncurrentVersionTransition_noncurrentDays :: Lens.Lens' NoncurrentVersionTransition Prelude.Int
noncurrentVersionTransition_noncurrentDays = Lens.lens (\NoncurrentVersionTransition' {noncurrentDays} -> noncurrentDays) (\s@NoncurrentVersionTransition' {} a -> s {noncurrentDays = a} :: NoncurrentVersionTransition)

-- | The class of storage used to store the object.
noncurrentVersionTransition_storageClass :: Lens.Lens' NoncurrentVersionTransition TransitionStorageClass
noncurrentVersionTransition_storageClass = Lens.lens (\NoncurrentVersionTransition' {storageClass} -> storageClass) (\s@NoncurrentVersionTransition' {} a -> s {storageClass = a} :: NoncurrentVersionTransition)

instance Data.FromXML NoncurrentVersionTransition where
  parseXML x =
    NoncurrentVersionTransition'
      Prelude.<$> (x Data..@? "NewerNoncurrentVersions")
      Prelude.<*> (x Data..@ "NoncurrentDays")
      Prelude.<*> (x Data..@ "StorageClass")

instance Prelude.Hashable NoncurrentVersionTransition where
  hashWithSalt _salt NoncurrentVersionTransition' {..} =
    _salt
      `Prelude.hashWithSalt` newerNoncurrentVersions'
      `Prelude.hashWithSalt` noncurrentDays
      `Prelude.hashWithSalt` storageClass

instance Prelude.NFData NoncurrentVersionTransition where
  rnf NoncurrentVersionTransition' {..} =
    Prelude.rnf newerNoncurrentVersions' `Prelude.seq`
      Prelude.rnf noncurrentDays `Prelude.seq`
        Prelude.rnf storageClass

instance Data.ToXML NoncurrentVersionTransition where
  toXML NoncurrentVersionTransition' {..} =
    Prelude.mconcat
      [ "NewerNoncurrentVersions"
          Data.@= newerNoncurrentVersions',
        "NoncurrentDays" Data.@= noncurrentDays,
        "StorageClass" Data.@= storageClass
      ]
