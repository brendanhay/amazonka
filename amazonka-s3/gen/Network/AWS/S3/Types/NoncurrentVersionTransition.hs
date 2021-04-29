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
-- Module      : Network.AWS.S3.Types.NoncurrentVersionTransition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.NoncurrentVersionTransition where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.TransitionStorageClass

-- | Container for the transition rule that describes when noncurrent objects
-- transition to the @STANDARD_IA@, @ONEZONE_IA@, @INTELLIGENT_TIERING@,
-- @GLACIER@, or @DEEP_ARCHIVE@ storage class. If your bucket is
-- versioning-enabled (or versioning is suspended), you can set this action
-- to request that Amazon S3 transition noncurrent object versions to the
-- @STANDARD_IA@, @ONEZONE_IA@, @INTELLIGENT_TIERING@, @GLACIER@, or
-- @DEEP_ARCHIVE@ storage class at a specific period in the object\'s
-- lifetime.
--
-- /See:/ 'newNoncurrentVersionTransition' smart constructor.
data NoncurrentVersionTransition = NoncurrentVersionTransition'
  { -- | Specifies the number of days an object is noncurrent before Amazon S3
    -- can perform the associated action. For information about the noncurrent
    -- days calculations, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html#non-current-days-calculations How Amazon S3 Calculates How Long an Object Has Been Noncurrent>
    -- in the /Amazon Simple Storage Service Developer Guide/.
    noncurrentDays :: Prelude.Int,
    -- | The class of storage used to store the object.
    storageClass :: TransitionStorageClass
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'NoncurrentVersionTransition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'noncurrentDays', 'noncurrentVersionTransition_noncurrentDays' - Specifies the number of days an object is noncurrent before Amazon S3
-- can perform the associated action. For information about the noncurrent
-- days calculations, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html#non-current-days-calculations How Amazon S3 Calculates How Long an Object Has Been Noncurrent>
-- in the /Amazon Simple Storage Service Developer Guide/.
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
      { noncurrentDays =
          pNoncurrentDays_,
        storageClass = pStorageClass_
      }

-- | Specifies the number of days an object is noncurrent before Amazon S3
-- can perform the associated action. For information about the noncurrent
-- days calculations, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html#non-current-days-calculations How Amazon S3 Calculates How Long an Object Has Been Noncurrent>
-- in the /Amazon Simple Storage Service Developer Guide/.
noncurrentVersionTransition_noncurrentDays :: Lens.Lens' NoncurrentVersionTransition Prelude.Int
noncurrentVersionTransition_noncurrentDays = Lens.lens (\NoncurrentVersionTransition' {noncurrentDays} -> noncurrentDays) (\s@NoncurrentVersionTransition' {} a -> s {noncurrentDays = a} :: NoncurrentVersionTransition)

-- | The class of storage used to store the object.
noncurrentVersionTransition_storageClass :: Lens.Lens' NoncurrentVersionTransition TransitionStorageClass
noncurrentVersionTransition_storageClass = Lens.lens (\NoncurrentVersionTransition' {storageClass} -> storageClass) (\s@NoncurrentVersionTransition' {} a -> s {storageClass = a} :: NoncurrentVersionTransition)

instance Prelude.FromXML NoncurrentVersionTransition where
  parseXML x =
    NoncurrentVersionTransition'
      Prelude.<$> (x Prelude..@ "NoncurrentDays")
      Prelude.<*> (x Prelude..@ "StorageClass")

instance Prelude.Hashable NoncurrentVersionTransition

instance Prelude.NFData NoncurrentVersionTransition

instance Prelude.ToXML NoncurrentVersionTransition where
  toXML NoncurrentVersionTransition' {..} =
    Prelude.mconcat
      [ "NoncurrentDays" Prelude.@= noncurrentDays,
        "StorageClass" Prelude.@= storageClass
      ]
