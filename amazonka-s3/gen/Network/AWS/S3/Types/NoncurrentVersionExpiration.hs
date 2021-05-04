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
-- Module      : Network.AWS.S3.Types.NoncurrentVersionExpiration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.NoncurrentVersionExpiration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal

-- | Specifies when noncurrent object versions expire. Upon expiration,
-- Amazon S3 permanently deletes the noncurrent object versions. You set
-- this lifecycle configuration action on a bucket that has versioning
-- enabled (or suspended) to request that Amazon S3 delete noncurrent
-- object versions at a specific period in the object\'s lifetime.
--
-- /See:/ 'newNoncurrentVersionExpiration' smart constructor.
data NoncurrentVersionExpiration = NoncurrentVersionExpiration'
  { -- | Specifies the number of days an object is noncurrent before Amazon S3
    -- can perform the associated action. For information about the noncurrent
    -- days calculations, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html#non-current-days-calculations How Amazon S3 Calculates When an Object Became Noncurrent>
    -- in the /Amazon Simple Storage Service Developer Guide/.
    noncurrentDays :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'NoncurrentVersionExpiration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'noncurrentDays', 'noncurrentVersionExpiration_noncurrentDays' - Specifies the number of days an object is noncurrent before Amazon S3
-- can perform the associated action. For information about the noncurrent
-- days calculations, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html#non-current-days-calculations How Amazon S3 Calculates When an Object Became Noncurrent>
-- in the /Amazon Simple Storage Service Developer Guide/.
newNoncurrentVersionExpiration ::
  -- | 'noncurrentDays'
  Prelude.Int ->
  NoncurrentVersionExpiration
newNoncurrentVersionExpiration pNoncurrentDays_ =
  NoncurrentVersionExpiration'
    { noncurrentDays =
        pNoncurrentDays_
    }

-- | Specifies the number of days an object is noncurrent before Amazon S3
-- can perform the associated action. For information about the noncurrent
-- days calculations, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html#non-current-days-calculations How Amazon S3 Calculates When an Object Became Noncurrent>
-- in the /Amazon Simple Storage Service Developer Guide/.
noncurrentVersionExpiration_noncurrentDays :: Lens.Lens' NoncurrentVersionExpiration Prelude.Int
noncurrentVersionExpiration_noncurrentDays = Lens.lens (\NoncurrentVersionExpiration' {noncurrentDays} -> noncurrentDays) (\s@NoncurrentVersionExpiration' {} a -> s {noncurrentDays = a} :: NoncurrentVersionExpiration)

instance Prelude.FromXML NoncurrentVersionExpiration where
  parseXML x =
    NoncurrentVersionExpiration'
      Prelude.<$> (x Prelude..@ "NoncurrentDays")

instance Prelude.Hashable NoncurrentVersionExpiration

instance Prelude.NFData NoncurrentVersionExpiration

instance Prelude.ToXML NoncurrentVersionExpiration where
  toXML NoncurrentVersionExpiration' {..} =
    Prelude.mconcat
      ["NoncurrentDays" Prelude.@= noncurrentDays]
