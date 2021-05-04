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
-- Module      : Network.AWS.CloudSearchDomains.Types.BucketInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.BucketInfo where

import Network.AWS.CloudSearchDomains.Types.Bucket
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A container for the calculated facet values and counts.
--
-- /See:/ 'newBucketInfo' smart constructor.
data BucketInfo = BucketInfo'
  { -- | A list of the calculated facet values and counts.
    buckets :: Prelude.Maybe [Bucket]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BucketInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'buckets', 'bucketInfo_buckets' - A list of the calculated facet values and counts.
newBucketInfo ::
  BucketInfo
newBucketInfo =
  BucketInfo' {buckets = Prelude.Nothing}

-- | A list of the calculated facet values and counts.
bucketInfo_buckets :: Lens.Lens' BucketInfo (Prelude.Maybe [Bucket])
bucketInfo_buckets = Lens.lens (\BucketInfo' {buckets} -> buckets) (\s@BucketInfo' {} a -> s {buckets = a} :: BucketInfo) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON BucketInfo where
  parseJSON =
    Prelude.withObject
      "BucketInfo"
      ( \x ->
          BucketInfo'
            Prelude.<$> (x Prelude..:? "buckets" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable BucketInfo

instance Prelude.NFData BucketInfo
