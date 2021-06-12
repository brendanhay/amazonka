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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A container for the calculated facet values and counts.
--
-- /See:/ 'newBucketInfo' smart constructor.
data BucketInfo = BucketInfo'
  { -- | A list of the calculated facet values and counts.
    buckets :: Core.Maybe [Bucket]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
newBucketInfo = BucketInfo' {buckets = Core.Nothing}

-- | A list of the calculated facet values and counts.
bucketInfo_buckets :: Lens.Lens' BucketInfo (Core.Maybe [Bucket])
bucketInfo_buckets = Lens.lens (\BucketInfo' {buckets} -> buckets) (\s@BucketInfo' {} a -> s {buckets = a} :: BucketInfo) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON BucketInfo where
  parseJSON =
    Core.withObject
      "BucketInfo"
      ( \x ->
          BucketInfo'
            Core.<$> (x Core..:? "buckets" Core..!= Core.mempty)
      )

instance Core.Hashable BucketInfo

instance Core.NFData BucketInfo
