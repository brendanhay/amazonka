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
-- Module      : Amazonka.CloudSearchDomains.Types.BucketInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearchDomains.Types.BucketInfo where

import Amazonka.CloudSearchDomains.Types.Bucket
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A container for the calculated facet values and counts.
--
-- /See:/ 'newBucketInfo' smart constructor.
data BucketInfo = BucketInfo'
  { -- | A list of the calculated facet values and counts.
    buckets :: Prelude.Maybe [Bucket]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
bucketInfo_buckets = Lens.lens (\BucketInfo' {buckets} -> buckets) (\s@BucketInfo' {} a -> s {buckets = a} :: BucketInfo) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON BucketInfo where
  parseJSON =
    Data.withObject
      "BucketInfo"
      ( \x ->
          BucketInfo'
            Prelude.<$> (x Data..:? "buckets" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable BucketInfo where
  hashWithSalt _salt BucketInfo' {..} =
    _salt `Prelude.hashWithSalt` buckets

instance Prelude.NFData BucketInfo where
  rnf BucketInfo' {..} = Prelude.rnf buckets
