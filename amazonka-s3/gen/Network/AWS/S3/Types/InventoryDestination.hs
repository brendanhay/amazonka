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
-- Module      : Network.AWS.S3.Types.InventoryDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.InventoryDestination where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.InventoryS3BucketDestination

-- | Specifies the inventory configuration for an Amazon S3 bucket.
--
-- /See:/ 'newInventoryDestination' smart constructor.
data InventoryDestination = InventoryDestination'
  { -- | Contains the bucket name, file format, bucket owner (optional), and
    -- prefix (optional) where inventory results are published.
    s3BucketDestination :: InventoryS3BucketDestination
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InventoryDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3BucketDestination', 'inventoryDestination_s3BucketDestination' - Contains the bucket name, file format, bucket owner (optional), and
-- prefix (optional) where inventory results are published.
newInventoryDestination ::
  -- | 's3BucketDestination'
  InventoryS3BucketDestination ->
  InventoryDestination
newInventoryDestination pS3BucketDestination_ =
  InventoryDestination'
    { s3BucketDestination =
        pS3BucketDestination_
    }

-- | Contains the bucket name, file format, bucket owner (optional), and
-- prefix (optional) where inventory results are published.
inventoryDestination_s3BucketDestination :: Lens.Lens' InventoryDestination InventoryS3BucketDestination
inventoryDestination_s3BucketDestination = Lens.lens (\InventoryDestination' {s3BucketDestination} -> s3BucketDestination) (\s@InventoryDestination' {} a -> s {s3BucketDestination = a} :: InventoryDestination)

instance Prelude.FromXML InventoryDestination where
  parseXML x =
    InventoryDestination'
      Prelude.<$> (x Prelude..@ "S3BucketDestination")

instance Prelude.Hashable InventoryDestination

instance Prelude.NFData InventoryDestination

instance Prelude.ToXML InventoryDestination where
  toXML InventoryDestination' {..} =
    Prelude.mconcat
      [ "S3BucketDestination"
          Prelude.@= s3BucketDestination
      ]
