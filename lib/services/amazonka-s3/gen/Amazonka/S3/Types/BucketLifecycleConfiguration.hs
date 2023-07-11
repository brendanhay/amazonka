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
-- Module      : Amazonka.S3.Types.BucketLifecycleConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.BucketLifecycleConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.LifecycleRule

-- | Specifies the lifecycle configuration for objects in an Amazon S3
-- bucket. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lifecycle-mgmt.html Object Lifecycle Management>
-- in the /Amazon S3 User Guide/.
--
-- /See:/ 'newBucketLifecycleConfiguration' smart constructor.
data BucketLifecycleConfiguration = BucketLifecycleConfiguration'
  { -- | A lifecycle rule for individual objects in an Amazon S3 bucket.
    rules :: [LifecycleRule]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BucketLifecycleConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rules', 'bucketLifecycleConfiguration_rules' - A lifecycle rule for individual objects in an Amazon S3 bucket.
newBucketLifecycleConfiguration ::
  BucketLifecycleConfiguration
newBucketLifecycleConfiguration =
  BucketLifecycleConfiguration'
    { rules =
        Prelude.mempty
    }

-- | A lifecycle rule for individual objects in an Amazon S3 bucket.
bucketLifecycleConfiguration_rules :: Lens.Lens' BucketLifecycleConfiguration [LifecycleRule]
bucketLifecycleConfiguration_rules = Lens.lens (\BucketLifecycleConfiguration' {rules} -> rules) (\s@BucketLifecycleConfiguration' {} a -> s {rules = a} :: BucketLifecycleConfiguration) Prelude.. Lens.coerced

instance
  Prelude.Hashable
    BucketLifecycleConfiguration
  where
  hashWithSalt _salt BucketLifecycleConfiguration' {..} =
    _salt `Prelude.hashWithSalt` rules

instance Prelude.NFData BucketLifecycleConfiguration where
  rnf BucketLifecycleConfiguration' {..} =
    Prelude.rnf rules

instance Data.ToXML BucketLifecycleConfiguration where
  toXML BucketLifecycleConfiguration' {..} =
    Prelude.mconcat [Data.toXMLList "Rule" rules]
