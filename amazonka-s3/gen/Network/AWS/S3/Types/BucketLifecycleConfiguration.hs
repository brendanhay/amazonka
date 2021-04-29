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
-- Module      : Network.AWS.S3.Types.BucketLifecycleConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.BucketLifecycleConfiguration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.LifecycleRule

-- | Specifies the lifecycle configuration for objects in an Amazon S3
-- bucket. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lifecycle-mgmt.html Object Lifecycle Management>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- /See:/ 'newBucketLifecycleConfiguration' smart constructor.
data BucketLifecycleConfiguration = BucketLifecycleConfiguration'
  { -- | A lifecycle rule for individual objects in an Amazon S3 bucket.
    rules :: [LifecycleRule]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
bucketLifecycleConfiguration_rules = Lens.lens (\BucketLifecycleConfiguration' {rules} -> rules) (\s@BucketLifecycleConfiguration' {} a -> s {rules = a} :: BucketLifecycleConfiguration) Prelude.. Prelude._Coerce

instance
  Prelude.Hashable
    BucketLifecycleConfiguration

instance Prelude.NFData BucketLifecycleConfiguration

instance Prelude.ToXML BucketLifecycleConfiguration where
  toXML BucketLifecycleConfiguration' {..} =
    Prelude.mconcat [Prelude.toXMLList "Rule" rules]
