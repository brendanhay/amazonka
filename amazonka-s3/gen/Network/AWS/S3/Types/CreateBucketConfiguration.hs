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
-- Module      : Network.AWS.S3.Types.CreateBucketConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.CreateBucketConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.S3.Internal

-- | The configuration information for the bucket.
--
-- /See:/ 'newCreateBucketConfiguration' smart constructor.
data CreateBucketConfiguration = CreateBucketConfiguration'
  { -- | Specifies the Region where the bucket will be created. If you don\'t
    -- specify a Region, the bucket is created in the US East (N. Virginia)
    -- Region (us-east-1).
    locationConstraint :: Core.Maybe LocationConstraint
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateBucketConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locationConstraint', 'createBucketConfiguration_locationConstraint' - Specifies the Region where the bucket will be created. If you don\'t
-- specify a Region, the bucket is created in the US East (N. Virginia)
-- Region (us-east-1).
newCreateBucketConfiguration ::
  CreateBucketConfiguration
newCreateBucketConfiguration =
  CreateBucketConfiguration'
    { locationConstraint =
        Core.Nothing
    }

-- | Specifies the Region where the bucket will be created. If you don\'t
-- specify a Region, the bucket is created in the US East (N. Virginia)
-- Region (us-east-1).
createBucketConfiguration_locationConstraint :: Lens.Lens' CreateBucketConfiguration (Core.Maybe LocationConstraint)
createBucketConfiguration_locationConstraint = Lens.lens (\CreateBucketConfiguration' {locationConstraint} -> locationConstraint) (\s@CreateBucketConfiguration' {} a -> s {locationConstraint = a} :: CreateBucketConfiguration)

instance Core.Hashable CreateBucketConfiguration

instance Core.NFData CreateBucketConfiguration

instance Core.ToXML CreateBucketConfiguration where
  toXML CreateBucketConfiguration' {..} =
    Core.mconcat
      ["LocationConstraint" Core.@= locationConstraint]
