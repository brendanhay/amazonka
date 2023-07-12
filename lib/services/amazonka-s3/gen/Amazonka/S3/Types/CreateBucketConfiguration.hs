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
-- Module      : Amazonka.S3.Types.CreateBucketConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.CreateBucketConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

-- | The configuration information for the bucket.
--
-- /See:/ 'newCreateBucketConfiguration' smart constructor.
data CreateBucketConfiguration = CreateBucketConfiguration'
  { -- | Specifies the Region where the bucket will be created. If you don\'t
    -- specify a Region, the bucket is created in the US East (N. Virginia)
    -- Region (us-east-1).
    locationConstraint :: Prelude.Maybe LocationConstraint
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | Specifies the Region where the bucket will be created. If you don\'t
-- specify a Region, the bucket is created in the US East (N. Virginia)
-- Region (us-east-1).
createBucketConfiguration_locationConstraint :: Lens.Lens' CreateBucketConfiguration (Prelude.Maybe LocationConstraint)
createBucketConfiguration_locationConstraint = Lens.lens (\CreateBucketConfiguration' {locationConstraint} -> locationConstraint) (\s@CreateBucketConfiguration' {} a -> s {locationConstraint = a} :: CreateBucketConfiguration)

instance Prelude.Hashable CreateBucketConfiguration where
  hashWithSalt _salt CreateBucketConfiguration' {..} =
    _salt `Prelude.hashWithSalt` locationConstraint

instance Prelude.NFData CreateBucketConfiguration where
  rnf CreateBucketConfiguration' {..} =
    Prelude.rnf locationConstraint

instance Data.ToXML CreateBucketConfiguration where
  toXML CreateBucketConfiguration' {..} =
    Prelude.mconcat
      ["LocationConstraint" Data.@= locationConstraint]
