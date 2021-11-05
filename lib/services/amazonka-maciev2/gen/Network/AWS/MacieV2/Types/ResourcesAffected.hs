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
-- Module      : Network.AWS.MacieV2.Types.ResourcesAffected
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MacieV2.Types.ResourcesAffected where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MacieV2.Types.S3Bucket
import Network.AWS.MacieV2.Types.S3Object
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about the resources that a finding applies to.
--
-- /See:/ 'newResourcesAffected' smart constructor.
data ResourcesAffected = ResourcesAffected'
  { -- | The details of the S3 object that the finding applies to.
    s3Object :: Prelude.Maybe S3Object,
    -- | The details of the S3 bucket that the finding applies to.
    s3Bucket :: Prelude.Maybe S3Bucket
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourcesAffected' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Object', 'resourcesAffected_s3Object' - The details of the S3 object that the finding applies to.
--
-- 's3Bucket', 'resourcesAffected_s3Bucket' - The details of the S3 bucket that the finding applies to.
newResourcesAffected ::
  ResourcesAffected
newResourcesAffected =
  ResourcesAffected'
    { s3Object = Prelude.Nothing,
      s3Bucket = Prelude.Nothing
    }

-- | The details of the S3 object that the finding applies to.
resourcesAffected_s3Object :: Lens.Lens' ResourcesAffected (Prelude.Maybe S3Object)
resourcesAffected_s3Object = Lens.lens (\ResourcesAffected' {s3Object} -> s3Object) (\s@ResourcesAffected' {} a -> s {s3Object = a} :: ResourcesAffected)

-- | The details of the S3 bucket that the finding applies to.
resourcesAffected_s3Bucket :: Lens.Lens' ResourcesAffected (Prelude.Maybe S3Bucket)
resourcesAffected_s3Bucket = Lens.lens (\ResourcesAffected' {s3Bucket} -> s3Bucket) (\s@ResourcesAffected' {} a -> s {s3Bucket = a} :: ResourcesAffected)

instance Core.FromJSON ResourcesAffected where
  parseJSON =
    Core.withObject
      "ResourcesAffected"
      ( \x ->
          ResourcesAffected'
            Prelude.<$> (x Core..:? "s3Object")
            Prelude.<*> (x Core..:? "s3Bucket")
      )

instance Prelude.Hashable ResourcesAffected

instance Prelude.NFData ResourcesAffected
