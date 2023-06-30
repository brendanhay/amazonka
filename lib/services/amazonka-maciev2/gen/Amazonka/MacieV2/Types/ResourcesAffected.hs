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
-- Module      : Amazonka.MacieV2.Types.ResourcesAffected
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.ResourcesAffected where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.S3Bucket
import Amazonka.MacieV2.Types.S3Object
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the resources that a finding applies to.
--
-- /See:/ 'newResourcesAffected' smart constructor.
data ResourcesAffected = ResourcesAffected'
  { -- | The details of the S3 bucket that the finding applies to.
    s3Bucket :: Prelude.Maybe S3Bucket,
    -- | The details of the S3 object that the finding applies to.
    s3Object :: Prelude.Maybe S3Object
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
-- 's3Bucket', 'resourcesAffected_s3Bucket' - The details of the S3 bucket that the finding applies to.
--
-- 's3Object', 'resourcesAffected_s3Object' - The details of the S3 object that the finding applies to.
newResourcesAffected ::
  ResourcesAffected
newResourcesAffected =
  ResourcesAffected'
    { s3Bucket = Prelude.Nothing,
      s3Object = Prelude.Nothing
    }

-- | The details of the S3 bucket that the finding applies to.
resourcesAffected_s3Bucket :: Lens.Lens' ResourcesAffected (Prelude.Maybe S3Bucket)
resourcesAffected_s3Bucket = Lens.lens (\ResourcesAffected' {s3Bucket} -> s3Bucket) (\s@ResourcesAffected' {} a -> s {s3Bucket = a} :: ResourcesAffected)

-- | The details of the S3 object that the finding applies to.
resourcesAffected_s3Object :: Lens.Lens' ResourcesAffected (Prelude.Maybe S3Object)
resourcesAffected_s3Object = Lens.lens (\ResourcesAffected' {s3Object} -> s3Object) (\s@ResourcesAffected' {} a -> s {s3Object = a} :: ResourcesAffected)

instance Data.FromJSON ResourcesAffected where
  parseJSON =
    Data.withObject
      "ResourcesAffected"
      ( \x ->
          ResourcesAffected'
            Prelude.<$> (x Data..:? "s3Bucket")
            Prelude.<*> (x Data..:? "s3Object")
      )

instance Prelude.Hashable ResourcesAffected where
  hashWithSalt _salt ResourcesAffected' {..} =
    _salt
      `Prelude.hashWithSalt` s3Bucket
      `Prelude.hashWithSalt` s3Object

instance Prelude.NFData ResourcesAffected where
  rnf ResourcesAffected' {..} =
    Prelude.rnf s3Bucket
      `Prelude.seq` Prelude.rnf s3Object
