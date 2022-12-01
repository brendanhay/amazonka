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
-- Module      : Amazonka.Rekognition.Types.GroundTruthManifest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.GroundTruthManifest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.S3Object

-- | The S3 bucket that contains an Amazon Sagemaker Ground Truth format
-- manifest file.
--
-- /See:/ 'newGroundTruthManifest' smart constructor.
data GroundTruthManifest = GroundTruthManifest'
  { s3Object :: Prelude.Maybe S3Object
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GroundTruthManifest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Object', 'groundTruthManifest_s3Object' - Undocumented member.
newGroundTruthManifest ::
  GroundTruthManifest
newGroundTruthManifest =
  GroundTruthManifest' {s3Object = Prelude.Nothing}

-- | Undocumented member.
groundTruthManifest_s3Object :: Lens.Lens' GroundTruthManifest (Prelude.Maybe S3Object)
groundTruthManifest_s3Object = Lens.lens (\GroundTruthManifest' {s3Object} -> s3Object) (\s@GroundTruthManifest' {} a -> s {s3Object = a} :: GroundTruthManifest)

instance Core.FromJSON GroundTruthManifest where
  parseJSON =
    Core.withObject
      "GroundTruthManifest"
      ( \x ->
          GroundTruthManifest'
            Prelude.<$> (x Core..:? "S3Object")
      )

instance Prelude.Hashable GroundTruthManifest where
  hashWithSalt _salt GroundTruthManifest' {..} =
    _salt `Prelude.hashWithSalt` s3Object

instance Prelude.NFData GroundTruthManifest where
  rnf GroundTruthManifest' {..} = Prelude.rnf s3Object

instance Core.ToJSON GroundTruthManifest where
  toJSON GroundTruthManifest' {..} =
    Core.object
      ( Prelude.catMaybes
          [("S3Object" Core..=) Prelude.<$> s3Object]
      )
