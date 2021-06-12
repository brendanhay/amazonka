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
-- Module      : Network.AWS.Rekognition.Types.GroundTruthManifest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.GroundTruthManifest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types.S3Object

-- | The S3 bucket that contains an Amazon Sagemaker Ground Truth format
-- manifest file.
--
-- /See:/ 'newGroundTruthManifest' smart constructor.
data GroundTruthManifest = GroundTruthManifest'
  { s3Object :: Core.Maybe S3Object
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  GroundTruthManifest' {s3Object = Core.Nothing}

-- | Undocumented member.
groundTruthManifest_s3Object :: Lens.Lens' GroundTruthManifest (Core.Maybe S3Object)
groundTruthManifest_s3Object = Lens.lens (\GroundTruthManifest' {s3Object} -> s3Object) (\s@GroundTruthManifest' {} a -> s {s3Object = a} :: GroundTruthManifest)

instance Core.FromJSON GroundTruthManifest where
  parseJSON =
    Core.withObject
      "GroundTruthManifest"
      ( \x ->
          GroundTruthManifest'
            Core.<$> (x Core..:? "S3Object")
      )

instance Core.Hashable GroundTruthManifest

instance Core.NFData GroundTruthManifest

instance Core.ToJSON GroundTruthManifest where
  toJSON GroundTruthManifest' {..} =
    Core.object
      ( Core.catMaybes
          [("S3Object" Core..=) Core.<$> s3Object]
      )
