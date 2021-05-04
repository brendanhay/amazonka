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
-- Module      : Network.AWS.Rekognition.Types.GroundTruthManifest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.GroundTruthManifest where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.S3Object

-- | The S3 bucket that contains an Amazon Sagemaker Ground Truth format
-- manifest file.
--
-- /See:/ 'newGroundTruthManifest' smart constructor.
data GroundTruthManifest = GroundTruthManifest'
  { s3Object :: Prelude.Maybe S3Object
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON GroundTruthManifest where
  parseJSON =
    Prelude.withObject
      "GroundTruthManifest"
      ( \x ->
          GroundTruthManifest'
            Prelude.<$> (x Prelude..:? "S3Object")
      )

instance Prelude.Hashable GroundTruthManifest

instance Prelude.NFData GroundTruthManifest

instance Prelude.ToJSON GroundTruthManifest where
  toJSON GroundTruthManifest' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("S3Object" Prelude..=) Prelude.<$> s3Object]
      )
