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
-- Module      : Network.AWS.SageMaker.Types.ModelDigests
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelDigests where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides information to verify the integrity of stored model artifacts.
--
-- /See:/ 'newModelDigests' smart constructor.
data ModelDigests = ModelDigests'
  { -- | Provides a hash value that uniquely identifies the stored model
    -- artifacts.
    artifactDigest :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModelDigests' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'artifactDigest', 'modelDigests_artifactDigest' - Provides a hash value that uniquely identifies the stored model
-- artifacts.
newModelDigests ::
  ModelDigests
newModelDigests =
  ModelDigests' {artifactDigest = Core.Nothing}

-- | Provides a hash value that uniquely identifies the stored model
-- artifacts.
modelDigests_artifactDigest :: Lens.Lens' ModelDigests (Core.Maybe Core.Text)
modelDigests_artifactDigest = Lens.lens (\ModelDigests' {artifactDigest} -> artifactDigest) (\s@ModelDigests' {} a -> s {artifactDigest = a} :: ModelDigests)

instance Core.FromJSON ModelDigests where
  parseJSON =
    Core.withObject
      "ModelDigests"
      ( \x ->
          ModelDigests' Core.<$> (x Core..:? "ArtifactDigest")
      )

instance Core.Hashable ModelDigests

instance Core.NFData ModelDigests
