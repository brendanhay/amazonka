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
-- Module      : Network.AWS.SageMaker.Types.ModelDigests
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelDigests where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information to verify the integrity of stored model artifacts.
--
-- /See:/ 'newModelDigests' smart constructor.
data ModelDigests = ModelDigests'
  { -- | Provides a hash value that uniquely identifies the stored model
    -- artifacts.
    artifactDigest :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  ModelDigests' {artifactDigest = Prelude.Nothing}

-- | Provides a hash value that uniquely identifies the stored model
-- artifacts.
modelDigests_artifactDigest :: Lens.Lens' ModelDigests (Prelude.Maybe Prelude.Text)
modelDigests_artifactDigest = Lens.lens (\ModelDigests' {artifactDigest} -> artifactDigest) (\s@ModelDigests' {} a -> s {artifactDigest = a} :: ModelDigests)

instance Prelude.FromJSON ModelDigests where
  parseJSON =
    Prelude.withObject
      "ModelDigests"
      ( \x ->
          ModelDigests'
            Prelude.<$> (x Prelude..:? "ArtifactDigest")
      )

instance Prelude.Hashable ModelDigests

instance Prelude.NFData ModelDigests
