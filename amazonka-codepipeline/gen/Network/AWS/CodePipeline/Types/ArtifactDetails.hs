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
-- Module      : Network.AWS.CodePipeline.Types.ArtifactDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ArtifactDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Returns information about the details of an artifact.
--
-- /See:/ 'newArtifactDetails' smart constructor.
data ArtifactDetails = ArtifactDetails'
  { -- | The minimum number of artifacts allowed for the action type.
    minimumCount :: Prelude.Natural,
    -- | The maximum number of artifacts allowed for the action type.
    maximumCount :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ArtifactDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minimumCount', 'artifactDetails_minimumCount' - The minimum number of artifacts allowed for the action type.
--
-- 'maximumCount', 'artifactDetails_maximumCount' - The maximum number of artifacts allowed for the action type.
newArtifactDetails ::
  -- | 'minimumCount'
  Prelude.Natural ->
  -- | 'maximumCount'
  Prelude.Natural ->
  ArtifactDetails
newArtifactDetails pMinimumCount_ pMaximumCount_ =
  ArtifactDetails'
    { minimumCount = pMinimumCount_,
      maximumCount = pMaximumCount_
    }

-- | The minimum number of artifacts allowed for the action type.
artifactDetails_minimumCount :: Lens.Lens' ArtifactDetails Prelude.Natural
artifactDetails_minimumCount = Lens.lens (\ArtifactDetails' {minimumCount} -> minimumCount) (\s@ArtifactDetails' {} a -> s {minimumCount = a} :: ArtifactDetails)

-- | The maximum number of artifacts allowed for the action type.
artifactDetails_maximumCount :: Lens.Lens' ArtifactDetails Prelude.Natural
artifactDetails_maximumCount = Lens.lens (\ArtifactDetails' {maximumCount} -> maximumCount) (\s@ArtifactDetails' {} a -> s {maximumCount = a} :: ArtifactDetails)

instance Core.FromJSON ArtifactDetails where
  parseJSON =
    Core.withObject
      "ArtifactDetails"
      ( \x ->
          ArtifactDetails'
            Prelude.<$> (x Core..: "minimumCount")
            Prelude.<*> (x Core..: "maximumCount")
      )

instance Prelude.Hashable ArtifactDetails

instance Prelude.NFData ArtifactDetails

instance Core.ToJSON ArtifactDetails where
  toJSON ArtifactDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("minimumCount" Core..= minimumCount),
            Prelude.Just ("maximumCount" Core..= maximumCount)
          ]
      )
