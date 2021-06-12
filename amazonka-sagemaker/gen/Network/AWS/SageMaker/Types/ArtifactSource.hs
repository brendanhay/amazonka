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
-- Module      : Network.AWS.SageMaker.Types.ArtifactSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ArtifactSource where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.ArtifactSourceType

-- | A structure describing the source of an artifact.
--
-- /See:/ 'newArtifactSource' smart constructor.
data ArtifactSource = ArtifactSource'
  { -- | A list of source types.
    sourceTypes :: Core.Maybe [ArtifactSourceType],
    -- | The URI of the source.
    sourceUri :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ArtifactSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceTypes', 'artifactSource_sourceTypes' - A list of source types.
--
-- 'sourceUri', 'artifactSource_sourceUri' - The URI of the source.
newArtifactSource ::
  -- | 'sourceUri'
  Core.Text ->
  ArtifactSource
newArtifactSource pSourceUri_ =
  ArtifactSource'
    { sourceTypes = Core.Nothing,
      sourceUri = pSourceUri_
    }

-- | A list of source types.
artifactSource_sourceTypes :: Lens.Lens' ArtifactSource (Core.Maybe [ArtifactSourceType])
artifactSource_sourceTypes = Lens.lens (\ArtifactSource' {sourceTypes} -> sourceTypes) (\s@ArtifactSource' {} a -> s {sourceTypes = a} :: ArtifactSource) Core.. Lens.mapping Lens._Coerce

-- | The URI of the source.
artifactSource_sourceUri :: Lens.Lens' ArtifactSource Core.Text
artifactSource_sourceUri = Lens.lens (\ArtifactSource' {sourceUri} -> sourceUri) (\s@ArtifactSource' {} a -> s {sourceUri = a} :: ArtifactSource)

instance Core.FromJSON ArtifactSource where
  parseJSON =
    Core.withObject
      "ArtifactSource"
      ( \x ->
          ArtifactSource'
            Core.<$> (x Core..:? "SourceTypes" Core..!= Core.mempty)
            Core.<*> (x Core..: "SourceUri")
      )

instance Core.Hashable ArtifactSource

instance Core.NFData ArtifactSource

instance Core.ToJSON ArtifactSource where
  toJSON ArtifactSource' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SourceTypes" Core..=) Core.<$> sourceTypes,
            Core.Just ("SourceUri" Core..= sourceUri)
          ]
      )
