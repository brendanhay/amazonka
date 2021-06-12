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
-- Module      : Network.AWS.SageMaker.Types.ArtifactSourceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ArtifactSourceType where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.ArtifactSourceIdType

-- | The ID and ID type of an artifact source.
--
-- /See:/ 'newArtifactSourceType' smart constructor.
data ArtifactSourceType = ArtifactSourceType'
  { -- | The type of ID.
    sourceIdType :: ArtifactSourceIdType,
    -- | The ID.
    value :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ArtifactSourceType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceIdType', 'artifactSourceType_sourceIdType' - The type of ID.
--
-- 'value', 'artifactSourceType_value' - The ID.
newArtifactSourceType ::
  -- | 'sourceIdType'
  ArtifactSourceIdType ->
  -- | 'value'
  Core.Text ->
  ArtifactSourceType
newArtifactSourceType pSourceIdType_ pValue_ =
  ArtifactSourceType'
    { sourceIdType = pSourceIdType_,
      value = pValue_
    }

-- | The type of ID.
artifactSourceType_sourceIdType :: Lens.Lens' ArtifactSourceType ArtifactSourceIdType
artifactSourceType_sourceIdType = Lens.lens (\ArtifactSourceType' {sourceIdType} -> sourceIdType) (\s@ArtifactSourceType' {} a -> s {sourceIdType = a} :: ArtifactSourceType)

-- | The ID.
artifactSourceType_value :: Lens.Lens' ArtifactSourceType Core.Text
artifactSourceType_value = Lens.lens (\ArtifactSourceType' {value} -> value) (\s@ArtifactSourceType' {} a -> s {value = a} :: ArtifactSourceType)

instance Core.FromJSON ArtifactSourceType where
  parseJSON =
    Core.withObject
      "ArtifactSourceType"
      ( \x ->
          ArtifactSourceType'
            Core.<$> (x Core..: "SourceIdType")
            Core.<*> (x Core..: "Value")
      )

instance Core.Hashable ArtifactSourceType

instance Core.NFData ArtifactSourceType

instance Core.ToJSON ArtifactSourceType where
  toJSON ArtifactSourceType' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SourceIdType" Core..= sourceIdType),
            Core.Just ("Value" Core..= value)
          ]
      )
