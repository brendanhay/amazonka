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
-- Module      : Network.AWS.SageMaker.Types.ArtifactSourceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ArtifactSourceType where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.ArtifactSourceIdType

-- | The ID and ID type of an artifact source.
--
-- /See:/ 'newArtifactSourceType' smart constructor.
data ArtifactSourceType = ArtifactSourceType'
  { -- | The type of ID.
    sourceIdType :: ArtifactSourceIdType,
    -- | The ID.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
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
artifactSourceType_value :: Lens.Lens' ArtifactSourceType Prelude.Text
artifactSourceType_value = Lens.lens (\ArtifactSourceType' {value} -> value) (\s@ArtifactSourceType' {} a -> s {value = a} :: ArtifactSourceType)

instance Prelude.FromJSON ArtifactSourceType where
  parseJSON =
    Prelude.withObject
      "ArtifactSourceType"
      ( \x ->
          ArtifactSourceType'
            Prelude.<$> (x Prelude..: "SourceIdType")
            Prelude.<*> (x Prelude..: "Value")
      )

instance Prelude.Hashable ArtifactSourceType

instance Prelude.NFData ArtifactSourceType

instance Prelude.ToJSON ArtifactSourceType where
  toJSON ArtifactSourceType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SourceIdType" Prelude..= sourceIdType),
            Prelude.Just ("Value" Prelude..= value)
          ]
      )
