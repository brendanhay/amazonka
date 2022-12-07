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
-- Module      : Amazonka.SageMaker.Types.ArtifactSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ArtifactSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ArtifactSourceType

-- | A structure describing the source of an artifact.
--
-- /See:/ 'newArtifactSource' smart constructor.
data ArtifactSource = ArtifactSource'
  { -- | A list of source types.
    sourceTypes :: Prelude.Maybe [ArtifactSourceType],
    -- | The URI of the source.
    sourceUri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ArtifactSource
newArtifactSource pSourceUri_ =
  ArtifactSource'
    { sourceTypes = Prelude.Nothing,
      sourceUri = pSourceUri_
    }

-- | A list of source types.
artifactSource_sourceTypes :: Lens.Lens' ArtifactSource (Prelude.Maybe [ArtifactSourceType])
artifactSource_sourceTypes = Lens.lens (\ArtifactSource' {sourceTypes} -> sourceTypes) (\s@ArtifactSource' {} a -> s {sourceTypes = a} :: ArtifactSource) Prelude.. Lens.mapping Lens.coerced

-- | The URI of the source.
artifactSource_sourceUri :: Lens.Lens' ArtifactSource Prelude.Text
artifactSource_sourceUri = Lens.lens (\ArtifactSource' {sourceUri} -> sourceUri) (\s@ArtifactSource' {} a -> s {sourceUri = a} :: ArtifactSource)

instance Data.FromJSON ArtifactSource where
  parseJSON =
    Data.withObject
      "ArtifactSource"
      ( \x ->
          ArtifactSource'
            Prelude.<$> (x Data..:? "SourceTypes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "SourceUri")
      )

instance Prelude.Hashable ArtifactSource where
  hashWithSalt _salt ArtifactSource' {..} =
    _salt `Prelude.hashWithSalt` sourceTypes
      `Prelude.hashWithSalt` sourceUri

instance Prelude.NFData ArtifactSource where
  rnf ArtifactSource' {..} =
    Prelude.rnf sourceTypes
      `Prelude.seq` Prelude.rnf sourceUri

instance Data.ToJSON ArtifactSource where
  toJSON ArtifactSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SourceTypes" Data..=) Prelude.<$> sourceTypes,
            Prelude.Just ("SourceUri" Data..= sourceUri)
          ]
      )
