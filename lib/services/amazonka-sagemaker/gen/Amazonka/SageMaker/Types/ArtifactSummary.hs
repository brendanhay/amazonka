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
-- Module      : Amazonka.SageMaker.Types.ArtifactSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ArtifactSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ArtifactSource

-- | Lists a summary of the properties of an artifact. An artifact represents
-- a URI addressable object or data. Some examples are a dataset and a
-- model.
--
-- /See:/ 'newArtifactSummary' smart constructor.
data ArtifactSummary = ArtifactSummary'
  { -- | The name of the artifact.
    artifactName :: Prelude.Maybe Prelude.Text,
    -- | The type of the artifact.
    artifactType :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the artifact.
    artifactArn :: Prelude.Maybe Prelude.Text,
    -- | When the artifact was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The source of the artifact.
    source :: Prelude.Maybe ArtifactSource,
    -- | When the artifact was created.
    creationTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ArtifactSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'artifactName', 'artifactSummary_artifactName' - The name of the artifact.
--
-- 'artifactType', 'artifactSummary_artifactType' - The type of the artifact.
--
-- 'artifactArn', 'artifactSummary_artifactArn' - The Amazon Resource Name (ARN) of the artifact.
--
-- 'lastModifiedTime', 'artifactSummary_lastModifiedTime' - When the artifact was last modified.
--
-- 'source', 'artifactSummary_source' - The source of the artifact.
--
-- 'creationTime', 'artifactSummary_creationTime' - When the artifact was created.
newArtifactSummary ::
  ArtifactSummary
newArtifactSummary =
  ArtifactSummary'
    { artifactName = Prelude.Nothing,
      artifactType = Prelude.Nothing,
      artifactArn = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      source = Prelude.Nothing,
      creationTime = Prelude.Nothing
    }

-- | The name of the artifact.
artifactSummary_artifactName :: Lens.Lens' ArtifactSummary (Prelude.Maybe Prelude.Text)
artifactSummary_artifactName = Lens.lens (\ArtifactSummary' {artifactName} -> artifactName) (\s@ArtifactSummary' {} a -> s {artifactName = a} :: ArtifactSummary)

-- | The type of the artifact.
artifactSummary_artifactType :: Lens.Lens' ArtifactSummary (Prelude.Maybe Prelude.Text)
artifactSummary_artifactType = Lens.lens (\ArtifactSummary' {artifactType} -> artifactType) (\s@ArtifactSummary' {} a -> s {artifactType = a} :: ArtifactSummary)

-- | The Amazon Resource Name (ARN) of the artifact.
artifactSummary_artifactArn :: Lens.Lens' ArtifactSummary (Prelude.Maybe Prelude.Text)
artifactSummary_artifactArn = Lens.lens (\ArtifactSummary' {artifactArn} -> artifactArn) (\s@ArtifactSummary' {} a -> s {artifactArn = a} :: ArtifactSummary)

-- | When the artifact was last modified.
artifactSummary_lastModifiedTime :: Lens.Lens' ArtifactSummary (Prelude.Maybe Prelude.UTCTime)
artifactSummary_lastModifiedTime = Lens.lens (\ArtifactSummary' {lastModifiedTime} -> lastModifiedTime) (\s@ArtifactSummary' {} a -> s {lastModifiedTime = a} :: ArtifactSummary) Prelude.. Lens.mapping Core._Time

-- | The source of the artifact.
artifactSummary_source :: Lens.Lens' ArtifactSummary (Prelude.Maybe ArtifactSource)
artifactSummary_source = Lens.lens (\ArtifactSummary' {source} -> source) (\s@ArtifactSummary' {} a -> s {source = a} :: ArtifactSummary)

-- | When the artifact was created.
artifactSummary_creationTime :: Lens.Lens' ArtifactSummary (Prelude.Maybe Prelude.UTCTime)
artifactSummary_creationTime = Lens.lens (\ArtifactSummary' {creationTime} -> creationTime) (\s@ArtifactSummary' {} a -> s {creationTime = a} :: ArtifactSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON ArtifactSummary where
  parseJSON =
    Core.withObject
      "ArtifactSummary"
      ( \x ->
          ArtifactSummary'
            Prelude.<$> (x Core..:? "ArtifactName")
            Prelude.<*> (x Core..:? "ArtifactType")
            Prelude.<*> (x Core..:? "ArtifactArn")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "Source")
            Prelude.<*> (x Core..:? "CreationTime")
      )

instance Prelude.Hashable ArtifactSummary where
  hashWithSalt _salt ArtifactSummary' {..} =
    _salt `Prelude.hashWithSalt` artifactName
      `Prelude.hashWithSalt` artifactType
      `Prelude.hashWithSalt` artifactArn
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData ArtifactSummary where
  rnf ArtifactSummary' {..} =
    Prelude.rnf artifactName
      `Prelude.seq` Prelude.rnf artifactType
      `Prelude.seq` Prelude.rnf artifactArn
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf creationTime
