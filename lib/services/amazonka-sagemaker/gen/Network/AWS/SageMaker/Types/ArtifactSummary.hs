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
-- Module      : Network.AWS.SageMaker.Types.ArtifactSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ArtifactSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.ArtifactSource

-- | Lists a summary of the properties of an artifact. An artifact represents
-- a URI addressable object or data. Some examples are a dataset and a
-- model.
--
-- /See:/ 'newArtifactSummary' smart constructor.
data ArtifactSummary = ArtifactSummary'
  { -- | When the artifact was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | When the artifact was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the artifact.
    artifactName :: Prelude.Maybe Prelude.Text,
    -- | The source of the artifact.
    source :: Prelude.Maybe ArtifactSource,
    -- | The Amazon Resource Name (ARN) of the artifact.
    artifactArn :: Prelude.Maybe Prelude.Text,
    -- | The type of the artifact.
    artifactType :: Prelude.Maybe Prelude.Text
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
-- 'creationTime', 'artifactSummary_creationTime' - When the artifact was created.
--
-- 'lastModifiedTime', 'artifactSummary_lastModifiedTime' - When the artifact was last modified.
--
-- 'artifactName', 'artifactSummary_artifactName' - The name of the artifact.
--
-- 'source', 'artifactSummary_source' - The source of the artifact.
--
-- 'artifactArn', 'artifactSummary_artifactArn' - The Amazon Resource Name (ARN) of the artifact.
--
-- 'artifactType', 'artifactSummary_artifactType' - The type of the artifact.
newArtifactSummary ::
  ArtifactSummary
newArtifactSummary =
  ArtifactSummary'
    { creationTime = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      artifactName = Prelude.Nothing,
      source = Prelude.Nothing,
      artifactArn = Prelude.Nothing,
      artifactType = Prelude.Nothing
    }

-- | When the artifact was created.
artifactSummary_creationTime :: Lens.Lens' ArtifactSummary (Prelude.Maybe Prelude.UTCTime)
artifactSummary_creationTime = Lens.lens (\ArtifactSummary' {creationTime} -> creationTime) (\s@ArtifactSummary' {} a -> s {creationTime = a} :: ArtifactSummary) Prelude.. Lens.mapping Core._Time

-- | When the artifact was last modified.
artifactSummary_lastModifiedTime :: Lens.Lens' ArtifactSummary (Prelude.Maybe Prelude.UTCTime)
artifactSummary_lastModifiedTime = Lens.lens (\ArtifactSummary' {lastModifiedTime} -> lastModifiedTime) (\s@ArtifactSummary' {} a -> s {lastModifiedTime = a} :: ArtifactSummary) Prelude.. Lens.mapping Core._Time

-- | The name of the artifact.
artifactSummary_artifactName :: Lens.Lens' ArtifactSummary (Prelude.Maybe Prelude.Text)
artifactSummary_artifactName = Lens.lens (\ArtifactSummary' {artifactName} -> artifactName) (\s@ArtifactSummary' {} a -> s {artifactName = a} :: ArtifactSummary)

-- | The source of the artifact.
artifactSummary_source :: Lens.Lens' ArtifactSummary (Prelude.Maybe ArtifactSource)
artifactSummary_source = Lens.lens (\ArtifactSummary' {source} -> source) (\s@ArtifactSummary' {} a -> s {source = a} :: ArtifactSummary)

-- | The Amazon Resource Name (ARN) of the artifact.
artifactSummary_artifactArn :: Lens.Lens' ArtifactSummary (Prelude.Maybe Prelude.Text)
artifactSummary_artifactArn = Lens.lens (\ArtifactSummary' {artifactArn} -> artifactArn) (\s@ArtifactSummary' {} a -> s {artifactArn = a} :: ArtifactSummary)

-- | The type of the artifact.
artifactSummary_artifactType :: Lens.Lens' ArtifactSummary (Prelude.Maybe Prelude.Text)
artifactSummary_artifactType = Lens.lens (\ArtifactSummary' {artifactType} -> artifactType) (\s@ArtifactSummary' {} a -> s {artifactType = a} :: ArtifactSummary)

instance Core.FromJSON ArtifactSummary where
  parseJSON =
    Core.withObject
      "ArtifactSummary"
      ( \x ->
          ArtifactSummary'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "ArtifactName")
            Prelude.<*> (x Core..:? "Source")
            Prelude.<*> (x Core..:? "ArtifactArn")
            Prelude.<*> (x Core..:? "ArtifactType")
      )

instance Prelude.Hashable ArtifactSummary

instance Prelude.NFData ArtifactSummary
