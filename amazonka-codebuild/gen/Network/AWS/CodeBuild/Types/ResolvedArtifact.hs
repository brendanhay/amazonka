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
-- Module      : Network.AWS.CodeBuild.Types.ResolvedArtifact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ResolvedArtifact where

import Network.AWS.CodeBuild.Types.ArtifactsType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents a resolved build artifact. A resolve artifact is an artifact
-- that is built and deployed to the destination, such as Amazon S3.
--
-- /See:/ 'newResolvedArtifact' smart constructor.
data ResolvedArtifact = ResolvedArtifact'
  { -- | The identifier of the artifact.
    identifier :: Core.Maybe Core.Text,
    -- | Specifies the type of artifact.
    type' :: Core.Maybe ArtifactsType,
    -- | The location of the artifact.
    location :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ResolvedArtifact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifier', 'resolvedArtifact_identifier' - The identifier of the artifact.
--
-- 'type'', 'resolvedArtifact_type' - Specifies the type of artifact.
--
-- 'location', 'resolvedArtifact_location' - The location of the artifact.
newResolvedArtifact ::
  ResolvedArtifact
newResolvedArtifact =
  ResolvedArtifact'
    { identifier = Core.Nothing,
      type' = Core.Nothing,
      location = Core.Nothing
    }

-- | The identifier of the artifact.
resolvedArtifact_identifier :: Lens.Lens' ResolvedArtifact (Core.Maybe Core.Text)
resolvedArtifact_identifier = Lens.lens (\ResolvedArtifact' {identifier} -> identifier) (\s@ResolvedArtifact' {} a -> s {identifier = a} :: ResolvedArtifact)

-- | Specifies the type of artifact.
resolvedArtifact_type :: Lens.Lens' ResolvedArtifact (Core.Maybe ArtifactsType)
resolvedArtifact_type = Lens.lens (\ResolvedArtifact' {type'} -> type') (\s@ResolvedArtifact' {} a -> s {type' = a} :: ResolvedArtifact)

-- | The location of the artifact.
resolvedArtifact_location :: Lens.Lens' ResolvedArtifact (Core.Maybe Core.Text)
resolvedArtifact_location = Lens.lens (\ResolvedArtifact' {location} -> location) (\s@ResolvedArtifact' {} a -> s {location = a} :: ResolvedArtifact)

instance Core.FromJSON ResolvedArtifact where
  parseJSON =
    Core.withObject
      "ResolvedArtifact"
      ( \x ->
          ResolvedArtifact'
            Core.<$> (x Core..:? "identifier")
            Core.<*> (x Core..:? "type")
            Core.<*> (x Core..:? "location")
      )

instance Core.Hashable ResolvedArtifact

instance Core.NFData ResolvedArtifact
