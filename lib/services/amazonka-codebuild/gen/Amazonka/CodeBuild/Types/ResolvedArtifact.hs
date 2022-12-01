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
-- Module      : Amazonka.CodeBuild.Types.ResolvedArtifact
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.ResolvedArtifact where

import Amazonka.CodeBuild.Types.ArtifactsType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents a resolved build artifact. A resolved artifact is an artifact
-- that is built and deployed to the destination, such as Amazon S3.
--
-- /See:/ 'newResolvedArtifact' smart constructor.
data ResolvedArtifact = ResolvedArtifact'
  { -- | Specifies the type of artifact.
    type' :: Prelude.Maybe ArtifactsType,
    -- | The location of the artifact.
    location :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the artifact.
    identifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResolvedArtifact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'resolvedArtifact_type' - Specifies the type of artifact.
--
-- 'location', 'resolvedArtifact_location' - The location of the artifact.
--
-- 'identifier', 'resolvedArtifact_identifier' - The identifier of the artifact.
newResolvedArtifact ::
  ResolvedArtifact
newResolvedArtifact =
  ResolvedArtifact'
    { type' = Prelude.Nothing,
      location = Prelude.Nothing,
      identifier = Prelude.Nothing
    }

-- | Specifies the type of artifact.
resolvedArtifact_type :: Lens.Lens' ResolvedArtifact (Prelude.Maybe ArtifactsType)
resolvedArtifact_type = Lens.lens (\ResolvedArtifact' {type'} -> type') (\s@ResolvedArtifact' {} a -> s {type' = a} :: ResolvedArtifact)

-- | The location of the artifact.
resolvedArtifact_location :: Lens.Lens' ResolvedArtifact (Prelude.Maybe Prelude.Text)
resolvedArtifact_location = Lens.lens (\ResolvedArtifact' {location} -> location) (\s@ResolvedArtifact' {} a -> s {location = a} :: ResolvedArtifact)

-- | The identifier of the artifact.
resolvedArtifact_identifier :: Lens.Lens' ResolvedArtifact (Prelude.Maybe Prelude.Text)
resolvedArtifact_identifier = Lens.lens (\ResolvedArtifact' {identifier} -> identifier) (\s@ResolvedArtifact' {} a -> s {identifier = a} :: ResolvedArtifact)

instance Core.FromJSON ResolvedArtifact where
  parseJSON =
    Core.withObject
      "ResolvedArtifact"
      ( \x ->
          ResolvedArtifact'
            Prelude.<$> (x Core..:? "type")
            Prelude.<*> (x Core..:? "location")
            Prelude.<*> (x Core..:? "identifier")
      )

instance Prelude.Hashable ResolvedArtifact where
  hashWithSalt _salt ResolvedArtifact' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` identifier

instance Prelude.NFData ResolvedArtifact where
  rnf ResolvedArtifact' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf identifier
