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
-- Module      : Network.AWS.ImportExport.Types.Artifact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ImportExport.Types.Artifact where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A discrete item that contains the description and URL of an artifact
-- (such as a PDF).
--
-- /See:/ 'newArtifact' smart constructor.
data Artifact = Artifact'
  { description :: Core.Maybe Core.Text,
    url :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Artifact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'artifact_description' - Undocumented member.
--
-- 'url', 'artifact_url' - Undocumented member.
newArtifact ::
  Artifact
newArtifact =
  Artifact'
    { description = Core.Nothing,
      url = Core.Nothing
    }

-- | Undocumented member.
artifact_description :: Lens.Lens' Artifact (Core.Maybe Core.Text)
artifact_description = Lens.lens (\Artifact' {description} -> description) (\s@Artifact' {} a -> s {description = a} :: Artifact)

-- | Undocumented member.
artifact_url :: Lens.Lens' Artifact (Core.Maybe Core.Text)
artifact_url = Lens.lens (\Artifact' {url} -> url) (\s@Artifact' {} a -> s {url = a} :: Artifact)

instance Core.FromXML Artifact where
  parseXML x =
    Artifact'
      Core.<$> (x Core..@? "Description")
      Core.<*> (x Core..@? "URL")

instance Core.Hashable Artifact

instance Core.NFData Artifact
