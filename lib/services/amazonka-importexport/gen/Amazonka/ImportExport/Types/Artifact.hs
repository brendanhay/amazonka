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
-- Module      : Amazonka.ImportExport.Types.Artifact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImportExport.Types.Artifact where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A discrete item that contains the description and URL of an artifact
-- (such as a PDF).
--
-- /See:/ 'newArtifact' smart constructor.
data Artifact = Artifact'
  { description :: Prelude.Maybe Prelude.Text,
    url :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { description = Prelude.Nothing,
      url = Prelude.Nothing
    }

-- | Undocumented member.
artifact_description :: Lens.Lens' Artifact (Prelude.Maybe Prelude.Text)
artifact_description = Lens.lens (\Artifact' {description} -> description) (\s@Artifact' {} a -> s {description = a} :: Artifact)

-- | Undocumented member.
artifact_url :: Lens.Lens' Artifact (Prelude.Maybe Prelude.Text)
artifact_url = Lens.lens (\Artifact' {url} -> url) (\s@Artifact' {} a -> s {url = a} :: Artifact)

instance Data.FromXML Artifact where
  parseXML x =
    Artifact'
      Prelude.<$> (x Data..@? "Description")
      Prelude.<*> (x Data..@? "URL")

instance Prelude.Hashable Artifact where
  hashWithSalt _salt Artifact' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` url

instance Prelude.NFData Artifact where
  rnf Artifact' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf url
