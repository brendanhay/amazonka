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
-- Module      : Amazonka.CodeBuild.Types.EnvironmentImage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.EnvironmentImage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a Docker image that is managed by CodeBuild.
--
-- /See:/ 'newEnvironmentImage' smart constructor.
data EnvironmentImage = EnvironmentImage'
  { -- | The description of the Docker image.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the Docker image.
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of environment image versions.
    versions :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'environmentImage_description' - The description of the Docker image.
--
-- 'name', 'environmentImage_name' - The name of the Docker image.
--
-- 'versions', 'environmentImage_versions' - A list of environment image versions.
newEnvironmentImage ::
  EnvironmentImage
newEnvironmentImage =
  EnvironmentImage'
    { description = Prelude.Nothing,
      name = Prelude.Nothing,
      versions = Prelude.Nothing
    }

-- | The description of the Docker image.
environmentImage_description :: Lens.Lens' EnvironmentImage (Prelude.Maybe Prelude.Text)
environmentImage_description = Lens.lens (\EnvironmentImage' {description} -> description) (\s@EnvironmentImage' {} a -> s {description = a} :: EnvironmentImage)

-- | The name of the Docker image.
environmentImage_name :: Lens.Lens' EnvironmentImage (Prelude.Maybe Prelude.Text)
environmentImage_name = Lens.lens (\EnvironmentImage' {name} -> name) (\s@EnvironmentImage' {} a -> s {name = a} :: EnvironmentImage)

-- | A list of environment image versions.
environmentImage_versions :: Lens.Lens' EnvironmentImage (Prelude.Maybe [Prelude.Text])
environmentImage_versions = Lens.lens (\EnvironmentImage' {versions} -> versions) (\s@EnvironmentImage' {} a -> s {versions = a} :: EnvironmentImage) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON EnvironmentImage where
  parseJSON =
    Data.withObject
      "EnvironmentImage"
      ( \x ->
          EnvironmentImage'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "versions" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable EnvironmentImage where
  hashWithSalt _salt EnvironmentImage' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` versions

instance Prelude.NFData EnvironmentImage where
  rnf EnvironmentImage' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf versions
