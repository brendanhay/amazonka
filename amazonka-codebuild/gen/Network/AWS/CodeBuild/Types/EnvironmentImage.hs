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
-- Module      : Network.AWS.CodeBuild.Types.EnvironmentImage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.EnvironmentImage where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a Docker image that is managed by AWS CodeBuild.
--
-- /See:/ 'newEnvironmentImage' smart constructor.
data EnvironmentImage = EnvironmentImage'
  { -- | A list of environment image versions.
    versions :: Prelude.Maybe [Prelude.Text],
    -- | The name of the Docker image.
    name :: Prelude.Maybe Prelude.Text,
    -- | The description of the Docker image.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versions', 'environmentImage_versions' - A list of environment image versions.
--
-- 'name', 'environmentImage_name' - The name of the Docker image.
--
-- 'description', 'environmentImage_description' - The description of the Docker image.
newEnvironmentImage ::
  EnvironmentImage
newEnvironmentImage =
  EnvironmentImage'
    { versions = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | A list of environment image versions.
environmentImage_versions :: Lens.Lens' EnvironmentImage (Prelude.Maybe [Prelude.Text])
environmentImage_versions = Lens.lens (\EnvironmentImage' {versions} -> versions) (\s@EnvironmentImage' {} a -> s {versions = a} :: EnvironmentImage) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the Docker image.
environmentImage_name :: Lens.Lens' EnvironmentImage (Prelude.Maybe Prelude.Text)
environmentImage_name = Lens.lens (\EnvironmentImage' {name} -> name) (\s@EnvironmentImage' {} a -> s {name = a} :: EnvironmentImage)

-- | The description of the Docker image.
environmentImage_description :: Lens.Lens' EnvironmentImage (Prelude.Maybe Prelude.Text)
environmentImage_description = Lens.lens (\EnvironmentImage' {description} -> description) (\s@EnvironmentImage' {} a -> s {description = a} :: EnvironmentImage)

instance Prelude.FromJSON EnvironmentImage where
  parseJSON =
    Prelude.withObject
      "EnvironmentImage"
      ( \x ->
          EnvironmentImage'
            Prelude.<$> (x Prelude..:? "versions" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "description")
      )

instance Prelude.Hashable EnvironmentImage

instance Prelude.NFData EnvironmentImage
