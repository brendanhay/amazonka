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
-- Module      : Network.AWS.CodeBuild.Types.EnvironmentLanguage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.EnvironmentLanguage where

import Network.AWS.CodeBuild.Types.EnvironmentImage
import Network.AWS.CodeBuild.Types.LanguageType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A set of Docker images that are related by programming language and are
-- managed by AWS CodeBuild.
--
-- /See:/ 'newEnvironmentLanguage' smart constructor.
data EnvironmentLanguage = EnvironmentLanguage'
  { -- | The list of Docker images that are related by the specified programming
    -- language.
    images :: Core.Maybe [EnvironmentImage],
    -- | The programming language for the Docker images.
    language :: Core.Maybe LanguageType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnvironmentLanguage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'images', 'environmentLanguage_images' - The list of Docker images that are related by the specified programming
-- language.
--
-- 'language', 'environmentLanguage_language' - The programming language for the Docker images.
newEnvironmentLanguage ::
  EnvironmentLanguage
newEnvironmentLanguage =
  EnvironmentLanguage'
    { images = Core.Nothing,
      language = Core.Nothing
    }

-- | The list of Docker images that are related by the specified programming
-- language.
environmentLanguage_images :: Lens.Lens' EnvironmentLanguage (Core.Maybe [EnvironmentImage])
environmentLanguage_images = Lens.lens (\EnvironmentLanguage' {images} -> images) (\s@EnvironmentLanguage' {} a -> s {images = a} :: EnvironmentLanguage) Core.. Lens.mapping Lens._Coerce

-- | The programming language for the Docker images.
environmentLanguage_language :: Lens.Lens' EnvironmentLanguage (Core.Maybe LanguageType)
environmentLanguage_language = Lens.lens (\EnvironmentLanguage' {language} -> language) (\s@EnvironmentLanguage' {} a -> s {language = a} :: EnvironmentLanguage)

instance Core.FromJSON EnvironmentLanguage where
  parseJSON =
    Core.withObject
      "EnvironmentLanguage"
      ( \x ->
          EnvironmentLanguage'
            Core.<$> (x Core..:? "images" Core..!= Core.mempty)
            Core.<*> (x Core..:? "language")
      )

instance Core.Hashable EnvironmentLanguage

instance Core.NFData EnvironmentLanguage
