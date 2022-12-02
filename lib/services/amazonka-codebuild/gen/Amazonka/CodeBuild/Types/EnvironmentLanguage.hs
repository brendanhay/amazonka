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
-- Module      : Amazonka.CodeBuild.Types.EnvironmentLanguage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.EnvironmentLanguage where

import Amazonka.CodeBuild.Types.EnvironmentImage
import Amazonka.CodeBuild.Types.LanguageType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A set of Docker images that are related by programming language and are
-- managed by CodeBuild.
--
-- /See:/ 'newEnvironmentLanguage' smart constructor.
data EnvironmentLanguage = EnvironmentLanguage'
  { -- | The programming language for the Docker images.
    language :: Prelude.Maybe LanguageType,
    -- | The list of Docker images that are related by the specified programming
    -- language.
    images :: Prelude.Maybe [EnvironmentImage]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentLanguage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'language', 'environmentLanguage_language' - The programming language for the Docker images.
--
-- 'images', 'environmentLanguage_images' - The list of Docker images that are related by the specified programming
-- language.
newEnvironmentLanguage ::
  EnvironmentLanguage
newEnvironmentLanguage =
  EnvironmentLanguage'
    { language = Prelude.Nothing,
      images = Prelude.Nothing
    }

-- | The programming language for the Docker images.
environmentLanguage_language :: Lens.Lens' EnvironmentLanguage (Prelude.Maybe LanguageType)
environmentLanguage_language = Lens.lens (\EnvironmentLanguage' {language} -> language) (\s@EnvironmentLanguage' {} a -> s {language = a} :: EnvironmentLanguage)

-- | The list of Docker images that are related by the specified programming
-- language.
environmentLanguage_images :: Lens.Lens' EnvironmentLanguage (Prelude.Maybe [EnvironmentImage])
environmentLanguage_images = Lens.lens (\EnvironmentLanguage' {images} -> images) (\s@EnvironmentLanguage' {} a -> s {images = a} :: EnvironmentLanguage) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON EnvironmentLanguage where
  parseJSON =
    Data.withObject
      "EnvironmentLanguage"
      ( \x ->
          EnvironmentLanguage'
            Prelude.<$> (x Data..:? "language")
            Prelude.<*> (x Data..:? "images" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable EnvironmentLanguage where
  hashWithSalt _salt EnvironmentLanguage' {..} =
    _salt `Prelude.hashWithSalt` language
      `Prelude.hashWithSalt` images

instance Prelude.NFData EnvironmentLanguage where
  rnf EnvironmentLanguage' {..} =
    Prelude.rnf language
      `Prelude.seq` Prelude.rnf images
