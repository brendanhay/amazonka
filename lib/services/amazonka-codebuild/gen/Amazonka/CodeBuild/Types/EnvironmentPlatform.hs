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
-- Module      : Amazonka.CodeBuild.Types.EnvironmentPlatform
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.EnvironmentPlatform where

import Amazonka.CodeBuild.Types.EnvironmentLanguage
import Amazonka.CodeBuild.Types.PlatformType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A set of Docker images that are related by platform and are managed by
-- CodeBuild.
--
-- /See:/ 'newEnvironmentPlatform' smart constructor.
data EnvironmentPlatform = EnvironmentPlatform'
  { -- | The list of programming languages that are available for the specified
    -- platform.
    languages :: Prelude.Maybe [EnvironmentLanguage],
    -- | The platform\'s name.
    platform :: Prelude.Maybe PlatformType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentPlatform' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languages', 'environmentPlatform_languages' - The list of programming languages that are available for the specified
-- platform.
--
-- 'platform', 'environmentPlatform_platform' - The platform\'s name.
newEnvironmentPlatform ::
  EnvironmentPlatform
newEnvironmentPlatform =
  EnvironmentPlatform'
    { languages = Prelude.Nothing,
      platform = Prelude.Nothing
    }

-- | The list of programming languages that are available for the specified
-- platform.
environmentPlatform_languages :: Lens.Lens' EnvironmentPlatform (Prelude.Maybe [EnvironmentLanguage])
environmentPlatform_languages = Lens.lens (\EnvironmentPlatform' {languages} -> languages) (\s@EnvironmentPlatform' {} a -> s {languages = a} :: EnvironmentPlatform) Prelude.. Lens.mapping Lens.coerced

-- | The platform\'s name.
environmentPlatform_platform :: Lens.Lens' EnvironmentPlatform (Prelude.Maybe PlatformType)
environmentPlatform_platform = Lens.lens (\EnvironmentPlatform' {platform} -> platform) (\s@EnvironmentPlatform' {} a -> s {platform = a} :: EnvironmentPlatform)

instance Data.FromJSON EnvironmentPlatform where
  parseJSON =
    Data.withObject
      "EnvironmentPlatform"
      ( \x ->
          EnvironmentPlatform'
            Prelude.<$> (x Data..:? "languages" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "platform")
      )

instance Prelude.Hashable EnvironmentPlatform where
  hashWithSalt _salt EnvironmentPlatform' {..} =
    _salt
      `Prelude.hashWithSalt` languages
      `Prelude.hashWithSalt` platform

instance Prelude.NFData EnvironmentPlatform where
  rnf EnvironmentPlatform' {..} =
    Prelude.rnf languages
      `Prelude.seq` Prelude.rnf platform
