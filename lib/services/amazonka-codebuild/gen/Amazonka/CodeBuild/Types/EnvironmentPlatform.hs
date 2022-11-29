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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.EnvironmentPlatform where

import Amazonka.CodeBuild.Types.EnvironmentLanguage
import Amazonka.CodeBuild.Types.PlatformType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A set of Docker images that are related by platform and are managed by
-- CodeBuild.
--
-- /See:/ 'newEnvironmentPlatform' smart constructor.
data EnvironmentPlatform = EnvironmentPlatform'
  { -- | The platform\'s name.
    platform :: Prelude.Maybe PlatformType,
    -- | The list of programming languages that are available for the specified
    -- platform.
    languages :: Prelude.Maybe [EnvironmentLanguage]
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
-- 'platform', 'environmentPlatform_platform' - The platform\'s name.
--
-- 'languages', 'environmentPlatform_languages' - The list of programming languages that are available for the specified
-- platform.
newEnvironmentPlatform ::
  EnvironmentPlatform
newEnvironmentPlatform =
  EnvironmentPlatform'
    { platform = Prelude.Nothing,
      languages = Prelude.Nothing
    }

-- | The platform\'s name.
environmentPlatform_platform :: Lens.Lens' EnvironmentPlatform (Prelude.Maybe PlatformType)
environmentPlatform_platform = Lens.lens (\EnvironmentPlatform' {platform} -> platform) (\s@EnvironmentPlatform' {} a -> s {platform = a} :: EnvironmentPlatform)

-- | The list of programming languages that are available for the specified
-- platform.
environmentPlatform_languages :: Lens.Lens' EnvironmentPlatform (Prelude.Maybe [EnvironmentLanguage])
environmentPlatform_languages = Lens.lens (\EnvironmentPlatform' {languages} -> languages) (\s@EnvironmentPlatform' {} a -> s {languages = a} :: EnvironmentPlatform) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON EnvironmentPlatform where
  parseJSON =
    Core.withObject
      "EnvironmentPlatform"
      ( \x ->
          EnvironmentPlatform'
            Prelude.<$> (x Core..:? "platform")
            Prelude.<*> (x Core..:? "languages" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable EnvironmentPlatform where
  hashWithSalt _salt EnvironmentPlatform' {..} =
    _salt `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` languages

instance Prelude.NFData EnvironmentPlatform where
  rnf EnvironmentPlatform' {..} =
    Prelude.rnf platform
      `Prelude.seq` Prelude.rnf languages
