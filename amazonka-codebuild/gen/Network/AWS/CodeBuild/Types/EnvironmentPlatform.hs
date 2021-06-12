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
-- Module      : Network.AWS.CodeBuild.Types.EnvironmentPlatform
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.EnvironmentPlatform where

import Network.AWS.CodeBuild.Types.EnvironmentLanguage
import Network.AWS.CodeBuild.Types.PlatformType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A set of Docker images that are related by platform and are managed by
-- AWS CodeBuild.
--
-- /See:/ 'newEnvironmentPlatform' smart constructor.
data EnvironmentPlatform = EnvironmentPlatform'
  { -- | The platform\'s name.
    platform :: Core.Maybe PlatformType,
    -- | The list of programming languages that are available for the specified
    -- platform.
    languages :: Core.Maybe [EnvironmentLanguage]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { platform = Core.Nothing,
      languages = Core.Nothing
    }

-- | The platform\'s name.
environmentPlatform_platform :: Lens.Lens' EnvironmentPlatform (Core.Maybe PlatformType)
environmentPlatform_platform = Lens.lens (\EnvironmentPlatform' {platform} -> platform) (\s@EnvironmentPlatform' {} a -> s {platform = a} :: EnvironmentPlatform)

-- | The list of programming languages that are available for the specified
-- platform.
environmentPlatform_languages :: Lens.Lens' EnvironmentPlatform (Core.Maybe [EnvironmentLanguage])
environmentPlatform_languages = Lens.lens (\EnvironmentPlatform' {languages} -> languages) (\s@EnvironmentPlatform' {} a -> s {languages = a} :: EnvironmentPlatform) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON EnvironmentPlatform where
  parseJSON =
    Core.withObject
      "EnvironmentPlatform"
      ( \x ->
          EnvironmentPlatform'
            Core.<$> (x Core..:? "platform")
            Core.<*> (x Core..:? "languages" Core..!= Core.mempty)
      )

instance Core.Hashable EnvironmentPlatform

instance Core.NFData EnvironmentPlatform
