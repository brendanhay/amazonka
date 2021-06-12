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
-- Module      : Network.AWS.ElasticBeanstalk.Types.PlatformProgrammingLanguage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.PlatformProgrammingLanguage where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A programming language supported by the platform.
--
-- /See:/ 'newPlatformProgrammingLanguage' smart constructor.
data PlatformProgrammingLanguage = PlatformProgrammingLanguage'
  { -- | The version of the programming language.
    version :: Core.Maybe Core.Text,
    -- | The name of the programming language.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PlatformProgrammingLanguage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'platformProgrammingLanguage_version' - The version of the programming language.
--
-- 'name', 'platformProgrammingLanguage_name' - The name of the programming language.
newPlatformProgrammingLanguage ::
  PlatformProgrammingLanguage
newPlatformProgrammingLanguage =
  PlatformProgrammingLanguage'
    { version =
        Core.Nothing,
      name = Core.Nothing
    }

-- | The version of the programming language.
platformProgrammingLanguage_version :: Lens.Lens' PlatformProgrammingLanguage (Core.Maybe Core.Text)
platformProgrammingLanguage_version = Lens.lens (\PlatformProgrammingLanguage' {version} -> version) (\s@PlatformProgrammingLanguage' {} a -> s {version = a} :: PlatformProgrammingLanguage)

-- | The name of the programming language.
platformProgrammingLanguage_name :: Lens.Lens' PlatformProgrammingLanguage (Core.Maybe Core.Text)
platformProgrammingLanguage_name = Lens.lens (\PlatformProgrammingLanguage' {name} -> name) (\s@PlatformProgrammingLanguage' {} a -> s {name = a} :: PlatformProgrammingLanguage)

instance Core.FromXML PlatformProgrammingLanguage where
  parseXML x =
    PlatformProgrammingLanguage'
      Core.<$> (x Core..@? "Version") Core.<*> (x Core..@? "Name")

instance Core.Hashable PlatformProgrammingLanguage

instance Core.NFData PlatformProgrammingLanguage
