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
-- Module      : Amazonka.GamesParks.Types.Generator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GamesParks.Types.Generator where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Properties that specify the code generator for a generated code job.
--
-- /See:/ 'newGenerator' smart constructor.
data Generator = Generator'
  { -- | The target version of the GameSparks Game SDK.
    gameSdkVersion :: Prelude.Maybe Prelude.Text,
    -- | The programming language for the generated code.
    --
    -- Not all languages are supported for each platform. For cases where
    -- multiple languages are supported, this parameter specifies the language
    -- to be used. If this value is omitted, the default language for the
    -- target platform will be used.
    language :: Prelude.Maybe Prelude.Text,
    -- | The platform that will be used to run the generated code.
    targetPlatform :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Generator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameSdkVersion', 'generator_gameSdkVersion' - The target version of the GameSparks Game SDK.
--
-- 'language', 'generator_language' - The programming language for the generated code.
--
-- Not all languages are supported for each platform. For cases where
-- multiple languages are supported, this parameter specifies the language
-- to be used. If this value is omitted, the default language for the
-- target platform will be used.
--
-- 'targetPlatform', 'generator_targetPlatform' - The platform that will be used to run the generated code.
newGenerator ::
  Generator
newGenerator =
  Generator'
    { gameSdkVersion = Prelude.Nothing,
      language = Prelude.Nothing,
      targetPlatform = Prelude.Nothing
    }

-- | The target version of the GameSparks Game SDK.
generator_gameSdkVersion :: Lens.Lens' Generator (Prelude.Maybe Prelude.Text)
generator_gameSdkVersion = Lens.lens (\Generator' {gameSdkVersion} -> gameSdkVersion) (\s@Generator' {} a -> s {gameSdkVersion = a} :: Generator)

-- | The programming language for the generated code.
--
-- Not all languages are supported for each platform. For cases where
-- multiple languages are supported, this parameter specifies the language
-- to be used. If this value is omitted, the default language for the
-- target platform will be used.
generator_language :: Lens.Lens' Generator (Prelude.Maybe Prelude.Text)
generator_language = Lens.lens (\Generator' {language} -> language) (\s@Generator' {} a -> s {language = a} :: Generator)

-- | The platform that will be used to run the generated code.
generator_targetPlatform :: Lens.Lens' Generator (Prelude.Maybe Prelude.Text)
generator_targetPlatform = Lens.lens (\Generator' {targetPlatform} -> targetPlatform) (\s@Generator' {} a -> s {targetPlatform = a} :: Generator)

instance Prelude.Hashable Generator where
  hashWithSalt _salt Generator' {..} =
    _salt `Prelude.hashWithSalt` gameSdkVersion
      `Prelude.hashWithSalt` language
      `Prelude.hashWithSalt` targetPlatform

instance Prelude.NFData Generator where
  rnf Generator' {..} =
    Prelude.rnf gameSdkVersion
      `Prelude.seq` Prelude.rnf language
      `Prelude.seq` Prelude.rnf targetPlatform

instance Data.ToJSON Generator where
  toJSON Generator' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GameSdkVersion" Data..=)
              Prelude.<$> gameSdkVersion,
            ("Language" Data..=) Prelude.<$> language,
            ("TargetPlatform" Data..=)
              Prelude.<$> targetPlatform
          ]
      )
