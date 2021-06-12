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
-- Module      : Network.AWS.Transcribe.Types.ModelSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.ModelSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The object used to call your custom language model to your transcription
-- job.
--
-- /See:/ 'newModelSettings' smart constructor.
data ModelSettings = ModelSettings'
  { -- | The name of your custom language model.
    languageModelName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModelSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageModelName', 'modelSettings_languageModelName' - The name of your custom language model.
newModelSettings ::
  ModelSettings
newModelSettings =
  ModelSettings' {languageModelName = Core.Nothing}

-- | The name of your custom language model.
modelSettings_languageModelName :: Lens.Lens' ModelSettings (Core.Maybe Core.Text)
modelSettings_languageModelName = Lens.lens (\ModelSettings' {languageModelName} -> languageModelName) (\s@ModelSettings' {} a -> s {languageModelName = a} :: ModelSettings)

instance Core.FromJSON ModelSettings where
  parseJSON =
    Core.withObject
      "ModelSettings"
      ( \x ->
          ModelSettings'
            Core.<$> (x Core..:? "LanguageModelName")
      )

instance Core.Hashable ModelSettings

instance Core.NFData ModelSettings

instance Core.ToJSON ModelSettings where
  toJSON ModelSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("LanguageModelName" Core..=)
              Core.<$> languageModelName
          ]
      )
