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
-- Module      : Network.AWS.Transcribe.Types.ModelSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.ModelSettings where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The object used to call your custom language model to your transcription
-- job.
--
-- /See:/ 'newModelSettings' smart constructor.
data ModelSettings = ModelSettings'
  { -- | The name of your custom language model.
    languageModelName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  ModelSettings' {languageModelName = Prelude.Nothing}

-- | The name of your custom language model.
modelSettings_languageModelName :: Lens.Lens' ModelSettings (Prelude.Maybe Prelude.Text)
modelSettings_languageModelName = Lens.lens (\ModelSettings' {languageModelName} -> languageModelName) (\s@ModelSettings' {} a -> s {languageModelName = a} :: ModelSettings)

instance Prelude.FromJSON ModelSettings where
  parseJSON =
    Prelude.withObject
      "ModelSettings"
      ( \x ->
          ModelSettings'
            Prelude.<$> (x Prelude..:? "LanguageModelName")
      )

instance Prelude.Hashable ModelSettings

instance Prelude.NFData ModelSettings

instance Prelude.ToJSON ModelSettings where
  toJSON ModelSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("LanguageModelName" Prelude..=)
              Prelude.<$> languageModelName
          ]
      )
