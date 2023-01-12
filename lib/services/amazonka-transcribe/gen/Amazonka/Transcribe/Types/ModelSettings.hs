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
-- Module      : Amazonka.Transcribe.Types.ModelSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.ModelSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the name of the custom language model that was included in the
-- specified transcription job.
--
-- Only use @ModelSettings@ with the @LanguageModelName@ sub-parameter if
-- you\'re __not__ using automatic language identification (@@). If using
-- @LanguageIdSettings@ in your request, this parameter contains a
-- @LanguageModelName@ sub-parameter.
--
-- /See:/ 'newModelSettings' smart constructor.
data ModelSettings = ModelSettings'
  { -- | The name of the custom language model you want to use when processing
    -- your transcription job. Note that custom language model names are case
    -- sensitive.
    --
    -- The language of the specified custom language model must match the
    -- language code that you specify in your transcription request. If the
    -- languages don\'t match, the custom language model isn\'t applied. There
    -- are no errors or warnings associated with a language mismatch.
    languageModelName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageModelName', 'modelSettings_languageModelName' - The name of the custom language model you want to use when processing
-- your transcription job. Note that custom language model names are case
-- sensitive.
--
-- The language of the specified custom language model must match the
-- language code that you specify in your transcription request. If the
-- languages don\'t match, the custom language model isn\'t applied. There
-- are no errors or warnings associated with a language mismatch.
newModelSettings ::
  ModelSettings
newModelSettings =
  ModelSettings' {languageModelName = Prelude.Nothing}

-- | The name of the custom language model you want to use when processing
-- your transcription job. Note that custom language model names are case
-- sensitive.
--
-- The language of the specified custom language model must match the
-- language code that you specify in your transcription request. If the
-- languages don\'t match, the custom language model isn\'t applied. There
-- are no errors or warnings associated with a language mismatch.
modelSettings_languageModelName :: Lens.Lens' ModelSettings (Prelude.Maybe Prelude.Text)
modelSettings_languageModelName = Lens.lens (\ModelSettings' {languageModelName} -> languageModelName) (\s@ModelSettings' {} a -> s {languageModelName = a} :: ModelSettings)

instance Data.FromJSON ModelSettings where
  parseJSON =
    Data.withObject
      "ModelSettings"
      ( \x ->
          ModelSettings'
            Prelude.<$> (x Data..:? "LanguageModelName")
      )

instance Prelude.Hashable ModelSettings where
  hashWithSalt _salt ModelSettings' {..} =
    _salt `Prelude.hashWithSalt` languageModelName

instance Prelude.NFData ModelSettings where
  rnf ModelSettings' {..} =
    Prelude.rnf languageModelName

instance Data.ToJSON ModelSettings where
  toJSON ModelSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LanguageModelName" Data..=)
              Prelude.<$> languageModelName
          ]
      )
