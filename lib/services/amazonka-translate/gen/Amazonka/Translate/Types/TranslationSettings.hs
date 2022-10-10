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
-- Module      : Amazonka.Translate.Types.TranslationSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Translate.Types.TranslationSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Translate.Types.Formality
import Amazonka.Translate.Types.Profanity

-- | Settings that configure the translation output.
--
-- /See:/ 'newTranslationSettings' smart constructor.
data TranslationSettings = TranslationSettings'
  { -- | You can optionally specify the desired level of formality for real-time
    -- translations to supported target languages. The formality setting
    -- controls the level of formal language usage (also known as
    -- <https://en.wikipedia.org/wiki/Register_(sociolinguistics) register>) in
    -- the translation output. You can set the value to informal or formal. If
    -- you don\'t specify a value for formality, or if the target language
    -- doesn\'t support formality, the translation will ignore the formality
    -- setting.
    --
    -- Note that asynchronous translation jobs don\'t support formality. If you
    -- provide a value for formality, the @StartTextTranslationJob@ API throws
    -- an exception (InvalidRequestException).
    --
    -- For target languages that support formality, see
    -- <https://docs.aws.amazon.com/translate/latest/dg/what-is.html Supported Languages and Language Codes in the Amazon Translate Developer Guide>.
    formality :: Prelude.Maybe Formality,
    -- | Enable the profanity setting if you want Amazon Translate to mask
    -- profane words and phrases in your translation output.
    --
    -- To mask profane words and phrases, Amazon Translate replaces them with
    -- the grawlix string “?$#\@$“. This 5-character sequence is used for each
    -- profane word or phrase, regardless of the length or number of words.
    --
    -- Amazon Translate doesn\'t detect profanity in all of its supported
    -- languages. For languages that support profanity detection, see
    -- <https://docs.aws.amazon.com/translate/latest/dg/what-is.html Supported Languages and Language Codes in the Amazon Translate Developer Guide>.
    profanity :: Prelude.Maybe Profanity
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TranslationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'formality', 'translationSettings_formality' - You can optionally specify the desired level of formality for real-time
-- translations to supported target languages. The formality setting
-- controls the level of formal language usage (also known as
-- <https://en.wikipedia.org/wiki/Register_(sociolinguistics) register>) in
-- the translation output. You can set the value to informal or formal. If
-- you don\'t specify a value for formality, or if the target language
-- doesn\'t support formality, the translation will ignore the formality
-- setting.
--
-- Note that asynchronous translation jobs don\'t support formality. If you
-- provide a value for formality, the @StartTextTranslationJob@ API throws
-- an exception (InvalidRequestException).
--
-- For target languages that support formality, see
-- <https://docs.aws.amazon.com/translate/latest/dg/what-is.html Supported Languages and Language Codes in the Amazon Translate Developer Guide>.
--
-- 'profanity', 'translationSettings_profanity' - Enable the profanity setting if you want Amazon Translate to mask
-- profane words and phrases in your translation output.
--
-- To mask profane words and phrases, Amazon Translate replaces them with
-- the grawlix string “?$#\@$“. This 5-character sequence is used for each
-- profane word or phrase, regardless of the length or number of words.
--
-- Amazon Translate doesn\'t detect profanity in all of its supported
-- languages. For languages that support profanity detection, see
-- <https://docs.aws.amazon.com/translate/latest/dg/what-is.html Supported Languages and Language Codes in the Amazon Translate Developer Guide>.
newTranslationSettings ::
  TranslationSettings
newTranslationSettings =
  TranslationSettings'
    { formality = Prelude.Nothing,
      profanity = Prelude.Nothing
    }

-- | You can optionally specify the desired level of formality for real-time
-- translations to supported target languages. The formality setting
-- controls the level of formal language usage (also known as
-- <https://en.wikipedia.org/wiki/Register_(sociolinguistics) register>) in
-- the translation output. You can set the value to informal or formal. If
-- you don\'t specify a value for formality, or if the target language
-- doesn\'t support formality, the translation will ignore the formality
-- setting.
--
-- Note that asynchronous translation jobs don\'t support formality. If you
-- provide a value for formality, the @StartTextTranslationJob@ API throws
-- an exception (InvalidRequestException).
--
-- For target languages that support formality, see
-- <https://docs.aws.amazon.com/translate/latest/dg/what-is.html Supported Languages and Language Codes in the Amazon Translate Developer Guide>.
translationSettings_formality :: Lens.Lens' TranslationSettings (Prelude.Maybe Formality)
translationSettings_formality = Lens.lens (\TranslationSettings' {formality} -> formality) (\s@TranslationSettings' {} a -> s {formality = a} :: TranslationSettings)

-- | Enable the profanity setting if you want Amazon Translate to mask
-- profane words and phrases in your translation output.
--
-- To mask profane words and phrases, Amazon Translate replaces them with
-- the grawlix string “?$#\@$“. This 5-character sequence is used for each
-- profane word or phrase, regardless of the length or number of words.
--
-- Amazon Translate doesn\'t detect profanity in all of its supported
-- languages. For languages that support profanity detection, see
-- <https://docs.aws.amazon.com/translate/latest/dg/what-is.html Supported Languages and Language Codes in the Amazon Translate Developer Guide>.
translationSettings_profanity :: Lens.Lens' TranslationSettings (Prelude.Maybe Profanity)
translationSettings_profanity = Lens.lens (\TranslationSettings' {profanity} -> profanity) (\s@TranslationSettings' {} a -> s {profanity = a} :: TranslationSettings)

instance Core.FromJSON TranslationSettings where
  parseJSON =
    Core.withObject
      "TranslationSettings"
      ( \x ->
          TranslationSettings'
            Prelude.<$> (x Core..:? "Formality")
            Prelude.<*> (x Core..:? "Profanity")
      )

instance Prelude.Hashable TranslationSettings where
  hashWithSalt _salt TranslationSettings' {..} =
    _salt `Prelude.hashWithSalt` formality
      `Prelude.hashWithSalt` profanity

instance Prelude.NFData TranslationSettings where
  rnf TranslationSettings' {..} =
    Prelude.rnf formality
      `Prelude.seq` Prelude.rnf profanity

instance Core.ToJSON TranslationSettings where
  toJSON TranslationSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Formality" Core..=) Prelude.<$> formality,
            ("Profanity" Core..=) Prelude.<$> profanity
          ]
      )
