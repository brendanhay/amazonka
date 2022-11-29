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
-- Module      : Amazonka.SageMaker.Types.ClarifyTextConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ClarifyTextConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ClarifyTextGranularity
import Amazonka.SageMaker.Types.ClarifyTextLanguage

-- | A parameter used to configure the SageMaker Clarify explainer to treat
-- text features as text so that explanations are provided for individual
-- units of text. Required only for natural language processing (NLP)
-- explainability.
--
-- /See:/ 'newClarifyTextConfig' smart constructor.
data ClarifyTextConfig = ClarifyTextConfig'
  { -- | Specifies the language of the text features in
    -- <%20https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes ISO 639-1> or
    -- <https://en.wikipedia.org/wiki/ISO_639-3 ISO 639-3> code of a supported
    -- language.
    --
    -- For a mix of multiple languages, use code @\'xx\'@.
    language :: ClarifyTextLanguage,
    -- | The unit of granularity for the analysis of text features. For example,
    -- if the unit is @\'token\'@, then each token (like a word in English) of
    -- the text is treated as a feature. SHAP values are computed for each
    -- unit\/feature.
    granularity :: ClarifyTextGranularity
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClarifyTextConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'language', 'clarifyTextConfig_language' - Specifies the language of the text features in
-- <%20https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes ISO 639-1> or
-- <https://en.wikipedia.org/wiki/ISO_639-3 ISO 639-3> code of a supported
-- language.
--
-- For a mix of multiple languages, use code @\'xx\'@.
--
-- 'granularity', 'clarifyTextConfig_granularity' - The unit of granularity for the analysis of text features. For example,
-- if the unit is @\'token\'@, then each token (like a word in English) of
-- the text is treated as a feature. SHAP values are computed for each
-- unit\/feature.
newClarifyTextConfig ::
  -- | 'language'
  ClarifyTextLanguage ->
  -- | 'granularity'
  ClarifyTextGranularity ->
  ClarifyTextConfig
newClarifyTextConfig pLanguage_ pGranularity_ =
  ClarifyTextConfig'
    { language = pLanguage_,
      granularity = pGranularity_
    }

-- | Specifies the language of the text features in
-- <%20https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes ISO 639-1> or
-- <https://en.wikipedia.org/wiki/ISO_639-3 ISO 639-3> code of a supported
-- language.
--
-- For a mix of multiple languages, use code @\'xx\'@.
clarifyTextConfig_language :: Lens.Lens' ClarifyTextConfig ClarifyTextLanguage
clarifyTextConfig_language = Lens.lens (\ClarifyTextConfig' {language} -> language) (\s@ClarifyTextConfig' {} a -> s {language = a} :: ClarifyTextConfig)

-- | The unit of granularity for the analysis of text features. For example,
-- if the unit is @\'token\'@, then each token (like a word in English) of
-- the text is treated as a feature. SHAP values are computed for each
-- unit\/feature.
clarifyTextConfig_granularity :: Lens.Lens' ClarifyTextConfig ClarifyTextGranularity
clarifyTextConfig_granularity = Lens.lens (\ClarifyTextConfig' {granularity} -> granularity) (\s@ClarifyTextConfig' {} a -> s {granularity = a} :: ClarifyTextConfig)

instance Core.FromJSON ClarifyTextConfig where
  parseJSON =
    Core.withObject
      "ClarifyTextConfig"
      ( \x ->
          ClarifyTextConfig'
            Prelude.<$> (x Core..: "Language")
            Prelude.<*> (x Core..: "Granularity")
      )

instance Prelude.Hashable ClarifyTextConfig where
  hashWithSalt _salt ClarifyTextConfig' {..} =
    _salt `Prelude.hashWithSalt` language
      `Prelude.hashWithSalt` granularity

instance Prelude.NFData ClarifyTextConfig where
  rnf ClarifyTextConfig' {..} =
    Prelude.rnf language
      `Prelude.seq` Prelude.rnf granularity

instance Core.ToJSON ClarifyTextConfig where
  toJSON ClarifyTextConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Language" Core..= language),
            Prelude.Just ("Granularity" Core..= granularity)
          ]
      )
