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
-- Module      : Amazonka.Kendra.Types.AdditionalResultAttributeValue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.AdditionalResultAttributeValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kendra.Types.TextWithHighlights
import qualified Amazonka.Prelude as Prelude

-- | An attribute returned with a document from a search.
--
-- /See:/ 'newAdditionalResultAttributeValue' smart constructor.
data AdditionalResultAttributeValue = AdditionalResultAttributeValue'
  { -- | The text associated with the attribute and information about the
    -- highlight to apply to the text.
    textWithHighlightsValue :: Prelude.Maybe TextWithHighlights
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdditionalResultAttributeValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'textWithHighlightsValue', 'additionalResultAttributeValue_textWithHighlightsValue' - The text associated with the attribute and information about the
-- highlight to apply to the text.
newAdditionalResultAttributeValue ::
  AdditionalResultAttributeValue
newAdditionalResultAttributeValue =
  AdditionalResultAttributeValue'
    { textWithHighlightsValue =
        Prelude.Nothing
    }

-- | The text associated with the attribute and information about the
-- highlight to apply to the text.
additionalResultAttributeValue_textWithHighlightsValue :: Lens.Lens' AdditionalResultAttributeValue (Prelude.Maybe TextWithHighlights)
additionalResultAttributeValue_textWithHighlightsValue = Lens.lens (\AdditionalResultAttributeValue' {textWithHighlightsValue} -> textWithHighlightsValue) (\s@AdditionalResultAttributeValue' {} a -> s {textWithHighlightsValue = a} :: AdditionalResultAttributeValue)

instance Core.FromJSON AdditionalResultAttributeValue where
  parseJSON =
    Core.withObject
      "AdditionalResultAttributeValue"
      ( \x ->
          AdditionalResultAttributeValue'
            Prelude.<$> (x Core..:? "TextWithHighlightsValue")
      )

instance
  Prelude.Hashable
    AdditionalResultAttributeValue
  where
  hashWithSalt
    _salt
    AdditionalResultAttributeValue' {..} =
      _salt
        `Prelude.hashWithSalt` textWithHighlightsValue

instance
  Prelude.NFData
    AdditionalResultAttributeValue
  where
  rnf AdditionalResultAttributeValue' {..} =
    Prelude.rnf textWithHighlightsValue
