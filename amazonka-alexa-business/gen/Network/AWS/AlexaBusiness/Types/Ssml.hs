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
-- Module      : Network.AWS.AlexaBusiness.Types.Ssml
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Ssml where

import Network.AWS.AlexaBusiness.Types.Locale
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The SSML message. For more information, see
-- <https://developer.amazon.com/docs/custom-skills/speech-synthesis-markup-language-ssml-reference.html SSML Reference>.
--
-- /See:/ 'newSsml' smart constructor.
data Ssml = Ssml'
  { -- | The locale of the SSML message. Currently, en-US is supported.
    locale :: Locale,
    -- | The value of the SSML message in the correct SSML format. The audio tag
    -- is not supported.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Ssml' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locale', 'ssml_locale' - The locale of the SSML message. Currently, en-US is supported.
--
-- 'value', 'ssml_value' - The value of the SSML message in the correct SSML format. The audio tag
-- is not supported.
newSsml ::
  -- | 'locale'
  Locale ->
  -- | 'value'
  Prelude.Text ->
  Ssml
newSsml pLocale_ pValue_ =
  Ssml' {locale = pLocale_, value = pValue_}

-- | The locale of the SSML message. Currently, en-US is supported.
ssml_locale :: Lens.Lens' Ssml Locale
ssml_locale = Lens.lens (\Ssml' {locale} -> locale) (\s@Ssml' {} a -> s {locale = a} :: Ssml)

-- | The value of the SSML message in the correct SSML format. The audio tag
-- is not supported.
ssml_value :: Lens.Lens' Ssml Prelude.Text
ssml_value = Lens.lens (\Ssml' {value} -> value) (\s@Ssml' {} a -> s {value = a} :: Ssml)

instance Prelude.Hashable Ssml

instance Prelude.NFData Ssml

instance Prelude.ToJSON Ssml where
  toJSON Ssml' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Locale" Prelude..= locale),
            Prelude.Just ("Value" Prelude..= value)
          ]
      )
