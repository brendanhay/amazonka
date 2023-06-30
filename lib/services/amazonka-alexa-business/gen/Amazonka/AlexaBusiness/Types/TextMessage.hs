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
-- Module      : Amazonka.AlexaBusiness.Types.TextMessage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.TextMessage where

import Amazonka.AlexaBusiness.Types.Locale
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The text message.
--
-- /See:/ 'newTextMessage' smart constructor.
data TextMessage = TextMessage'
  { -- | The locale of the text message. Currently, en-US is supported.
    locale :: Locale,
    -- | The value of the text message.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TextMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locale', 'textMessage_locale' - The locale of the text message. Currently, en-US is supported.
--
-- 'value', 'textMessage_value' - The value of the text message.
newTextMessage ::
  -- | 'locale'
  Locale ->
  -- | 'value'
  Prelude.Text ->
  TextMessage
newTextMessage pLocale_ pValue_ =
  TextMessage' {locale = pLocale_, value = pValue_}

-- | The locale of the text message. Currently, en-US is supported.
textMessage_locale :: Lens.Lens' TextMessage Locale
textMessage_locale = Lens.lens (\TextMessage' {locale} -> locale) (\s@TextMessage' {} a -> s {locale = a} :: TextMessage)

-- | The value of the text message.
textMessage_value :: Lens.Lens' TextMessage Prelude.Text
textMessage_value = Lens.lens (\TextMessage' {value} -> value) (\s@TextMessage' {} a -> s {value = a} :: TextMessage)

instance Prelude.Hashable TextMessage where
  hashWithSalt _salt TextMessage' {..} =
    _salt
      `Prelude.hashWithSalt` locale
      `Prelude.hashWithSalt` value

instance Prelude.NFData TextMessage where
  rnf TextMessage' {..} =
    Prelude.rnf locale `Prelude.seq` Prelude.rnf value

instance Data.ToJSON TextMessage where
  toJSON TextMessage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Locale" Data..= locale),
            Prelude.Just ("Value" Data..= value)
          ]
      )
