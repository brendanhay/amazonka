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
-- Module      : Amazonka.LexV2Models.Types.PlainTextMessage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.PlainTextMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines an ASCII text message to send to the user.
--
-- /See:/ 'newPlainTextMessage' smart constructor.
data PlainTextMessage = PlainTextMessage'
  { -- | The message to send to the user.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PlainTextMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'plainTextMessage_value' - The message to send to the user.
newPlainTextMessage ::
  -- | 'value'
  Prelude.Text ->
  PlainTextMessage
newPlainTextMessage pValue_ =
  PlainTextMessage' {value = pValue_}

-- | The message to send to the user.
plainTextMessage_value :: Lens.Lens' PlainTextMessage Prelude.Text
plainTextMessage_value = Lens.lens (\PlainTextMessage' {value} -> value) (\s@PlainTextMessage' {} a -> s {value = a} :: PlainTextMessage)

instance Data.FromJSON PlainTextMessage where
  parseJSON =
    Data.withObject
      "PlainTextMessage"
      ( \x ->
          PlainTextMessage' Prelude.<$> (x Data..: "value")
      )

instance Prelude.Hashable PlainTextMessage where
  hashWithSalt _salt PlainTextMessage' {..} =
    _salt `Prelude.hashWithSalt` value

instance Prelude.NFData PlainTextMessage where
  rnf PlainTextMessage' {..} = Prelude.rnf value

instance Data.ToJSON PlainTextMessage where
  toJSON PlainTextMessage' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("value" Data..= value)]
      )
