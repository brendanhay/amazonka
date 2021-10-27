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
-- Module      : Network.AWS.LexV2Models.Types.PlainTextMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Models.Types.PlainTextMessage where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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

instance Core.FromJSON PlainTextMessage where
  parseJSON =
    Core.withObject
      "PlainTextMessage"
      ( \x ->
          PlainTextMessage' Prelude.<$> (x Core..: "value")
      )

instance Prelude.Hashable PlainTextMessage

instance Prelude.NFData PlainTextMessage

instance Core.ToJSON PlainTextMessage where
  toJSON PlainTextMessage' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("value" Core..= value)]
      )
