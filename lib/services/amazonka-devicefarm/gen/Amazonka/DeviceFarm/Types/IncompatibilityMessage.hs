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
-- Module      : Amazonka.DeviceFarm.Types.IncompatibilityMessage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.IncompatibilityMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types.DeviceAttribute
import qualified Amazonka.Prelude as Prelude

-- | Represents information about incompatibility.
--
-- /See:/ 'newIncompatibilityMessage' smart constructor.
data IncompatibilityMessage = IncompatibilityMessage'
  { -- | A message about the incompatibility.
    message :: Prelude.Maybe Prelude.Text,
    -- | The type of incompatibility.
    --
    -- Allowed values include:
    --
    -- -   ARN
    --
    -- -   FORM_FACTOR (for example, phone or tablet)
    --
    -- -   MANUFACTURER
    --
    -- -   PLATFORM (for example, Android or iOS)
    --
    -- -   REMOTE_ACCESS_ENABLED
    --
    -- -   APPIUM_VERSION
    type' :: Prelude.Maybe DeviceAttribute
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IncompatibilityMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'incompatibilityMessage_message' - A message about the incompatibility.
--
-- 'type'', 'incompatibilityMessage_type' - The type of incompatibility.
--
-- Allowed values include:
--
-- -   ARN
--
-- -   FORM_FACTOR (for example, phone or tablet)
--
-- -   MANUFACTURER
--
-- -   PLATFORM (for example, Android or iOS)
--
-- -   REMOTE_ACCESS_ENABLED
--
-- -   APPIUM_VERSION
newIncompatibilityMessage ::
  IncompatibilityMessage
newIncompatibilityMessage =
  IncompatibilityMessage'
    { message = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | A message about the incompatibility.
incompatibilityMessage_message :: Lens.Lens' IncompatibilityMessage (Prelude.Maybe Prelude.Text)
incompatibilityMessage_message = Lens.lens (\IncompatibilityMessage' {message} -> message) (\s@IncompatibilityMessage' {} a -> s {message = a} :: IncompatibilityMessage)

-- | The type of incompatibility.
--
-- Allowed values include:
--
-- -   ARN
--
-- -   FORM_FACTOR (for example, phone or tablet)
--
-- -   MANUFACTURER
--
-- -   PLATFORM (for example, Android or iOS)
--
-- -   REMOTE_ACCESS_ENABLED
--
-- -   APPIUM_VERSION
incompatibilityMessage_type :: Lens.Lens' IncompatibilityMessage (Prelude.Maybe DeviceAttribute)
incompatibilityMessage_type = Lens.lens (\IncompatibilityMessage' {type'} -> type') (\s@IncompatibilityMessage' {} a -> s {type' = a} :: IncompatibilityMessage)

instance Data.FromJSON IncompatibilityMessage where
  parseJSON =
    Data.withObject
      "IncompatibilityMessage"
      ( \x ->
          IncompatibilityMessage'
            Prelude.<$> (x Data..:? "message")
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable IncompatibilityMessage where
  hashWithSalt _salt IncompatibilityMessage' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` type'

instance Prelude.NFData IncompatibilityMessage where
  rnf IncompatibilityMessage' {..} =
    Prelude.rnf message `Prelude.seq` Prelude.rnf type'
