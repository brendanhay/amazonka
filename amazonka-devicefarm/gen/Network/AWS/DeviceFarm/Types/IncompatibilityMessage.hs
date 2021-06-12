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
-- Module      : Network.AWS.DeviceFarm.Types.IncompatibilityMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.IncompatibilityMessage where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types.DeviceAttribute
import qualified Network.AWS.Lens as Lens

-- | Represents information about incompatibility.
--
-- /See:/ 'newIncompatibilityMessage' smart constructor.
data IncompatibilityMessage = IncompatibilityMessage'
  { -- | A message about the incompatibility.
    message :: Core.Maybe Core.Text,
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
    type' :: Core.Maybe DeviceAttribute
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { message = Core.Nothing,
      type' = Core.Nothing
    }

-- | A message about the incompatibility.
incompatibilityMessage_message :: Lens.Lens' IncompatibilityMessage (Core.Maybe Core.Text)
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
incompatibilityMessage_type :: Lens.Lens' IncompatibilityMessage (Core.Maybe DeviceAttribute)
incompatibilityMessage_type = Lens.lens (\IncompatibilityMessage' {type'} -> type') (\s@IncompatibilityMessage' {} a -> s {type' = a} :: IncompatibilityMessage)

instance Core.FromJSON IncompatibilityMessage where
  parseJSON =
    Core.withObject
      "IncompatibilityMessage"
      ( \x ->
          IncompatibilityMessage'
            Core.<$> (x Core..:? "message") Core.<*> (x Core..:? "type")
      )

instance Core.Hashable IncompatibilityMessage

instance Core.NFData IncompatibilityMessage
