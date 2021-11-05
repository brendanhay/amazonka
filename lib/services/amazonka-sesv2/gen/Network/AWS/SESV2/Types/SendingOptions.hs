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
-- Module      : Network.AWS.SESV2.Types.SendingOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESV2.Types.SendingOptions where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Used to enable or disable email sending for messages that use this
-- configuration set in the current Amazon Web Services Region.
--
-- /See:/ 'newSendingOptions' smart constructor.
data SendingOptions = SendingOptions'
  { -- | If @true@, email sending is enabled for the configuration set. If
    -- @false@, email sending is disabled for the configuration set.
    sendingEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendingOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sendingEnabled', 'sendingOptions_sendingEnabled' - If @true@, email sending is enabled for the configuration set. If
-- @false@, email sending is disabled for the configuration set.
newSendingOptions ::
  SendingOptions
newSendingOptions =
  SendingOptions' {sendingEnabled = Prelude.Nothing}

-- | If @true@, email sending is enabled for the configuration set. If
-- @false@, email sending is disabled for the configuration set.
sendingOptions_sendingEnabled :: Lens.Lens' SendingOptions (Prelude.Maybe Prelude.Bool)
sendingOptions_sendingEnabled = Lens.lens (\SendingOptions' {sendingEnabled} -> sendingEnabled) (\s@SendingOptions' {} a -> s {sendingEnabled = a} :: SendingOptions)

instance Core.FromJSON SendingOptions where
  parseJSON =
    Core.withObject
      "SendingOptions"
      ( \x ->
          SendingOptions'
            Prelude.<$> (x Core..:? "SendingEnabled")
      )

instance Prelude.Hashable SendingOptions

instance Prelude.NFData SendingOptions

instance Core.ToJSON SendingOptions where
  toJSON SendingOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SendingEnabled" Core..=)
              Prelude.<$> sendingEnabled
          ]
      )
