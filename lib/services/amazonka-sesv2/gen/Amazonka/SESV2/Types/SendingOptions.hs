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
-- Module      : Amazonka.SESV2.Types.SendingOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.SendingOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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

instance Data.FromJSON SendingOptions where
  parseJSON =
    Data.withObject
      "SendingOptions"
      ( \x ->
          SendingOptions'
            Prelude.<$> (x Data..:? "SendingEnabled")
      )

instance Prelude.Hashable SendingOptions where
  hashWithSalt _salt SendingOptions' {..} =
    _salt `Prelude.hashWithSalt` sendingEnabled

instance Prelude.NFData SendingOptions where
  rnf SendingOptions' {..} = Prelude.rnf sendingEnabled

instance Data.ToJSON SendingOptions where
  toJSON SendingOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SendingEnabled" Data..=)
              Prelude.<$> sendingEnabled
          ]
      )
