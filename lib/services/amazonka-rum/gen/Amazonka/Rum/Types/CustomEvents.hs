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
-- Module      : Amazonka.Rum.Types.CustomEvents
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rum.Types.CustomEvents where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rum.Types.CustomEventsStatus

-- | A structure that contains information about custom events for this app
-- monitor.
--
-- /See:/ 'newCustomEvents' smart constructor.
data CustomEvents = CustomEvents'
  { -- | Specifies whether this app monitor allows the web client to define and
    -- send custom events. The default is for custom events to be @DISABLED@.
    status :: Prelude.Maybe CustomEventsStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'customEvents_status' - Specifies whether this app monitor allows the web client to define and
-- send custom events. The default is for custom events to be @DISABLED@.
newCustomEvents ::
  CustomEvents
newCustomEvents =
  CustomEvents' {status = Prelude.Nothing}

-- | Specifies whether this app monitor allows the web client to define and
-- send custom events. The default is for custom events to be @DISABLED@.
customEvents_status :: Lens.Lens' CustomEvents (Prelude.Maybe CustomEventsStatus)
customEvents_status = Lens.lens (\CustomEvents' {status} -> status) (\s@CustomEvents' {} a -> s {status = a} :: CustomEvents)

instance Data.FromJSON CustomEvents where
  parseJSON =
    Data.withObject
      "CustomEvents"
      ( \x ->
          CustomEvents' Prelude.<$> (x Data..:? "Status")
      )

instance Prelude.Hashable CustomEvents where
  hashWithSalt _salt CustomEvents' {..} =
    _salt `Prelude.hashWithSalt` status

instance Prelude.NFData CustomEvents where
  rnf CustomEvents' {..} = Prelude.rnf status

instance Data.ToJSON CustomEvents where
  toJSON CustomEvents' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Status" Data..=) Prelude.<$> status]
      )
