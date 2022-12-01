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
-- Module      : Amazonka.IoTEventsData.Types.DisableActionConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types.DisableActionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains the configuration information of a disable action.
--
-- /See:/ 'newDisableActionConfiguration' smart constructor.
data DisableActionConfiguration = DisableActionConfiguration'
  { -- | The note that you can leave when you disable the alarm.
    note :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableActionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'note', 'disableActionConfiguration_note' - The note that you can leave when you disable the alarm.
newDisableActionConfiguration ::
  DisableActionConfiguration
newDisableActionConfiguration =
  DisableActionConfiguration' {note = Prelude.Nothing}

-- | The note that you can leave when you disable the alarm.
disableActionConfiguration_note :: Lens.Lens' DisableActionConfiguration (Prelude.Maybe Prelude.Text)
disableActionConfiguration_note = Lens.lens (\DisableActionConfiguration' {note} -> note) (\s@DisableActionConfiguration' {} a -> s {note = a} :: DisableActionConfiguration)

instance Core.FromJSON DisableActionConfiguration where
  parseJSON =
    Core.withObject
      "DisableActionConfiguration"
      ( \x ->
          DisableActionConfiguration'
            Prelude.<$> (x Core..:? "note")
      )

instance Prelude.Hashable DisableActionConfiguration where
  hashWithSalt _salt DisableActionConfiguration' {..} =
    _salt `Prelude.hashWithSalt` note

instance Prelude.NFData DisableActionConfiguration where
  rnf DisableActionConfiguration' {..} =
    Prelude.rnf note
