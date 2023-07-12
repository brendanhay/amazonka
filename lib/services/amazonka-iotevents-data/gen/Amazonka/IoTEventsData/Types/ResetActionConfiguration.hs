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
-- Module      : Amazonka.IoTEventsData.Types.ResetActionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types.ResetActionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the configuration information of a reset action.
--
-- /See:/ 'newResetActionConfiguration' smart constructor.
data ResetActionConfiguration = ResetActionConfiguration'
  { -- | The note that you can leave when you reset the alarm.
    note :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetActionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'note', 'resetActionConfiguration_note' - The note that you can leave when you reset the alarm.
newResetActionConfiguration ::
  ResetActionConfiguration
newResetActionConfiguration =
  ResetActionConfiguration' {note = Prelude.Nothing}

-- | The note that you can leave when you reset the alarm.
resetActionConfiguration_note :: Lens.Lens' ResetActionConfiguration (Prelude.Maybe Prelude.Text)
resetActionConfiguration_note = Lens.lens (\ResetActionConfiguration' {note} -> note) (\s@ResetActionConfiguration' {} a -> s {note = a} :: ResetActionConfiguration)

instance Data.FromJSON ResetActionConfiguration where
  parseJSON =
    Data.withObject
      "ResetActionConfiguration"
      ( \x ->
          ResetActionConfiguration'
            Prelude.<$> (x Data..:? "note")
      )

instance Prelude.Hashable ResetActionConfiguration where
  hashWithSalt _salt ResetActionConfiguration' {..} =
    _salt `Prelude.hashWithSalt` note

instance Prelude.NFData ResetActionConfiguration where
  rnf ResetActionConfiguration' {..} = Prelude.rnf note
