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
-- Module      : Amazonka.IoTEventsData.Types.EnableActionConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types.EnableActionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the configuration information of an enable action.
--
-- /See:/ 'newEnableActionConfiguration' smart constructor.
data EnableActionConfiguration = EnableActionConfiguration'
  { -- | The note that you can leave when you enable the alarm.
    note :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableActionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'note', 'enableActionConfiguration_note' - The note that you can leave when you enable the alarm.
newEnableActionConfiguration ::
  EnableActionConfiguration
newEnableActionConfiguration =
  EnableActionConfiguration' {note = Prelude.Nothing}

-- | The note that you can leave when you enable the alarm.
enableActionConfiguration_note :: Lens.Lens' EnableActionConfiguration (Prelude.Maybe Prelude.Text)
enableActionConfiguration_note = Lens.lens (\EnableActionConfiguration' {note} -> note) (\s@EnableActionConfiguration' {} a -> s {note = a} :: EnableActionConfiguration)

instance Data.FromJSON EnableActionConfiguration where
  parseJSON =
    Data.withObject
      "EnableActionConfiguration"
      ( \x ->
          EnableActionConfiguration'
            Prelude.<$> (x Data..:? "note")
      )

instance Prelude.Hashable EnableActionConfiguration where
  hashWithSalt _salt EnableActionConfiguration' {..} =
    _salt `Prelude.hashWithSalt` note

instance Prelude.NFData EnableActionConfiguration where
  rnf EnableActionConfiguration' {..} = Prelude.rnf note
