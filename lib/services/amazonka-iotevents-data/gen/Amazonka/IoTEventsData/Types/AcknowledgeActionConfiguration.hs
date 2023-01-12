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
-- Module      : Amazonka.IoTEventsData.Types.AcknowledgeActionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types.AcknowledgeActionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the configuration information of an acknowledge action.
--
-- /See:/ 'newAcknowledgeActionConfiguration' smart constructor.
data AcknowledgeActionConfiguration = AcknowledgeActionConfiguration'
  { -- | The note that you can leave when you acknowledge the alarm.
    note :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcknowledgeActionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'note', 'acknowledgeActionConfiguration_note' - The note that you can leave when you acknowledge the alarm.
newAcknowledgeActionConfiguration ::
  AcknowledgeActionConfiguration
newAcknowledgeActionConfiguration =
  AcknowledgeActionConfiguration'
    { note =
        Prelude.Nothing
    }

-- | The note that you can leave when you acknowledge the alarm.
acknowledgeActionConfiguration_note :: Lens.Lens' AcknowledgeActionConfiguration (Prelude.Maybe Prelude.Text)
acknowledgeActionConfiguration_note = Lens.lens (\AcknowledgeActionConfiguration' {note} -> note) (\s@AcknowledgeActionConfiguration' {} a -> s {note = a} :: AcknowledgeActionConfiguration)

instance Data.FromJSON AcknowledgeActionConfiguration where
  parseJSON =
    Data.withObject
      "AcknowledgeActionConfiguration"
      ( \x ->
          AcknowledgeActionConfiguration'
            Prelude.<$> (x Data..:? "note")
      )

instance
  Prelude.Hashable
    AcknowledgeActionConfiguration
  where
  hashWithSalt
    _salt
    AcknowledgeActionConfiguration' {..} =
      _salt `Prelude.hashWithSalt` note

instance
  Prelude.NFData
    AcknowledgeActionConfiguration
  where
  rnf AcknowledgeActionConfiguration' {..} =
    Prelude.rnf note
