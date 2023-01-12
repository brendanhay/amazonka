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
-- Module      : Amazonka.MediaConvert.Types.DvbTdtSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.DvbTdtSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Use these settings to insert a DVB Time and Date Table (TDT) in the
-- transport stream of this output. When you work directly in your JSON job
-- specification, include this object only when your job has a transport
-- stream output and the container settings contain the object
-- M2tsSettings.
--
-- /See:/ 'newDvbTdtSettings' smart constructor.
data DvbTdtSettings = DvbTdtSettings'
  { -- | The number of milliseconds between instances of this table in the output
    -- transport stream.
    tdtInterval :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DvbTdtSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tdtInterval', 'dvbTdtSettings_tdtInterval' - The number of milliseconds between instances of this table in the output
-- transport stream.
newDvbTdtSettings ::
  DvbTdtSettings
newDvbTdtSettings =
  DvbTdtSettings' {tdtInterval = Prelude.Nothing}

-- | The number of milliseconds between instances of this table in the output
-- transport stream.
dvbTdtSettings_tdtInterval :: Lens.Lens' DvbTdtSettings (Prelude.Maybe Prelude.Natural)
dvbTdtSettings_tdtInterval = Lens.lens (\DvbTdtSettings' {tdtInterval} -> tdtInterval) (\s@DvbTdtSettings' {} a -> s {tdtInterval = a} :: DvbTdtSettings)

instance Data.FromJSON DvbTdtSettings where
  parseJSON =
    Data.withObject
      "DvbTdtSettings"
      ( \x ->
          DvbTdtSettings'
            Prelude.<$> (x Data..:? "tdtInterval")
      )

instance Prelude.Hashable DvbTdtSettings where
  hashWithSalt _salt DvbTdtSettings' {..} =
    _salt `Prelude.hashWithSalt` tdtInterval

instance Prelude.NFData DvbTdtSettings where
  rnf DvbTdtSettings' {..} = Prelude.rnf tdtInterval

instance Data.ToJSON DvbTdtSettings where
  toJSON DvbTdtSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [("tdtInterval" Data..=) Prelude.<$> tdtInterval]
      )
