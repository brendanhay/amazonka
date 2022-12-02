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
-- Module      : Amazonka.MediaLive.Types.DvbTdtSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.DvbTdtSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | DVB Time and Date Table (SDT)
--
-- /See:/ 'newDvbTdtSettings' smart constructor.
data DvbTdtSettings = DvbTdtSettings'
  { -- | The number of milliseconds between instances of this table in the output
    -- transport stream.
    repInterval :: Prelude.Maybe Prelude.Natural
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
-- 'repInterval', 'dvbTdtSettings_repInterval' - The number of milliseconds between instances of this table in the output
-- transport stream.
newDvbTdtSettings ::
  DvbTdtSettings
newDvbTdtSettings =
  DvbTdtSettings' {repInterval = Prelude.Nothing}

-- | The number of milliseconds between instances of this table in the output
-- transport stream.
dvbTdtSettings_repInterval :: Lens.Lens' DvbTdtSettings (Prelude.Maybe Prelude.Natural)
dvbTdtSettings_repInterval = Lens.lens (\DvbTdtSettings' {repInterval} -> repInterval) (\s@DvbTdtSettings' {} a -> s {repInterval = a} :: DvbTdtSettings)

instance Data.FromJSON DvbTdtSettings where
  parseJSON =
    Data.withObject
      "DvbTdtSettings"
      ( \x ->
          DvbTdtSettings'
            Prelude.<$> (x Data..:? "repInterval")
      )

instance Prelude.Hashable DvbTdtSettings where
  hashWithSalt _salt DvbTdtSettings' {..} =
    _salt `Prelude.hashWithSalt` repInterval

instance Prelude.NFData DvbTdtSettings where
  rnf DvbTdtSettings' {..} = Prelude.rnf repInterval

instance Data.ToJSON DvbTdtSettings where
  toJSON DvbTdtSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [("repInterval" Data..=) Prelude.<$> repInterval]
      )
