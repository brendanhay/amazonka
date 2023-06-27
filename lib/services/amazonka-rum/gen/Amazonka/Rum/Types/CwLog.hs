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
-- Module      : Amazonka.Rum.Types.CwLog
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rum.Types.CwLog where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains the information about whether the app monitor
-- stores copies of the data that RUM collects in CloudWatch Logs. If it
-- does, this structure also contains the name of the log group.
--
-- /See:/ 'newCwLog' smart constructor.
data CwLog = CwLog'
  { -- | Indicated whether the app monitor stores copies of the data that RUM
    -- collects in CloudWatch Logs.
    cwLogEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the log group where the copies are stored.
    cwLogGroup :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CwLog' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cwLogEnabled', 'cwLog_cwLogEnabled' - Indicated whether the app monitor stores copies of the data that RUM
-- collects in CloudWatch Logs.
--
-- 'cwLogGroup', 'cwLog_cwLogGroup' - The name of the log group where the copies are stored.
newCwLog ::
  CwLog
newCwLog =
  CwLog'
    { cwLogEnabled = Prelude.Nothing,
      cwLogGroup = Prelude.Nothing
    }

-- | Indicated whether the app monitor stores copies of the data that RUM
-- collects in CloudWatch Logs.
cwLog_cwLogEnabled :: Lens.Lens' CwLog (Prelude.Maybe Prelude.Bool)
cwLog_cwLogEnabled = Lens.lens (\CwLog' {cwLogEnabled} -> cwLogEnabled) (\s@CwLog' {} a -> s {cwLogEnabled = a} :: CwLog)

-- | The name of the log group where the copies are stored.
cwLog_cwLogGroup :: Lens.Lens' CwLog (Prelude.Maybe Prelude.Text)
cwLog_cwLogGroup = Lens.lens (\CwLog' {cwLogGroup} -> cwLogGroup) (\s@CwLog' {} a -> s {cwLogGroup = a} :: CwLog)

instance Data.FromJSON CwLog where
  parseJSON =
    Data.withObject
      "CwLog"
      ( \x ->
          CwLog'
            Prelude.<$> (x Data..:? "CwLogEnabled")
            Prelude.<*> (x Data..:? "CwLogGroup")
      )

instance Prelude.Hashable CwLog where
  hashWithSalt _salt CwLog' {..} =
    _salt
      `Prelude.hashWithSalt` cwLogEnabled
      `Prelude.hashWithSalt` cwLogGroup

instance Prelude.NFData CwLog where
  rnf CwLog' {..} =
    Prelude.rnf cwLogEnabled
      `Prelude.seq` Prelude.rnf cwLogGroup
