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
-- Module      : Amazonka.StepFunctions.Types.MapRunListItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.MapRunListItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details about a specific Map Run.
--
-- /See:/ 'newMapRunListItem' smart constructor.
data MapRunListItem = MapRunListItem'
  { -- | The date on which the Map Run stopped.
    stopDate :: Prelude.Maybe Data.POSIX,
    -- | The @executionArn@ of the execution from which the Map Run was started.
    executionArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Map Run.
    mapRunArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the executed state machine.
    stateMachineArn :: Prelude.Text,
    -- | The date on which the Map Run started.
    startDate :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MapRunListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stopDate', 'mapRunListItem_stopDate' - The date on which the Map Run stopped.
--
-- 'executionArn', 'mapRunListItem_executionArn' - The @executionArn@ of the execution from which the Map Run was started.
--
-- 'mapRunArn', 'mapRunListItem_mapRunArn' - The Amazon Resource Name (ARN) of the Map Run.
--
-- 'stateMachineArn', 'mapRunListItem_stateMachineArn' - The Amazon Resource Name (ARN) of the executed state machine.
--
-- 'startDate', 'mapRunListItem_startDate' - The date on which the Map Run started.
newMapRunListItem ::
  -- | 'executionArn'
  Prelude.Text ->
  -- | 'mapRunArn'
  Prelude.Text ->
  -- | 'stateMachineArn'
  Prelude.Text ->
  -- | 'startDate'
  Prelude.UTCTime ->
  MapRunListItem
newMapRunListItem
  pExecutionArn_
  pMapRunArn_
  pStateMachineArn_
  pStartDate_ =
    MapRunListItem'
      { stopDate = Prelude.Nothing,
        executionArn = pExecutionArn_,
        mapRunArn = pMapRunArn_,
        stateMachineArn = pStateMachineArn_,
        startDate = Data._Time Lens.# pStartDate_
      }

-- | The date on which the Map Run stopped.
mapRunListItem_stopDate :: Lens.Lens' MapRunListItem (Prelude.Maybe Prelude.UTCTime)
mapRunListItem_stopDate = Lens.lens (\MapRunListItem' {stopDate} -> stopDate) (\s@MapRunListItem' {} a -> s {stopDate = a} :: MapRunListItem) Prelude.. Lens.mapping Data._Time

-- | The @executionArn@ of the execution from which the Map Run was started.
mapRunListItem_executionArn :: Lens.Lens' MapRunListItem Prelude.Text
mapRunListItem_executionArn = Lens.lens (\MapRunListItem' {executionArn} -> executionArn) (\s@MapRunListItem' {} a -> s {executionArn = a} :: MapRunListItem)

-- | The Amazon Resource Name (ARN) of the Map Run.
mapRunListItem_mapRunArn :: Lens.Lens' MapRunListItem Prelude.Text
mapRunListItem_mapRunArn = Lens.lens (\MapRunListItem' {mapRunArn} -> mapRunArn) (\s@MapRunListItem' {} a -> s {mapRunArn = a} :: MapRunListItem)

-- | The Amazon Resource Name (ARN) of the executed state machine.
mapRunListItem_stateMachineArn :: Lens.Lens' MapRunListItem Prelude.Text
mapRunListItem_stateMachineArn = Lens.lens (\MapRunListItem' {stateMachineArn} -> stateMachineArn) (\s@MapRunListItem' {} a -> s {stateMachineArn = a} :: MapRunListItem)

-- | The date on which the Map Run started.
mapRunListItem_startDate :: Lens.Lens' MapRunListItem Prelude.UTCTime
mapRunListItem_startDate = Lens.lens (\MapRunListItem' {startDate} -> startDate) (\s@MapRunListItem' {} a -> s {startDate = a} :: MapRunListItem) Prelude.. Data._Time

instance Data.FromJSON MapRunListItem where
  parseJSON =
    Data.withObject
      "MapRunListItem"
      ( \x ->
          MapRunListItem'
            Prelude.<$> (x Data..:? "stopDate")
            Prelude.<*> (x Data..: "executionArn")
            Prelude.<*> (x Data..: "mapRunArn")
            Prelude.<*> (x Data..: "stateMachineArn")
            Prelude.<*> (x Data..: "startDate")
      )

instance Prelude.Hashable MapRunListItem where
  hashWithSalt _salt MapRunListItem' {..} =
    _salt
      `Prelude.hashWithSalt` stopDate
      `Prelude.hashWithSalt` executionArn
      `Prelude.hashWithSalt` mapRunArn
      `Prelude.hashWithSalt` stateMachineArn
      `Prelude.hashWithSalt` startDate

instance Prelude.NFData MapRunListItem where
  rnf MapRunListItem' {..} =
    Prelude.rnf stopDate
      `Prelude.seq` Prelude.rnf executionArn
      `Prelude.seq` Prelude.rnf mapRunArn
      `Prelude.seq` Prelude.rnf stateMachineArn
      `Prelude.seq` Prelude.rnf startDate
