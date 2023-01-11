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
-- Module      : Amazonka.Inspector2.Types.FreeTrialInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.FreeTrialInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.FreeTrialStatus
import Amazonka.Inspector2.Types.FreeTrialType
import qualified Amazonka.Prelude as Prelude

-- | An object that contains information about the Amazon Inspector free
-- trial for an account.
--
-- /See:/ 'newFreeTrialInfo' smart constructor.
data FreeTrialInfo = FreeTrialInfo'
  { -- | The date and time that the Amazon Inspector free trail ends for a given
    -- account.
    end :: Data.POSIX,
    -- | The date and time that the Amazon Inspector free trail started for a
    -- given account.
    start :: Data.POSIX,
    -- | The order to sort results by.
    status :: FreeTrialStatus,
    -- | The type of scan covered by the Amazon Inspector free trail.
    type' :: FreeTrialType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FreeTrialInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'end', 'freeTrialInfo_end' - The date and time that the Amazon Inspector free trail ends for a given
-- account.
--
-- 'start', 'freeTrialInfo_start' - The date and time that the Amazon Inspector free trail started for a
-- given account.
--
-- 'status', 'freeTrialInfo_status' - The order to sort results by.
--
-- 'type'', 'freeTrialInfo_type' - The type of scan covered by the Amazon Inspector free trail.
newFreeTrialInfo ::
  -- | 'end'
  Prelude.UTCTime ->
  -- | 'start'
  Prelude.UTCTime ->
  -- | 'status'
  FreeTrialStatus ->
  -- | 'type''
  FreeTrialType ->
  FreeTrialInfo
newFreeTrialInfo pEnd_ pStart_ pStatus_ pType_ =
  FreeTrialInfo'
    { end = Data._Time Lens.# pEnd_,
      start = Data._Time Lens.# pStart_,
      status = pStatus_,
      type' = pType_
    }

-- | The date and time that the Amazon Inspector free trail ends for a given
-- account.
freeTrialInfo_end :: Lens.Lens' FreeTrialInfo Prelude.UTCTime
freeTrialInfo_end = Lens.lens (\FreeTrialInfo' {end} -> end) (\s@FreeTrialInfo' {} a -> s {end = a} :: FreeTrialInfo) Prelude.. Data._Time

-- | The date and time that the Amazon Inspector free trail started for a
-- given account.
freeTrialInfo_start :: Lens.Lens' FreeTrialInfo Prelude.UTCTime
freeTrialInfo_start = Lens.lens (\FreeTrialInfo' {start} -> start) (\s@FreeTrialInfo' {} a -> s {start = a} :: FreeTrialInfo) Prelude.. Data._Time

-- | The order to sort results by.
freeTrialInfo_status :: Lens.Lens' FreeTrialInfo FreeTrialStatus
freeTrialInfo_status = Lens.lens (\FreeTrialInfo' {status} -> status) (\s@FreeTrialInfo' {} a -> s {status = a} :: FreeTrialInfo)

-- | The type of scan covered by the Amazon Inspector free trail.
freeTrialInfo_type :: Lens.Lens' FreeTrialInfo FreeTrialType
freeTrialInfo_type = Lens.lens (\FreeTrialInfo' {type'} -> type') (\s@FreeTrialInfo' {} a -> s {type' = a} :: FreeTrialInfo)

instance Data.FromJSON FreeTrialInfo where
  parseJSON =
    Data.withObject
      "FreeTrialInfo"
      ( \x ->
          FreeTrialInfo'
            Prelude.<$> (x Data..: "end")
            Prelude.<*> (x Data..: "start")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable FreeTrialInfo where
  hashWithSalt _salt FreeTrialInfo' {..} =
    _salt `Prelude.hashWithSalt` end
      `Prelude.hashWithSalt` start
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` type'

instance Prelude.NFData FreeTrialInfo where
  rnf FreeTrialInfo' {..} =
    Prelude.rnf end
      `Prelude.seq` Prelude.rnf start
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf type'
