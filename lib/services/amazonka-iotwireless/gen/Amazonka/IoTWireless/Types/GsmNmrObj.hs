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
-- Module      : Amazonka.IoTWireless.Types.GsmNmrObj
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.GsmNmrObj where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.GlobalIdentity
import qualified Amazonka.Prelude as Prelude

-- | GSM object for network measurement reports.
--
-- /See:/ 'newGsmNmrObj' smart constructor.
data GsmNmrObj = GsmNmrObj'
  { -- | Global identity information of the GSM object.
    globalIdentity :: Prelude.Maybe GlobalIdentity,
    -- | Rx level, which is the received signal power, measured in dBm
    -- (decibel-milliwatts).
    rxLevel :: Prelude.Maybe Prelude.Int,
    -- | GSM base station identity code (BSIC).
    bsic :: Prelude.Natural,
    -- | GSM broadcast control channel.
    bcch :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GsmNmrObj' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalIdentity', 'gsmNmrObj_globalIdentity' - Global identity information of the GSM object.
--
-- 'rxLevel', 'gsmNmrObj_rxLevel' - Rx level, which is the received signal power, measured in dBm
-- (decibel-milliwatts).
--
-- 'bsic', 'gsmNmrObj_bsic' - GSM base station identity code (BSIC).
--
-- 'bcch', 'gsmNmrObj_bcch' - GSM broadcast control channel.
newGsmNmrObj ::
  -- | 'bsic'
  Prelude.Natural ->
  -- | 'bcch'
  Prelude.Natural ->
  GsmNmrObj
newGsmNmrObj pBsic_ pBcch_ =
  GsmNmrObj'
    { globalIdentity = Prelude.Nothing,
      rxLevel = Prelude.Nothing,
      bsic = pBsic_,
      bcch = pBcch_
    }

-- | Global identity information of the GSM object.
gsmNmrObj_globalIdentity :: Lens.Lens' GsmNmrObj (Prelude.Maybe GlobalIdentity)
gsmNmrObj_globalIdentity = Lens.lens (\GsmNmrObj' {globalIdentity} -> globalIdentity) (\s@GsmNmrObj' {} a -> s {globalIdentity = a} :: GsmNmrObj)

-- | Rx level, which is the received signal power, measured in dBm
-- (decibel-milliwatts).
gsmNmrObj_rxLevel :: Lens.Lens' GsmNmrObj (Prelude.Maybe Prelude.Int)
gsmNmrObj_rxLevel = Lens.lens (\GsmNmrObj' {rxLevel} -> rxLevel) (\s@GsmNmrObj' {} a -> s {rxLevel = a} :: GsmNmrObj)

-- | GSM base station identity code (BSIC).
gsmNmrObj_bsic :: Lens.Lens' GsmNmrObj Prelude.Natural
gsmNmrObj_bsic = Lens.lens (\GsmNmrObj' {bsic} -> bsic) (\s@GsmNmrObj' {} a -> s {bsic = a} :: GsmNmrObj)

-- | GSM broadcast control channel.
gsmNmrObj_bcch :: Lens.Lens' GsmNmrObj Prelude.Natural
gsmNmrObj_bcch = Lens.lens (\GsmNmrObj' {bcch} -> bcch) (\s@GsmNmrObj' {} a -> s {bcch = a} :: GsmNmrObj)

instance Prelude.Hashable GsmNmrObj where
  hashWithSalt _salt GsmNmrObj' {..} =
    _salt `Prelude.hashWithSalt` globalIdentity
      `Prelude.hashWithSalt` rxLevel
      `Prelude.hashWithSalt` bsic
      `Prelude.hashWithSalt` bcch

instance Prelude.NFData GsmNmrObj where
  rnf GsmNmrObj' {..} =
    Prelude.rnf globalIdentity
      `Prelude.seq` Prelude.rnf rxLevel
      `Prelude.seq` Prelude.rnf bsic
      `Prelude.seq` Prelude.rnf bcch

instance Data.ToJSON GsmNmrObj where
  toJSON GsmNmrObj' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GlobalIdentity" Data..=)
              Prelude.<$> globalIdentity,
            ("RxLevel" Data..=) Prelude.<$> rxLevel,
            Prelude.Just ("Bsic" Data..= bsic),
            Prelude.Just ("Bcch" Data..= bcch)
          ]
      )
