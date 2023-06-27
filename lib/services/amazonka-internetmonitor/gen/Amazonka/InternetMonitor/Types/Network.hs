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
-- Module      : Amazonka.InternetMonitor.Types.Network
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.InternetMonitor.Types.Network where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An internet service provider (ISP) or network in Amazon CloudWatch
-- Internet Monitor.
--
-- /See:/ 'newNetwork' smart constructor.
data Network = Network'
  { -- | The internet provider name or network name.
    aSName :: Prelude.Text,
    -- | The Autonomous System Number (ASN) of the internet provider or network.
    aSNumber :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Network' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aSName', 'network_aSName' - The internet provider name or network name.
--
-- 'aSNumber', 'network_aSNumber' - The Autonomous System Number (ASN) of the internet provider or network.
newNetwork ::
  -- | 'aSName'
  Prelude.Text ->
  -- | 'aSNumber'
  Prelude.Integer ->
  Network
newNetwork pASName_ pASNumber_ =
  Network' {aSName = pASName_, aSNumber = pASNumber_}

-- | The internet provider name or network name.
network_aSName :: Lens.Lens' Network Prelude.Text
network_aSName = Lens.lens (\Network' {aSName} -> aSName) (\s@Network' {} a -> s {aSName = a} :: Network)

-- | The Autonomous System Number (ASN) of the internet provider or network.
network_aSNumber :: Lens.Lens' Network Prelude.Integer
network_aSNumber = Lens.lens (\Network' {aSNumber} -> aSNumber) (\s@Network' {} a -> s {aSNumber = a} :: Network)

instance Data.FromJSON Network where
  parseJSON =
    Data.withObject
      "Network"
      ( \x ->
          Network'
            Prelude.<$> (x Data..: "ASName")
            Prelude.<*> (x Data..: "ASNumber")
      )

instance Prelude.Hashable Network where
  hashWithSalt _salt Network' {..} =
    _salt
      `Prelude.hashWithSalt` aSName
      `Prelude.hashWithSalt` aSNumber

instance Prelude.NFData Network where
  rnf Network' {..} =
    Prelude.rnf aSName
      `Prelude.seq` Prelude.rnf aSNumber
