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
-- Module      : Amazonka.IoTWireless.Types.SidewalkAccountInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.SidewalkAccountInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a Sidewalk account.
--
-- /See:/ 'newSidewalkAccountInfo' smart constructor.
data SidewalkAccountInfo = SidewalkAccountInfo'
  { -- | The Sidewalk application server private key.
    appServerPrivateKey :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The Sidewalk Amazon ID.
    amazonId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SidewalkAccountInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appServerPrivateKey', 'sidewalkAccountInfo_appServerPrivateKey' - The Sidewalk application server private key.
--
-- 'amazonId', 'sidewalkAccountInfo_amazonId' - The Sidewalk Amazon ID.
newSidewalkAccountInfo ::
  SidewalkAccountInfo
newSidewalkAccountInfo =
  SidewalkAccountInfo'
    { appServerPrivateKey =
        Prelude.Nothing,
      amazonId = Prelude.Nothing
    }

-- | The Sidewalk application server private key.
sidewalkAccountInfo_appServerPrivateKey :: Lens.Lens' SidewalkAccountInfo (Prelude.Maybe Prelude.Text)
sidewalkAccountInfo_appServerPrivateKey = Lens.lens (\SidewalkAccountInfo' {appServerPrivateKey} -> appServerPrivateKey) (\s@SidewalkAccountInfo' {} a -> s {appServerPrivateKey = a} :: SidewalkAccountInfo) Prelude.. Lens.mapping Data._Sensitive

-- | The Sidewalk Amazon ID.
sidewalkAccountInfo_amazonId :: Lens.Lens' SidewalkAccountInfo (Prelude.Maybe Prelude.Text)
sidewalkAccountInfo_amazonId = Lens.lens (\SidewalkAccountInfo' {amazonId} -> amazonId) (\s@SidewalkAccountInfo' {} a -> s {amazonId = a} :: SidewalkAccountInfo)

instance Data.FromJSON SidewalkAccountInfo where
  parseJSON =
    Data.withObject
      "SidewalkAccountInfo"
      ( \x ->
          SidewalkAccountInfo'
            Prelude.<$> (x Data..:? "AppServerPrivateKey")
            Prelude.<*> (x Data..:? "AmazonId")
      )

instance Prelude.Hashable SidewalkAccountInfo where
  hashWithSalt _salt SidewalkAccountInfo' {..} =
    _salt `Prelude.hashWithSalt` appServerPrivateKey
      `Prelude.hashWithSalt` amazonId

instance Prelude.NFData SidewalkAccountInfo where
  rnf SidewalkAccountInfo' {..} =
    Prelude.rnf appServerPrivateKey
      `Prelude.seq` Prelude.rnf amazonId

instance Data.ToJSON SidewalkAccountInfo where
  toJSON SidewalkAccountInfo' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AppServerPrivateKey" Data..=)
              Prelude.<$> appServerPrivateKey,
            ("AmazonId" Data..=) Prelude.<$> amazonId
          ]
      )
