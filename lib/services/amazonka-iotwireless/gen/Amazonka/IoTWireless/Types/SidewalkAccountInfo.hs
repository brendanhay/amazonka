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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
  { -- | The Sidewalk Amazon ID.
    amazonId :: Prelude.Maybe Prelude.Text,
    -- | The Sidewalk application server private key.
    appServerPrivateKey :: Prelude.Maybe (Data.Sensitive Prelude.Text)
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
-- 'amazonId', 'sidewalkAccountInfo_amazonId' - The Sidewalk Amazon ID.
--
-- 'appServerPrivateKey', 'sidewalkAccountInfo_appServerPrivateKey' - The Sidewalk application server private key.
newSidewalkAccountInfo ::
  SidewalkAccountInfo
newSidewalkAccountInfo =
  SidewalkAccountInfo'
    { amazonId = Prelude.Nothing,
      appServerPrivateKey = Prelude.Nothing
    }

-- | The Sidewalk Amazon ID.
sidewalkAccountInfo_amazonId :: Lens.Lens' SidewalkAccountInfo (Prelude.Maybe Prelude.Text)
sidewalkAccountInfo_amazonId = Lens.lens (\SidewalkAccountInfo' {amazonId} -> amazonId) (\s@SidewalkAccountInfo' {} a -> s {amazonId = a} :: SidewalkAccountInfo)

-- | The Sidewalk application server private key.
sidewalkAccountInfo_appServerPrivateKey :: Lens.Lens' SidewalkAccountInfo (Prelude.Maybe Prelude.Text)
sidewalkAccountInfo_appServerPrivateKey = Lens.lens (\SidewalkAccountInfo' {appServerPrivateKey} -> appServerPrivateKey) (\s@SidewalkAccountInfo' {} a -> s {appServerPrivateKey = a} :: SidewalkAccountInfo) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON SidewalkAccountInfo where
  parseJSON =
    Data.withObject
      "SidewalkAccountInfo"
      ( \x ->
          SidewalkAccountInfo'
            Prelude.<$> (x Data..:? "AmazonId")
            Prelude.<*> (x Data..:? "AppServerPrivateKey")
      )

instance Prelude.Hashable SidewalkAccountInfo where
  hashWithSalt _salt SidewalkAccountInfo' {..} =
    _salt
      `Prelude.hashWithSalt` amazonId
      `Prelude.hashWithSalt` appServerPrivateKey

instance Prelude.NFData SidewalkAccountInfo where
  rnf SidewalkAccountInfo' {..} =
    Prelude.rnf amazonId `Prelude.seq`
      Prelude.rnf appServerPrivateKey

instance Data.ToJSON SidewalkAccountInfo where
  toJSON SidewalkAccountInfo' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AmazonId" Data..=) Prelude.<$> amazonId,
            ("AppServerPrivateKey" Data..=)
              Prelude.<$> appServerPrivateKey
          ]
      )
