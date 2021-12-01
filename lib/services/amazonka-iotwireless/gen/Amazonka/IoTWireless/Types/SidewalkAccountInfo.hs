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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.SidewalkAccountInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a Sidewalk account.
--
-- /See:/ 'newSidewalkAccountInfo' smart constructor.
data SidewalkAccountInfo = SidewalkAccountInfo'
  { -- | The Sidewalk Amazon ID.
    amazonId :: Prelude.Maybe Prelude.Text,
    -- | The Sidewalk application server private key.
    appServerPrivateKey :: Prelude.Maybe (Core.Sensitive Prelude.Text)
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
sidewalkAccountInfo_appServerPrivateKey = Lens.lens (\SidewalkAccountInfo' {appServerPrivateKey} -> appServerPrivateKey) (\s@SidewalkAccountInfo' {} a -> s {appServerPrivateKey = a} :: SidewalkAccountInfo) Prelude.. Lens.mapping Core._Sensitive

instance Core.FromJSON SidewalkAccountInfo where
  parseJSON =
    Core.withObject
      "SidewalkAccountInfo"
      ( \x ->
          SidewalkAccountInfo'
            Prelude.<$> (x Core..:? "AmazonId")
            Prelude.<*> (x Core..:? "AppServerPrivateKey")
      )

instance Prelude.Hashable SidewalkAccountInfo where
  hashWithSalt salt' SidewalkAccountInfo' {..} =
    salt' `Prelude.hashWithSalt` appServerPrivateKey
      `Prelude.hashWithSalt` amazonId

instance Prelude.NFData SidewalkAccountInfo where
  rnf SidewalkAccountInfo' {..} =
    Prelude.rnf amazonId
      `Prelude.seq` Prelude.rnf appServerPrivateKey

instance Core.ToJSON SidewalkAccountInfo where
  toJSON SidewalkAccountInfo' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AmazonId" Core..=) Prelude.<$> amazonId,
            ("AppServerPrivateKey" Core..=)
              Prelude.<$> appServerPrivateKey
          ]
      )
