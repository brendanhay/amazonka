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
-- Module      : Amazonka.MediaLive.Types.DvbNitSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.DvbNitSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | DVB Network Information Table (NIT)
--
-- /See:/ 'newDvbNitSettings' smart constructor.
data DvbNitSettings = DvbNitSettings'
  { -- | The number of milliseconds between instances of this table in the output
    -- transport stream.
    repInterval :: Prelude.Maybe Prelude.Natural,
    -- | The network name text placed in the networkNameDescriptor inside the
    -- Network Information Table. Maximum length is 256 characters.
    networkName :: Prelude.Text,
    -- | The numeric value placed in the Network Information Table (NIT).
    networkId :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DvbNitSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repInterval', 'dvbNitSettings_repInterval' - The number of milliseconds between instances of this table in the output
-- transport stream.
--
-- 'networkName', 'dvbNitSettings_networkName' - The network name text placed in the networkNameDescriptor inside the
-- Network Information Table. Maximum length is 256 characters.
--
-- 'networkId', 'dvbNitSettings_networkId' - The numeric value placed in the Network Information Table (NIT).
newDvbNitSettings ::
  -- | 'networkName'
  Prelude.Text ->
  -- | 'networkId'
  Prelude.Natural ->
  DvbNitSettings
newDvbNitSettings pNetworkName_ pNetworkId_ =
  DvbNitSettings'
    { repInterval = Prelude.Nothing,
      networkName = pNetworkName_,
      networkId = pNetworkId_
    }

-- | The number of milliseconds between instances of this table in the output
-- transport stream.
dvbNitSettings_repInterval :: Lens.Lens' DvbNitSettings (Prelude.Maybe Prelude.Natural)
dvbNitSettings_repInterval = Lens.lens (\DvbNitSettings' {repInterval} -> repInterval) (\s@DvbNitSettings' {} a -> s {repInterval = a} :: DvbNitSettings)

-- | The network name text placed in the networkNameDescriptor inside the
-- Network Information Table. Maximum length is 256 characters.
dvbNitSettings_networkName :: Lens.Lens' DvbNitSettings Prelude.Text
dvbNitSettings_networkName = Lens.lens (\DvbNitSettings' {networkName} -> networkName) (\s@DvbNitSettings' {} a -> s {networkName = a} :: DvbNitSettings)

-- | The numeric value placed in the Network Information Table (NIT).
dvbNitSettings_networkId :: Lens.Lens' DvbNitSettings Prelude.Natural
dvbNitSettings_networkId = Lens.lens (\DvbNitSettings' {networkId} -> networkId) (\s@DvbNitSettings' {} a -> s {networkId = a} :: DvbNitSettings)

instance Data.FromJSON DvbNitSettings where
  parseJSON =
    Data.withObject
      "DvbNitSettings"
      ( \x ->
          DvbNitSettings'
            Prelude.<$> (x Data..:? "repInterval")
            Prelude.<*> (x Data..: "networkName")
            Prelude.<*> (x Data..: "networkId")
      )

instance Prelude.Hashable DvbNitSettings where
  hashWithSalt _salt DvbNitSettings' {..} =
    _salt `Prelude.hashWithSalt` repInterval
      `Prelude.hashWithSalt` networkName
      `Prelude.hashWithSalt` networkId

instance Prelude.NFData DvbNitSettings where
  rnf DvbNitSettings' {..} =
    Prelude.rnf repInterval
      `Prelude.seq` Prelude.rnf networkName
      `Prelude.seq` Prelude.rnf networkId

instance Data.ToJSON DvbNitSettings where
  toJSON DvbNitSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("repInterval" Data..=) Prelude.<$> repInterval,
            Prelude.Just ("networkName" Data..= networkName),
            Prelude.Just ("networkId" Data..= networkId)
          ]
      )
