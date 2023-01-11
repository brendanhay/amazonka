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
-- Module      : Amazonka.IoTWireless.Types.JoinResourceTypeEventConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.JoinResourceTypeEventConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.LoRaWANJoinResourceTypeEventConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Join resource type event configuration object for enabling or disabling
-- topic.
--
-- /See:/ 'newJoinResourceTypeEventConfiguration' smart constructor.
data JoinResourceTypeEventConfiguration = JoinResourceTypeEventConfiguration'
  { -- | Join resource type event configuration object for enabling or disabling
    -- LoRaWAN related event topics.
    loRaWAN :: Prelude.Maybe LoRaWANJoinResourceTypeEventConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JoinResourceTypeEventConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loRaWAN', 'joinResourceTypeEventConfiguration_loRaWAN' - Join resource type event configuration object for enabling or disabling
-- LoRaWAN related event topics.
newJoinResourceTypeEventConfiguration ::
  JoinResourceTypeEventConfiguration
newJoinResourceTypeEventConfiguration =
  JoinResourceTypeEventConfiguration'
    { loRaWAN =
        Prelude.Nothing
    }

-- | Join resource type event configuration object for enabling or disabling
-- LoRaWAN related event topics.
joinResourceTypeEventConfiguration_loRaWAN :: Lens.Lens' JoinResourceTypeEventConfiguration (Prelude.Maybe LoRaWANJoinResourceTypeEventConfiguration)
joinResourceTypeEventConfiguration_loRaWAN = Lens.lens (\JoinResourceTypeEventConfiguration' {loRaWAN} -> loRaWAN) (\s@JoinResourceTypeEventConfiguration' {} a -> s {loRaWAN = a} :: JoinResourceTypeEventConfiguration)

instance
  Data.FromJSON
    JoinResourceTypeEventConfiguration
  where
  parseJSON =
    Data.withObject
      "JoinResourceTypeEventConfiguration"
      ( \x ->
          JoinResourceTypeEventConfiguration'
            Prelude.<$> (x Data..:? "LoRaWAN")
      )

instance
  Prelude.Hashable
    JoinResourceTypeEventConfiguration
  where
  hashWithSalt
    _salt
    JoinResourceTypeEventConfiguration' {..} =
      _salt `Prelude.hashWithSalt` loRaWAN

instance
  Prelude.NFData
    JoinResourceTypeEventConfiguration
  where
  rnf JoinResourceTypeEventConfiguration' {..} =
    Prelude.rnf loRaWAN

instance
  Data.ToJSON
    JoinResourceTypeEventConfiguration
  where
  toJSON JoinResourceTypeEventConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("LoRaWAN" Data..=) Prelude.<$> loRaWAN]
      )
