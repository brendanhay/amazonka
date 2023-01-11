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
-- Module      : Amazonka.IoTWireless.Types.MessageDeliveryStatusResourceTypeEventConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.MessageDeliveryStatusResourceTypeEventConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.SidewalkResourceTypeEventConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Message delivery status resource type event configuration object for
-- enabling or disabling relevant topic.
--
-- /See:/ 'newMessageDeliveryStatusResourceTypeEventConfiguration' smart constructor.
data MessageDeliveryStatusResourceTypeEventConfiguration = MessageDeliveryStatusResourceTypeEventConfiguration'
  { sidewalk :: Prelude.Maybe SidewalkResourceTypeEventConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MessageDeliveryStatusResourceTypeEventConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sidewalk', 'messageDeliveryStatusResourceTypeEventConfiguration_sidewalk' - Undocumented member.
newMessageDeliveryStatusResourceTypeEventConfiguration ::
  MessageDeliveryStatusResourceTypeEventConfiguration
newMessageDeliveryStatusResourceTypeEventConfiguration =
  MessageDeliveryStatusResourceTypeEventConfiguration'
    { sidewalk =
        Prelude.Nothing
    }

-- | Undocumented member.
messageDeliveryStatusResourceTypeEventConfiguration_sidewalk :: Lens.Lens' MessageDeliveryStatusResourceTypeEventConfiguration (Prelude.Maybe SidewalkResourceTypeEventConfiguration)
messageDeliveryStatusResourceTypeEventConfiguration_sidewalk = Lens.lens (\MessageDeliveryStatusResourceTypeEventConfiguration' {sidewalk} -> sidewalk) (\s@MessageDeliveryStatusResourceTypeEventConfiguration' {} a -> s {sidewalk = a} :: MessageDeliveryStatusResourceTypeEventConfiguration)

instance
  Data.FromJSON
    MessageDeliveryStatusResourceTypeEventConfiguration
  where
  parseJSON =
    Data.withObject
      "MessageDeliveryStatusResourceTypeEventConfiguration"
      ( \x ->
          MessageDeliveryStatusResourceTypeEventConfiguration'
            Prelude.<$> (x Data..:? "Sidewalk")
      )

instance
  Prelude.Hashable
    MessageDeliveryStatusResourceTypeEventConfiguration
  where
  hashWithSalt
    _salt
    MessageDeliveryStatusResourceTypeEventConfiguration' {..} =
      _salt `Prelude.hashWithSalt` sidewalk

instance
  Prelude.NFData
    MessageDeliveryStatusResourceTypeEventConfiguration
  where
  rnf
    MessageDeliveryStatusResourceTypeEventConfiguration' {..} =
      Prelude.rnf sidewalk

instance
  Data.ToJSON
    MessageDeliveryStatusResourceTypeEventConfiguration
  where
  toJSON
    MessageDeliveryStatusResourceTypeEventConfiguration' {..} =
      Data.object
        ( Prelude.catMaybes
            [("Sidewalk" Data..=) Prelude.<$> sidewalk]
        )
