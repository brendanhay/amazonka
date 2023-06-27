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
-- Module      : Amazonka.KafkaConnect.Types.Capacity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.Capacity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KafkaConnect.Types.AutoScaling
import Amazonka.KafkaConnect.Types.ProvisionedCapacity
import qualified Amazonka.Prelude as Prelude

-- | Information about the capacity of the connector, whether it is auto
-- scaled or provisioned.
--
-- /See:/ 'newCapacity' smart constructor.
data Capacity = Capacity'
  { -- | Information about the auto scaling parameters for the connector.
    autoScaling :: Prelude.Maybe AutoScaling,
    -- | Details about a fixed capacity allocated to a connector.
    provisionedCapacity :: Prelude.Maybe ProvisionedCapacity
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Capacity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScaling', 'capacity_autoScaling' - Information about the auto scaling parameters for the connector.
--
-- 'provisionedCapacity', 'capacity_provisionedCapacity' - Details about a fixed capacity allocated to a connector.
newCapacity ::
  Capacity
newCapacity =
  Capacity'
    { autoScaling = Prelude.Nothing,
      provisionedCapacity = Prelude.Nothing
    }

-- | Information about the auto scaling parameters for the connector.
capacity_autoScaling :: Lens.Lens' Capacity (Prelude.Maybe AutoScaling)
capacity_autoScaling = Lens.lens (\Capacity' {autoScaling} -> autoScaling) (\s@Capacity' {} a -> s {autoScaling = a} :: Capacity)

-- | Details about a fixed capacity allocated to a connector.
capacity_provisionedCapacity :: Lens.Lens' Capacity (Prelude.Maybe ProvisionedCapacity)
capacity_provisionedCapacity = Lens.lens (\Capacity' {provisionedCapacity} -> provisionedCapacity) (\s@Capacity' {} a -> s {provisionedCapacity = a} :: Capacity)

instance Prelude.Hashable Capacity where
  hashWithSalt _salt Capacity' {..} =
    _salt
      `Prelude.hashWithSalt` autoScaling
      `Prelude.hashWithSalt` provisionedCapacity

instance Prelude.NFData Capacity where
  rnf Capacity' {..} =
    Prelude.rnf autoScaling
      `Prelude.seq` Prelude.rnf provisionedCapacity

instance Data.ToJSON Capacity where
  toJSON Capacity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("autoScaling" Data..=) Prelude.<$> autoScaling,
            ("provisionedCapacity" Data..=)
              Prelude.<$> provisionedCapacity
          ]
      )
