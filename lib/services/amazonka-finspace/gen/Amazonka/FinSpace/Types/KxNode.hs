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
-- Module      : Amazonka.FinSpace.Types.KxNode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpace.Types.KxNode where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that stores metadata for a kdb node.
--
-- /See:/ 'newKxNode' smart constructor.
data KxNode = KxNode'
  { -- | The identifier of the availability zones where subnets for the
    -- environment are created.
    availabilityZoneId :: Prelude.Maybe Prelude.Text,
    -- | The time when a particular node is started. The value is determined as
    -- epoch time in milliseconds. For example, the value for Monday, November
    -- 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
    launchTime :: Prelude.Maybe Data.POSIX,
    -- | A unique identifier for the node.
    nodeId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KxNode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZoneId', 'kxNode_availabilityZoneId' - The identifier of the availability zones where subnets for the
-- environment are created.
--
-- 'launchTime', 'kxNode_launchTime' - The time when a particular node is started. The value is determined as
-- epoch time in milliseconds. For example, the value for Monday, November
-- 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
--
-- 'nodeId', 'kxNode_nodeId' - A unique identifier for the node.
newKxNode ::
  KxNode
newKxNode =
  KxNode'
    { availabilityZoneId = Prelude.Nothing,
      launchTime = Prelude.Nothing,
      nodeId = Prelude.Nothing
    }

-- | The identifier of the availability zones where subnets for the
-- environment are created.
kxNode_availabilityZoneId :: Lens.Lens' KxNode (Prelude.Maybe Prelude.Text)
kxNode_availabilityZoneId = Lens.lens (\KxNode' {availabilityZoneId} -> availabilityZoneId) (\s@KxNode' {} a -> s {availabilityZoneId = a} :: KxNode)

-- | The time when a particular node is started. The value is determined as
-- epoch time in milliseconds. For example, the value for Monday, November
-- 1, 2021 12:00:00 PM UTC is specified as 1635768000000.
kxNode_launchTime :: Lens.Lens' KxNode (Prelude.Maybe Prelude.UTCTime)
kxNode_launchTime = Lens.lens (\KxNode' {launchTime} -> launchTime) (\s@KxNode' {} a -> s {launchTime = a} :: KxNode) Prelude.. Lens.mapping Data._Time

-- | A unique identifier for the node.
kxNode_nodeId :: Lens.Lens' KxNode (Prelude.Maybe Prelude.Text)
kxNode_nodeId = Lens.lens (\KxNode' {nodeId} -> nodeId) (\s@KxNode' {} a -> s {nodeId = a} :: KxNode)

instance Data.FromJSON KxNode where
  parseJSON =
    Data.withObject
      "KxNode"
      ( \x ->
          KxNode'
            Prelude.<$> (x Data..:? "availabilityZoneId")
            Prelude.<*> (x Data..:? "launchTime")
            Prelude.<*> (x Data..:? "nodeId")
      )

instance Prelude.Hashable KxNode where
  hashWithSalt _salt KxNode' {..} =
    _salt
      `Prelude.hashWithSalt` availabilityZoneId
      `Prelude.hashWithSalt` launchTime
      `Prelude.hashWithSalt` nodeId

instance Prelude.NFData KxNode where
  rnf KxNode' {..} =
    Prelude.rnf availabilityZoneId
      `Prelude.seq` Prelude.rnf launchTime
      `Prelude.seq` Prelude.rnf nodeId
