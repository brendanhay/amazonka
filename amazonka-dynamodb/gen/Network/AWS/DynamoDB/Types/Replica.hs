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
-- Module      : Network.AWS.DynamoDB.Types.Replica
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.Replica where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents the properties of a replica.
--
-- /See:/ 'newReplica' smart constructor.
data Replica = Replica'
  { -- | The Region where the replica needs to be created.
    regionName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Replica' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regionName', 'replica_regionName' - The Region where the replica needs to be created.
newReplica ::
  Replica
newReplica = Replica' {regionName = Core.Nothing}

-- | The Region where the replica needs to be created.
replica_regionName :: Lens.Lens' Replica (Core.Maybe Core.Text)
replica_regionName = Lens.lens (\Replica' {regionName} -> regionName) (\s@Replica' {} a -> s {regionName = a} :: Replica)

instance Core.FromJSON Replica where
  parseJSON =
    Core.withObject
      "Replica"
      (\x -> Replica' Core.<$> (x Core..:? "RegionName"))

instance Core.Hashable Replica

instance Core.NFData Replica

instance Core.ToJSON Replica where
  toJSON Replica' {..} =
    Core.object
      ( Core.catMaybes
          [("RegionName" Core..=) Core.<$> regionName]
      )
