{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the properties of a replica.
--
-- /See:/ 'newReplica' smart constructor.
data Replica = Replica'
  { -- | The Region where the replica needs to be created.
    regionName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
newReplica = Replica' {regionName = Prelude.Nothing}

-- | The Region where the replica needs to be created.
replica_regionName :: Lens.Lens' Replica (Prelude.Maybe Prelude.Text)
replica_regionName = Lens.lens (\Replica' {regionName} -> regionName) (\s@Replica' {} a -> s {regionName = a} :: Replica)

instance Prelude.FromJSON Replica where
  parseJSON =
    Prelude.withObject
      "Replica"
      ( \x ->
          Replica' Prelude.<$> (x Prelude..:? "RegionName")
      )

instance Prelude.Hashable Replica

instance Prelude.NFData Replica

instance Prelude.ToJSON Replica where
  toJSON Replica' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("RegionName" Prelude..=) Prelude.<$> regionName]
      )
