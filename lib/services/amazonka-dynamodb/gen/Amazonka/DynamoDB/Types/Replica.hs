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
-- Module      : Amazonka.DynamoDB.Types.Replica
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.Replica where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents the properties of a replica.
--
-- /See:/ 'newReplica' smart constructor.
data Replica = Replica'
  { -- | The Region where the replica needs to be created.
    regionName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON Replica where
  parseJSON =
    Data.withObject
      "Replica"
      ( \x ->
          Replica' Prelude.<$> (x Data..:? "RegionName")
      )

instance Prelude.Hashable Replica where
  hashWithSalt _salt Replica' {..} =
    _salt `Prelude.hashWithSalt` regionName

instance Prelude.NFData Replica where
  rnf Replica' {..} = Prelude.rnf regionName

instance Data.ToJSON Replica where
  toJSON Replica' {..} =
    Data.object
      ( Prelude.catMaybes
          [("RegionName" Data..=) Prelude.<$> regionName]
      )
