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
-- Module      : Amazonka.Redshift.Types.OrderableClusterOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.OrderableClusterOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.AvailabilityZone

-- | Describes an orderable cluster option.
--
-- /See:/ 'newOrderableClusterOption' smart constructor.
data OrderableClusterOption = OrderableClusterOption'
  { -- | A list of availability zones for the orderable cluster.
    availabilityZones :: Prelude.Maybe [AvailabilityZone],
    -- | The cluster type, for example @multi-node@.
    clusterType :: Prelude.Maybe Prelude.Text,
    -- | The version of the orderable cluster.
    clusterVersion :: Prelude.Maybe Prelude.Text,
    -- | The node type for the orderable cluster.
    nodeType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrderableClusterOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZones', 'orderableClusterOption_availabilityZones' - A list of availability zones for the orderable cluster.
--
-- 'clusterType', 'orderableClusterOption_clusterType' - The cluster type, for example @multi-node@.
--
-- 'clusterVersion', 'orderableClusterOption_clusterVersion' - The version of the orderable cluster.
--
-- 'nodeType', 'orderableClusterOption_nodeType' - The node type for the orderable cluster.
newOrderableClusterOption ::
  OrderableClusterOption
newOrderableClusterOption =
  OrderableClusterOption'
    { availabilityZones =
        Prelude.Nothing,
      clusterType = Prelude.Nothing,
      clusterVersion = Prelude.Nothing,
      nodeType = Prelude.Nothing
    }

-- | A list of availability zones for the orderable cluster.
orderableClusterOption_availabilityZones :: Lens.Lens' OrderableClusterOption (Prelude.Maybe [AvailabilityZone])
orderableClusterOption_availabilityZones = Lens.lens (\OrderableClusterOption' {availabilityZones} -> availabilityZones) (\s@OrderableClusterOption' {} a -> s {availabilityZones = a} :: OrderableClusterOption) Prelude.. Lens.mapping Lens.coerced

-- | The cluster type, for example @multi-node@.
orderableClusterOption_clusterType :: Lens.Lens' OrderableClusterOption (Prelude.Maybe Prelude.Text)
orderableClusterOption_clusterType = Lens.lens (\OrderableClusterOption' {clusterType} -> clusterType) (\s@OrderableClusterOption' {} a -> s {clusterType = a} :: OrderableClusterOption)

-- | The version of the orderable cluster.
orderableClusterOption_clusterVersion :: Lens.Lens' OrderableClusterOption (Prelude.Maybe Prelude.Text)
orderableClusterOption_clusterVersion = Lens.lens (\OrderableClusterOption' {clusterVersion} -> clusterVersion) (\s@OrderableClusterOption' {} a -> s {clusterVersion = a} :: OrderableClusterOption)

-- | The node type for the orderable cluster.
orderableClusterOption_nodeType :: Lens.Lens' OrderableClusterOption (Prelude.Maybe Prelude.Text)
orderableClusterOption_nodeType = Lens.lens (\OrderableClusterOption' {nodeType} -> nodeType) (\s@OrderableClusterOption' {} a -> s {nodeType = a} :: OrderableClusterOption)

instance Data.FromXML OrderableClusterOption where
  parseXML x =
    OrderableClusterOption'
      Prelude.<$> ( x
                      Data..@? "AvailabilityZones"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "AvailabilityZone")
                  )
      Prelude.<*> (x Data..@? "ClusterType")
      Prelude.<*> (x Data..@? "ClusterVersion")
      Prelude.<*> (x Data..@? "NodeType")

instance Prelude.Hashable OrderableClusterOption where
  hashWithSalt _salt OrderableClusterOption' {..} =
    _salt
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` clusterType
      `Prelude.hashWithSalt` clusterVersion
      `Prelude.hashWithSalt` nodeType

instance Prelude.NFData OrderableClusterOption where
  rnf OrderableClusterOption' {..} =
    Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf clusterType
      `Prelude.seq` Prelude.rnf clusterVersion
      `Prelude.seq` Prelude.rnf nodeType
