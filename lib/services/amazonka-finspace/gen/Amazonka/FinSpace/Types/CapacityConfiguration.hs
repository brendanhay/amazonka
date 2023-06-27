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
-- Module      : Amazonka.FinSpace.Types.CapacityConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpace.Types.CapacityConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure for the metadata of a cluster. It includes information like
-- the CPUs needed, memory of instances, number of instances, and the port
-- used while establishing a connection.
--
-- /See:/ 'newCapacityConfiguration' smart constructor.
data CapacityConfiguration = CapacityConfiguration'
  { -- | The number of instances running in a cluster.
    nodeCount :: Prelude.Maybe Prelude.Natural,
    -- | The type that determines the hardware of the host computer used for your
    -- cluster instance. Each node type offers different memory and storage
    -- capabilities. Choose a node type based on the requirements of the
    -- application or software that you plan to run on your instance.
    --
    -- You can only specify one of the following values:
    --
    -- -   @kx.s.large@ – The node type with a configuration of 12 GiB memory
    --     and 2 vCPUs.
    --
    -- -   @kx.s.xlarge@ – The node type with a configuration of 27 GiB memory
    --     and 4 vCPUs.
    --
    -- -   @kx.s.2xlarge@ – The node type with a configuration of 54 GiB memory
    --     and 8 vCPUs.
    --
    -- -   @kx.s.4xlarge@ – The node type with a configuration of 108 GiB
    --     memory and 16 vCPUs.
    --
    -- -   @kx.s.8xlarge@ – The node type with a configuration of 216 GiB
    --     memory and 32 vCPUs.
    --
    -- -   @kx.s.16xlarge@ – The node type with a configuration of 432 GiB
    --     memory and 64 vCPUs.
    --
    -- -   @kx.s.32xlarge@ – The node type with a configuration of 864 GiB
    --     memory and 128 vCPUs.
    nodeType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CapacityConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nodeCount', 'capacityConfiguration_nodeCount' - The number of instances running in a cluster.
--
-- 'nodeType', 'capacityConfiguration_nodeType' - The type that determines the hardware of the host computer used for your
-- cluster instance. Each node type offers different memory and storage
-- capabilities. Choose a node type based on the requirements of the
-- application or software that you plan to run on your instance.
--
-- You can only specify one of the following values:
--
-- -   @kx.s.large@ – The node type with a configuration of 12 GiB memory
--     and 2 vCPUs.
--
-- -   @kx.s.xlarge@ – The node type with a configuration of 27 GiB memory
--     and 4 vCPUs.
--
-- -   @kx.s.2xlarge@ – The node type with a configuration of 54 GiB memory
--     and 8 vCPUs.
--
-- -   @kx.s.4xlarge@ – The node type with a configuration of 108 GiB
--     memory and 16 vCPUs.
--
-- -   @kx.s.8xlarge@ – The node type with a configuration of 216 GiB
--     memory and 32 vCPUs.
--
-- -   @kx.s.16xlarge@ – The node type with a configuration of 432 GiB
--     memory and 64 vCPUs.
--
-- -   @kx.s.32xlarge@ – The node type with a configuration of 864 GiB
--     memory and 128 vCPUs.
newCapacityConfiguration ::
  CapacityConfiguration
newCapacityConfiguration =
  CapacityConfiguration'
    { nodeCount = Prelude.Nothing,
      nodeType = Prelude.Nothing
    }

-- | The number of instances running in a cluster.
capacityConfiguration_nodeCount :: Lens.Lens' CapacityConfiguration (Prelude.Maybe Prelude.Natural)
capacityConfiguration_nodeCount = Lens.lens (\CapacityConfiguration' {nodeCount} -> nodeCount) (\s@CapacityConfiguration' {} a -> s {nodeCount = a} :: CapacityConfiguration)

-- | The type that determines the hardware of the host computer used for your
-- cluster instance. Each node type offers different memory and storage
-- capabilities. Choose a node type based on the requirements of the
-- application or software that you plan to run on your instance.
--
-- You can only specify one of the following values:
--
-- -   @kx.s.large@ – The node type with a configuration of 12 GiB memory
--     and 2 vCPUs.
--
-- -   @kx.s.xlarge@ – The node type with a configuration of 27 GiB memory
--     and 4 vCPUs.
--
-- -   @kx.s.2xlarge@ – The node type with a configuration of 54 GiB memory
--     and 8 vCPUs.
--
-- -   @kx.s.4xlarge@ – The node type with a configuration of 108 GiB
--     memory and 16 vCPUs.
--
-- -   @kx.s.8xlarge@ – The node type with a configuration of 216 GiB
--     memory and 32 vCPUs.
--
-- -   @kx.s.16xlarge@ – The node type with a configuration of 432 GiB
--     memory and 64 vCPUs.
--
-- -   @kx.s.32xlarge@ – The node type with a configuration of 864 GiB
--     memory and 128 vCPUs.
capacityConfiguration_nodeType :: Lens.Lens' CapacityConfiguration (Prelude.Maybe Prelude.Text)
capacityConfiguration_nodeType = Lens.lens (\CapacityConfiguration' {nodeType} -> nodeType) (\s@CapacityConfiguration' {} a -> s {nodeType = a} :: CapacityConfiguration)

instance Data.FromJSON CapacityConfiguration where
  parseJSON =
    Data.withObject
      "CapacityConfiguration"
      ( \x ->
          CapacityConfiguration'
            Prelude.<$> (x Data..:? "nodeCount")
            Prelude.<*> (x Data..:? "nodeType")
      )

instance Prelude.Hashable CapacityConfiguration where
  hashWithSalt _salt CapacityConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` nodeCount
      `Prelude.hashWithSalt` nodeType

instance Prelude.NFData CapacityConfiguration where
  rnf CapacityConfiguration' {..} =
    Prelude.rnf nodeCount
      `Prelude.seq` Prelude.rnf nodeType

instance Data.ToJSON CapacityConfiguration where
  toJSON CapacityConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nodeCount" Data..=) Prelude.<$> nodeCount,
            ("nodeType" Data..=) Prelude.<$> nodeType
          ]
      )
