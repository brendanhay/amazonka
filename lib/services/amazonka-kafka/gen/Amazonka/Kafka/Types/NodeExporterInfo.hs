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
-- Module      : Amazonka.Kafka.Types.NodeExporterInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.NodeExporterInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Indicates whether you want to turn on or turn off the Node Exporter.
--
-- /See:/ 'newNodeExporterInfo' smart constructor.
data NodeExporterInfo = NodeExporterInfo'
  { -- | Indicates whether you want to turn on or turn off the Node Exporter.
    enabledInBroker :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NodeExporterInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabledInBroker', 'nodeExporterInfo_enabledInBroker' - Indicates whether you want to turn on or turn off the Node Exporter.
newNodeExporterInfo ::
  -- | 'enabledInBroker'
  Prelude.Bool ->
  NodeExporterInfo
newNodeExporterInfo pEnabledInBroker_ =
  NodeExporterInfo'
    { enabledInBroker =
        pEnabledInBroker_
    }

-- | Indicates whether you want to turn on or turn off the Node Exporter.
nodeExporterInfo_enabledInBroker :: Lens.Lens' NodeExporterInfo Prelude.Bool
nodeExporterInfo_enabledInBroker = Lens.lens (\NodeExporterInfo' {enabledInBroker} -> enabledInBroker) (\s@NodeExporterInfo' {} a -> s {enabledInBroker = a} :: NodeExporterInfo)

instance Core.FromJSON NodeExporterInfo where
  parseJSON =
    Core.withObject
      "NodeExporterInfo"
      ( \x ->
          NodeExporterInfo'
            Prelude.<$> (x Core..: "enabledInBroker")
      )

instance Prelude.Hashable NodeExporterInfo where
  hashWithSalt _salt NodeExporterInfo' {..} =
    _salt `Prelude.hashWithSalt` enabledInBroker

instance Prelude.NFData NodeExporterInfo where
  rnf NodeExporterInfo' {..} =
    Prelude.rnf enabledInBroker

instance Core.ToJSON NodeExporterInfo where
  toJSON NodeExporterInfo' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("enabledInBroker" Core..= enabledInBroker)
          ]
      )
