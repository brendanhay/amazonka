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
-- Module      : Amazonka.Kafka.Types.NodeExporter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.NodeExporter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Indicates whether you want to turn on or turn off the Node Exporter.
--
-- /See:/ 'newNodeExporter' smart constructor.
data NodeExporter = NodeExporter'
  { -- | Indicates whether you want to turn on or turn off the Node Exporter.
    enabledInBroker :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NodeExporter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabledInBroker', 'nodeExporter_enabledInBroker' - Indicates whether you want to turn on or turn off the Node Exporter.
newNodeExporter ::
  -- | 'enabledInBroker'
  Prelude.Bool ->
  NodeExporter
newNodeExporter pEnabledInBroker_ =
  NodeExporter' {enabledInBroker = pEnabledInBroker_}

-- | Indicates whether you want to turn on or turn off the Node Exporter.
nodeExporter_enabledInBroker :: Lens.Lens' NodeExporter Prelude.Bool
nodeExporter_enabledInBroker = Lens.lens (\NodeExporter' {enabledInBroker} -> enabledInBroker) (\s@NodeExporter' {} a -> s {enabledInBroker = a} :: NodeExporter)

instance Data.FromJSON NodeExporter where
  parseJSON =
    Data.withObject
      "NodeExporter"
      ( \x ->
          NodeExporter'
            Prelude.<$> (x Data..: "enabledInBroker")
      )

instance Prelude.Hashable NodeExporter where
  hashWithSalt _salt NodeExporter' {..} =
    _salt `Prelude.hashWithSalt` enabledInBroker

instance Prelude.NFData NodeExporter where
  rnf NodeExporter' {..} = Prelude.rnf enabledInBroker
