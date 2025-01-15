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
-- Module      : Amazonka.Kafka.Types.Prometheus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.Prometheus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types.JmxExporter
import Amazonka.Kafka.Types.NodeExporter
import qualified Amazonka.Prelude as Prelude

-- | Prometheus settings.
--
-- /See:/ 'newPrometheus' smart constructor.
data Prometheus = Prometheus'
  { -- | Indicates whether you want to turn on or turn off the JMX Exporter.
    jmxExporter :: Prelude.Maybe JmxExporter,
    -- | Indicates whether you want to turn on or turn off the Node Exporter.
    nodeExporter :: Prelude.Maybe NodeExporter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Prometheus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jmxExporter', 'prometheus_jmxExporter' - Indicates whether you want to turn on or turn off the JMX Exporter.
--
-- 'nodeExporter', 'prometheus_nodeExporter' - Indicates whether you want to turn on or turn off the Node Exporter.
newPrometheus ::
  Prometheus
newPrometheus =
  Prometheus'
    { jmxExporter = Prelude.Nothing,
      nodeExporter = Prelude.Nothing
    }

-- | Indicates whether you want to turn on or turn off the JMX Exporter.
prometheus_jmxExporter :: Lens.Lens' Prometheus (Prelude.Maybe JmxExporter)
prometheus_jmxExporter = Lens.lens (\Prometheus' {jmxExporter} -> jmxExporter) (\s@Prometheus' {} a -> s {jmxExporter = a} :: Prometheus)

-- | Indicates whether you want to turn on or turn off the Node Exporter.
prometheus_nodeExporter :: Lens.Lens' Prometheus (Prelude.Maybe NodeExporter)
prometheus_nodeExporter = Lens.lens (\Prometheus' {nodeExporter} -> nodeExporter) (\s@Prometheus' {} a -> s {nodeExporter = a} :: Prometheus)

instance Data.FromJSON Prometheus where
  parseJSON =
    Data.withObject
      "Prometheus"
      ( \x ->
          Prometheus'
            Prelude.<$> (x Data..:? "jmxExporter")
            Prelude.<*> (x Data..:? "nodeExporter")
      )

instance Prelude.Hashable Prometheus where
  hashWithSalt _salt Prometheus' {..} =
    _salt
      `Prelude.hashWithSalt` jmxExporter
      `Prelude.hashWithSalt` nodeExporter

instance Prelude.NFData Prometheus where
  rnf Prometheus' {..} =
    Prelude.rnf jmxExporter `Prelude.seq`
      Prelude.rnf nodeExporter
