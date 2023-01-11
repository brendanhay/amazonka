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
-- Module      : Amazonka.Kafka.Types.PrometheusInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.PrometheusInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types.JmxExporterInfo
import Amazonka.Kafka.Types.NodeExporterInfo
import qualified Amazonka.Prelude as Prelude

-- | Prometheus settings.
--
-- /See:/ 'newPrometheusInfo' smart constructor.
data PrometheusInfo = PrometheusInfo'
  { -- | Indicates whether you want to turn on or turn off the JMX Exporter.
    jmxExporter :: Prelude.Maybe JmxExporterInfo,
    -- | Indicates whether you want to turn on or turn off the Node Exporter.
    nodeExporter :: Prelude.Maybe NodeExporterInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PrometheusInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jmxExporter', 'prometheusInfo_jmxExporter' - Indicates whether you want to turn on or turn off the JMX Exporter.
--
-- 'nodeExporter', 'prometheusInfo_nodeExporter' - Indicates whether you want to turn on or turn off the Node Exporter.
newPrometheusInfo ::
  PrometheusInfo
newPrometheusInfo =
  PrometheusInfo'
    { jmxExporter = Prelude.Nothing,
      nodeExporter = Prelude.Nothing
    }

-- | Indicates whether you want to turn on or turn off the JMX Exporter.
prometheusInfo_jmxExporter :: Lens.Lens' PrometheusInfo (Prelude.Maybe JmxExporterInfo)
prometheusInfo_jmxExporter = Lens.lens (\PrometheusInfo' {jmxExporter} -> jmxExporter) (\s@PrometheusInfo' {} a -> s {jmxExporter = a} :: PrometheusInfo)

-- | Indicates whether you want to turn on or turn off the Node Exporter.
prometheusInfo_nodeExporter :: Lens.Lens' PrometheusInfo (Prelude.Maybe NodeExporterInfo)
prometheusInfo_nodeExporter = Lens.lens (\PrometheusInfo' {nodeExporter} -> nodeExporter) (\s@PrometheusInfo' {} a -> s {nodeExporter = a} :: PrometheusInfo)

instance Data.FromJSON PrometheusInfo where
  parseJSON =
    Data.withObject
      "PrometheusInfo"
      ( \x ->
          PrometheusInfo'
            Prelude.<$> (x Data..:? "jmxExporter")
            Prelude.<*> (x Data..:? "nodeExporter")
      )

instance Prelude.Hashable PrometheusInfo where
  hashWithSalt _salt PrometheusInfo' {..} =
    _salt `Prelude.hashWithSalt` jmxExporter
      `Prelude.hashWithSalt` nodeExporter

instance Prelude.NFData PrometheusInfo where
  rnf PrometheusInfo' {..} =
    Prelude.rnf jmxExporter
      `Prelude.seq` Prelude.rnf nodeExporter

instance Data.ToJSON PrometheusInfo where
  toJSON PrometheusInfo' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("jmxExporter" Data..=) Prelude.<$> jmxExporter,
            ("nodeExporter" Data..=) Prelude.<$> nodeExporter
          ]
      )
