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
-- Module      : Network.AWS.Kafka.Types.PrometheusInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kafka.Types.PrometheusInfo where

import qualified Network.AWS.Core as Core
import Network.AWS.Kafka.Types.JmxExporterInfo
import Network.AWS.Kafka.Types.NodeExporterInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Prometheus settings.
--
-- /See:/ 'newPrometheusInfo' smart constructor.
data PrometheusInfo = PrometheusInfo'
  { -- | Indicates whether you want to enable or disable the JMX Exporter.
    jmxExporter :: Prelude.Maybe JmxExporterInfo,
    -- | Indicates whether you want to enable or disable the Node Exporter.
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
-- 'jmxExporter', 'prometheusInfo_jmxExporter' - Indicates whether you want to enable or disable the JMX Exporter.
--
-- 'nodeExporter', 'prometheusInfo_nodeExporter' - Indicates whether you want to enable or disable the Node Exporter.
newPrometheusInfo ::
  PrometheusInfo
newPrometheusInfo =
  PrometheusInfo'
    { jmxExporter = Prelude.Nothing,
      nodeExporter = Prelude.Nothing
    }

-- | Indicates whether you want to enable or disable the JMX Exporter.
prometheusInfo_jmxExporter :: Lens.Lens' PrometheusInfo (Prelude.Maybe JmxExporterInfo)
prometheusInfo_jmxExporter = Lens.lens (\PrometheusInfo' {jmxExporter} -> jmxExporter) (\s@PrometheusInfo' {} a -> s {jmxExporter = a} :: PrometheusInfo)

-- | Indicates whether you want to enable or disable the Node Exporter.
prometheusInfo_nodeExporter :: Lens.Lens' PrometheusInfo (Prelude.Maybe NodeExporterInfo)
prometheusInfo_nodeExporter = Lens.lens (\PrometheusInfo' {nodeExporter} -> nodeExporter) (\s@PrometheusInfo' {} a -> s {nodeExporter = a} :: PrometheusInfo)

instance Prelude.Hashable PrometheusInfo

instance Prelude.NFData PrometheusInfo

instance Core.ToJSON PrometheusInfo where
  toJSON PrometheusInfo' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("jmxExporter" Core..=) Prelude.<$> jmxExporter,
            ("nodeExporter" Core..=) Prelude.<$> nodeExporter
          ]
      )
