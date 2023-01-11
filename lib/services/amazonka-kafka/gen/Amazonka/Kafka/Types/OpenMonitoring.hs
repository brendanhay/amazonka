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
-- Module      : Amazonka.Kafka.Types.OpenMonitoring
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.OpenMonitoring where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types.Prometheus
import qualified Amazonka.Prelude as Prelude

-- | JMX and Node monitoring for the MSK cluster.
--
-- /See:/ 'newOpenMonitoring' smart constructor.
data OpenMonitoring = OpenMonitoring'
  { -- | Prometheus settings.
    prometheus :: Prometheus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OpenMonitoring' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prometheus', 'openMonitoring_prometheus' - Prometheus settings.
newOpenMonitoring ::
  -- | 'prometheus'
  Prometheus ->
  OpenMonitoring
newOpenMonitoring pPrometheus_ =
  OpenMonitoring' {prometheus = pPrometheus_}

-- | Prometheus settings.
openMonitoring_prometheus :: Lens.Lens' OpenMonitoring Prometheus
openMonitoring_prometheus = Lens.lens (\OpenMonitoring' {prometheus} -> prometheus) (\s@OpenMonitoring' {} a -> s {prometheus = a} :: OpenMonitoring)

instance Data.FromJSON OpenMonitoring where
  parseJSON =
    Data.withObject
      "OpenMonitoring"
      ( \x ->
          OpenMonitoring' Prelude.<$> (x Data..: "prometheus")
      )

instance Prelude.Hashable OpenMonitoring where
  hashWithSalt _salt OpenMonitoring' {..} =
    _salt `Prelude.hashWithSalt` prometheus

instance Prelude.NFData OpenMonitoring where
  rnf OpenMonitoring' {..} = Prelude.rnf prometheus
