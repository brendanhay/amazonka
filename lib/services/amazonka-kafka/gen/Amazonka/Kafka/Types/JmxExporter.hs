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
-- Module      : Amazonka.Kafka.Types.JmxExporter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.JmxExporter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Indicates whether you want to turn on or turn off the JMX Exporter.
--
-- /See:/ 'newJmxExporter' smart constructor.
data JmxExporter = JmxExporter'
  { -- | Indicates whether you want to turn on or turn off the JMX Exporter.
    enabledInBroker :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JmxExporter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabledInBroker', 'jmxExporter_enabledInBroker' - Indicates whether you want to turn on or turn off the JMX Exporter.
newJmxExporter ::
  -- | 'enabledInBroker'
  Prelude.Bool ->
  JmxExporter
newJmxExporter pEnabledInBroker_ =
  JmxExporter' {enabledInBroker = pEnabledInBroker_}

-- | Indicates whether you want to turn on or turn off the JMX Exporter.
jmxExporter_enabledInBroker :: Lens.Lens' JmxExporter Prelude.Bool
jmxExporter_enabledInBroker = Lens.lens (\JmxExporter' {enabledInBroker} -> enabledInBroker) (\s@JmxExporter' {} a -> s {enabledInBroker = a} :: JmxExporter)

instance Data.FromJSON JmxExporter where
  parseJSON =
    Data.withObject
      "JmxExporter"
      ( \x ->
          JmxExporter'
            Prelude.<$> (x Data..: "enabledInBroker")
      )

instance Prelude.Hashable JmxExporter where
  hashWithSalt _salt JmxExporter' {..} =
    _salt `Prelude.hashWithSalt` enabledInBroker

instance Prelude.NFData JmxExporter where
  rnf JmxExporter' {..} = Prelude.rnf enabledInBroker
