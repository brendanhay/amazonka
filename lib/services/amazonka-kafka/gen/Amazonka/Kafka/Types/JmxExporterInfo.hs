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
-- Module      : Amazonka.Kafka.Types.JmxExporterInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.JmxExporterInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Indicates whether you want to turn on or turn off the JMX Exporter.
--
-- /See:/ 'newJmxExporterInfo' smart constructor.
data JmxExporterInfo = JmxExporterInfo'
  { -- | Indicates whether you want to turn on or turn off the JMX Exporter.
    enabledInBroker :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JmxExporterInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabledInBroker', 'jmxExporterInfo_enabledInBroker' - Indicates whether you want to turn on or turn off the JMX Exporter.
newJmxExporterInfo ::
  -- | 'enabledInBroker'
  Prelude.Bool ->
  JmxExporterInfo
newJmxExporterInfo pEnabledInBroker_ =
  JmxExporterInfo'
    { enabledInBroker =
        pEnabledInBroker_
    }

-- | Indicates whether you want to turn on or turn off the JMX Exporter.
jmxExporterInfo_enabledInBroker :: Lens.Lens' JmxExporterInfo Prelude.Bool
jmxExporterInfo_enabledInBroker = Lens.lens (\JmxExporterInfo' {enabledInBroker} -> enabledInBroker) (\s@JmxExporterInfo' {} a -> s {enabledInBroker = a} :: JmxExporterInfo)

instance Data.FromJSON JmxExporterInfo where
  parseJSON =
    Data.withObject
      "JmxExporterInfo"
      ( \x ->
          JmxExporterInfo'
            Prelude.<$> (x Data..: "enabledInBroker")
      )

instance Prelude.Hashable JmxExporterInfo where
  hashWithSalt _salt JmxExporterInfo' {..} =
    _salt `Prelude.hashWithSalt` enabledInBroker

instance Prelude.NFData JmxExporterInfo where
  rnf JmxExporterInfo' {..} =
    Prelude.rnf enabledInBroker

instance Data.ToJSON JmxExporterInfo where
  toJSON JmxExporterInfo' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("enabledInBroker" Data..= enabledInBroker)
          ]
      )
