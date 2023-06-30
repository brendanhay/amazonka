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
-- Module      : Amazonka.Kafka.Types.BrokerSoftwareInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.BrokerSoftwareInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the current software installed on the cluster.
--
-- /See:/ 'newBrokerSoftwareInfo' smart constructor.
data BrokerSoftwareInfo = BrokerSoftwareInfo'
  { -- | The Amazon Resource Name (ARN) of the configuration used for the
    -- cluster. This field isn\'t visible in this preview release.
    configurationArn :: Prelude.Maybe Prelude.Text,
    -- | The revision of the configuration to use. This field isn\'t visible in
    -- this preview release.
    configurationRevision :: Prelude.Maybe Prelude.Integer,
    -- | The version of Apache Kafka.
    kafkaVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BrokerSoftwareInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationArn', 'brokerSoftwareInfo_configurationArn' - The Amazon Resource Name (ARN) of the configuration used for the
-- cluster. This field isn\'t visible in this preview release.
--
-- 'configurationRevision', 'brokerSoftwareInfo_configurationRevision' - The revision of the configuration to use. This field isn\'t visible in
-- this preview release.
--
-- 'kafkaVersion', 'brokerSoftwareInfo_kafkaVersion' - The version of Apache Kafka.
newBrokerSoftwareInfo ::
  BrokerSoftwareInfo
newBrokerSoftwareInfo =
  BrokerSoftwareInfo'
    { configurationArn =
        Prelude.Nothing,
      configurationRevision = Prelude.Nothing,
      kafkaVersion = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the configuration used for the
-- cluster. This field isn\'t visible in this preview release.
brokerSoftwareInfo_configurationArn :: Lens.Lens' BrokerSoftwareInfo (Prelude.Maybe Prelude.Text)
brokerSoftwareInfo_configurationArn = Lens.lens (\BrokerSoftwareInfo' {configurationArn} -> configurationArn) (\s@BrokerSoftwareInfo' {} a -> s {configurationArn = a} :: BrokerSoftwareInfo)

-- | The revision of the configuration to use. This field isn\'t visible in
-- this preview release.
brokerSoftwareInfo_configurationRevision :: Lens.Lens' BrokerSoftwareInfo (Prelude.Maybe Prelude.Integer)
brokerSoftwareInfo_configurationRevision = Lens.lens (\BrokerSoftwareInfo' {configurationRevision} -> configurationRevision) (\s@BrokerSoftwareInfo' {} a -> s {configurationRevision = a} :: BrokerSoftwareInfo)

-- | The version of Apache Kafka.
brokerSoftwareInfo_kafkaVersion :: Lens.Lens' BrokerSoftwareInfo (Prelude.Maybe Prelude.Text)
brokerSoftwareInfo_kafkaVersion = Lens.lens (\BrokerSoftwareInfo' {kafkaVersion} -> kafkaVersion) (\s@BrokerSoftwareInfo' {} a -> s {kafkaVersion = a} :: BrokerSoftwareInfo)

instance Data.FromJSON BrokerSoftwareInfo where
  parseJSON =
    Data.withObject
      "BrokerSoftwareInfo"
      ( \x ->
          BrokerSoftwareInfo'
            Prelude.<$> (x Data..:? "configurationArn")
            Prelude.<*> (x Data..:? "configurationRevision")
            Prelude.<*> (x Data..:? "kafkaVersion")
      )

instance Prelude.Hashable BrokerSoftwareInfo where
  hashWithSalt _salt BrokerSoftwareInfo' {..} =
    _salt
      `Prelude.hashWithSalt` configurationArn
      `Prelude.hashWithSalt` configurationRevision
      `Prelude.hashWithSalt` kafkaVersion

instance Prelude.NFData BrokerSoftwareInfo where
  rnf BrokerSoftwareInfo' {..} =
    Prelude.rnf configurationArn
      `Prelude.seq` Prelude.rnf configurationRevision
      `Prelude.seq` Prelude.rnf kafkaVersion
