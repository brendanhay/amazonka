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
-- Module      : Amazonka.Kafka.Types.MutableClusterInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.MutableClusterInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types.BrokerEBSVolumeInfo
import Amazonka.Kafka.Types.ClientAuthentication
import Amazonka.Kafka.Types.ConfigurationInfo
import Amazonka.Kafka.Types.ConnectivityInfo
import Amazonka.Kafka.Types.EncryptionInfo
import Amazonka.Kafka.Types.EnhancedMonitoring
import Amazonka.Kafka.Types.LoggingInfo
import Amazonka.Kafka.Types.OpenMonitoring
import Amazonka.Kafka.Types.StorageMode
import qualified Amazonka.Prelude as Prelude

-- | Information about cluster attributes that can be updated via update
-- APIs.
--
-- /See:/ 'newMutableClusterInfo' smart constructor.
data MutableClusterInfo = MutableClusterInfo'
  { -- | Specifies the size of the EBS volume and the ID of the associated
    -- broker.
    brokerEBSVolumeInfo :: Prelude.Maybe [BrokerEBSVolumeInfo],
    -- | Includes all client authentication information.
    clientAuthentication :: Prelude.Maybe ClientAuthentication,
    -- | Information about the changes in the configuration of the brokers.
    configurationInfo :: Prelude.Maybe ConfigurationInfo,
    -- | Information about the broker access configuration.
    connectivityInfo :: Prelude.Maybe ConnectivityInfo,
    -- | Includes all encryption-related information.
    encryptionInfo :: Prelude.Maybe EncryptionInfo,
    -- | Specifies which Apache Kafka metrics Amazon MSK gathers and sends to
    -- Amazon CloudWatch for this cluster.
    enhancedMonitoring :: Prelude.Maybe EnhancedMonitoring,
    -- | Information about the Amazon MSK broker type.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The Apache Kafka version.
    kafkaVersion :: Prelude.Maybe Prelude.Text,
    -- | You can configure your MSK cluster to send broker logs to different
    -- destination types. This is a container for the configuration details
    -- related to broker logs.
    loggingInfo :: Prelude.Maybe LoggingInfo,
    -- | The number of broker nodes in the cluster.
    numberOfBrokerNodes :: Prelude.Maybe Prelude.Int,
    -- | The settings for open monitoring.
    openMonitoring :: Prelude.Maybe OpenMonitoring,
    -- | This controls storage mode for supported storage tiers.
    storageMode :: Prelude.Maybe StorageMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MutableClusterInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'brokerEBSVolumeInfo', 'mutableClusterInfo_brokerEBSVolumeInfo' - Specifies the size of the EBS volume and the ID of the associated
-- broker.
--
-- 'clientAuthentication', 'mutableClusterInfo_clientAuthentication' - Includes all client authentication information.
--
-- 'configurationInfo', 'mutableClusterInfo_configurationInfo' - Information about the changes in the configuration of the brokers.
--
-- 'connectivityInfo', 'mutableClusterInfo_connectivityInfo' - Information about the broker access configuration.
--
-- 'encryptionInfo', 'mutableClusterInfo_encryptionInfo' - Includes all encryption-related information.
--
-- 'enhancedMonitoring', 'mutableClusterInfo_enhancedMonitoring' - Specifies which Apache Kafka metrics Amazon MSK gathers and sends to
-- Amazon CloudWatch for this cluster.
--
-- 'instanceType', 'mutableClusterInfo_instanceType' - Information about the Amazon MSK broker type.
--
-- 'kafkaVersion', 'mutableClusterInfo_kafkaVersion' - The Apache Kafka version.
--
-- 'loggingInfo', 'mutableClusterInfo_loggingInfo' - You can configure your MSK cluster to send broker logs to different
-- destination types. This is a container for the configuration details
-- related to broker logs.
--
-- 'numberOfBrokerNodes', 'mutableClusterInfo_numberOfBrokerNodes' - The number of broker nodes in the cluster.
--
-- 'openMonitoring', 'mutableClusterInfo_openMonitoring' - The settings for open monitoring.
--
-- 'storageMode', 'mutableClusterInfo_storageMode' - This controls storage mode for supported storage tiers.
newMutableClusterInfo ::
  MutableClusterInfo
newMutableClusterInfo =
  MutableClusterInfo'
    { brokerEBSVolumeInfo =
        Prelude.Nothing,
      clientAuthentication = Prelude.Nothing,
      configurationInfo = Prelude.Nothing,
      connectivityInfo = Prelude.Nothing,
      encryptionInfo = Prelude.Nothing,
      enhancedMonitoring = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      kafkaVersion = Prelude.Nothing,
      loggingInfo = Prelude.Nothing,
      numberOfBrokerNodes = Prelude.Nothing,
      openMonitoring = Prelude.Nothing,
      storageMode = Prelude.Nothing
    }

-- | Specifies the size of the EBS volume and the ID of the associated
-- broker.
mutableClusterInfo_brokerEBSVolumeInfo :: Lens.Lens' MutableClusterInfo (Prelude.Maybe [BrokerEBSVolumeInfo])
mutableClusterInfo_brokerEBSVolumeInfo = Lens.lens (\MutableClusterInfo' {brokerEBSVolumeInfo} -> brokerEBSVolumeInfo) (\s@MutableClusterInfo' {} a -> s {brokerEBSVolumeInfo = a} :: MutableClusterInfo) Prelude.. Lens.mapping Lens.coerced

-- | Includes all client authentication information.
mutableClusterInfo_clientAuthentication :: Lens.Lens' MutableClusterInfo (Prelude.Maybe ClientAuthentication)
mutableClusterInfo_clientAuthentication = Lens.lens (\MutableClusterInfo' {clientAuthentication} -> clientAuthentication) (\s@MutableClusterInfo' {} a -> s {clientAuthentication = a} :: MutableClusterInfo)

-- | Information about the changes in the configuration of the brokers.
mutableClusterInfo_configurationInfo :: Lens.Lens' MutableClusterInfo (Prelude.Maybe ConfigurationInfo)
mutableClusterInfo_configurationInfo = Lens.lens (\MutableClusterInfo' {configurationInfo} -> configurationInfo) (\s@MutableClusterInfo' {} a -> s {configurationInfo = a} :: MutableClusterInfo)

-- | Information about the broker access configuration.
mutableClusterInfo_connectivityInfo :: Lens.Lens' MutableClusterInfo (Prelude.Maybe ConnectivityInfo)
mutableClusterInfo_connectivityInfo = Lens.lens (\MutableClusterInfo' {connectivityInfo} -> connectivityInfo) (\s@MutableClusterInfo' {} a -> s {connectivityInfo = a} :: MutableClusterInfo)

-- | Includes all encryption-related information.
mutableClusterInfo_encryptionInfo :: Lens.Lens' MutableClusterInfo (Prelude.Maybe EncryptionInfo)
mutableClusterInfo_encryptionInfo = Lens.lens (\MutableClusterInfo' {encryptionInfo} -> encryptionInfo) (\s@MutableClusterInfo' {} a -> s {encryptionInfo = a} :: MutableClusterInfo)

-- | Specifies which Apache Kafka metrics Amazon MSK gathers and sends to
-- Amazon CloudWatch for this cluster.
mutableClusterInfo_enhancedMonitoring :: Lens.Lens' MutableClusterInfo (Prelude.Maybe EnhancedMonitoring)
mutableClusterInfo_enhancedMonitoring = Lens.lens (\MutableClusterInfo' {enhancedMonitoring} -> enhancedMonitoring) (\s@MutableClusterInfo' {} a -> s {enhancedMonitoring = a} :: MutableClusterInfo)

-- | Information about the Amazon MSK broker type.
mutableClusterInfo_instanceType :: Lens.Lens' MutableClusterInfo (Prelude.Maybe Prelude.Text)
mutableClusterInfo_instanceType = Lens.lens (\MutableClusterInfo' {instanceType} -> instanceType) (\s@MutableClusterInfo' {} a -> s {instanceType = a} :: MutableClusterInfo)

-- | The Apache Kafka version.
mutableClusterInfo_kafkaVersion :: Lens.Lens' MutableClusterInfo (Prelude.Maybe Prelude.Text)
mutableClusterInfo_kafkaVersion = Lens.lens (\MutableClusterInfo' {kafkaVersion} -> kafkaVersion) (\s@MutableClusterInfo' {} a -> s {kafkaVersion = a} :: MutableClusterInfo)

-- | You can configure your MSK cluster to send broker logs to different
-- destination types. This is a container for the configuration details
-- related to broker logs.
mutableClusterInfo_loggingInfo :: Lens.Lens' MutableClusterInfo (Prelude.Maybe LoggingInfo)
mutableClusterInfo_loggingInfo = Lens.lens (\MutableClusterInfo' {loggingInfo} -> loggingInfo) (\s@MutableClusterInfo' {} a -> s {loggingInfo = a} :: MutableClusterInfo)

-- | The number of broker nodes in the cluster.
mutableClusterInfo_numberOfBrokerNodes :: Lens.Lens' MutableClusterInfo (Prelude.Maybe Prelude.Int)
mutableClusterInfo_numberOfBrokerNodes = Lens.lens (\MutableClusterInfo' {numberOfBrokerNodes} -> numberOfBrokerNodes) (\s@MutableClusterInfo' {} a -> s {numberOfBrokerNodes = a} :: MutableClusterInfo)

-- | The settings for open monitoring.
mutableClusterInfo_openMonitoring :: Lens.Lens' MutableClusterInfo (Prelude.Maybe OpenMonitoring)
mutableClusterInfo_openMonitoring = Lens.lens (\MutableClusterInfo' {openMonitoring} -> openMonitoring) (\s@MutableClusterInfo' {} a -> s {openMonitoring = a} :: MutableClusterInfo)

-- | This controls storage mode for supported storage tiers.
mutableClusterInfo_storageMode :: Lens.Lens' MutableClusterInfo (Prelude.Maybe StorageMode)
mutableClusterInfo_storageMode = Lens.lens (\MutableClusterInfo' {storageMode} -> storageMode) (\s@MutableClusterInfo' {} a -> s {storageMode = a} :: MutableClusterInfo)

instance Data.FromJSON MutableClusterInfo where
  parseJSON =
    Data.withObject
      "MutableClusterInfo"
      ( \x ->
          MutableClusterInfo'
            Prelude.<$> ( x
                            Data..:? "brokerEBSVolumeInfo"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "clientAuthentication")
            Prelude.<*> (x Data..:? "configurationInfo")
            Prelude.<*> (x Data..:? "connectivityInfo")
            Prelude.<*> (x Data..:? "encryptionInfo")
            Prelude.<*> (x Data..:? "enhancedMonitoring")
            Prelude.<*> (x Data..:? "instanceType")
            Prelude.<*> (x Data..:? "kafkaVersion")
            Prelude.<*> (x Data..:? "loggingInfo")
            Prelude.<*> (x Data..:? "numberOfBrokerNodes")
            Prelude.<*> (x Data..:? "openMonitoring")
            Prelude.<*> (x Data..:? "storageMode")
      )

instance Prelude.Hashable MutableClusterInfo where
  hashWithSalt _salt MutableClusterInfo' {..} =
    _salt
      `Prelude.hashWithSalt` brokerEBSVolumeInfo
      `Prelude.hashWithSalt` clientAuthentication
      `Prelude.hashWithSalt` configurationInfo
      `Prelude.hashWithSalt` connectivityInfo
      `Prelude.hashWithSalt` encryptionInfo
      `Prelude.hashWithSalt` enhancedMonitoring
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` kafkaVersion
      `Prelude.hashWithSalt` loggingInfo
      `Prelude.hashWithSalt` numberOfBrokerNodes
      `Prelude.hashWithSalt` openMonitoring
      `Prelude.hashWithSalt` storageMode

instance Prelude.NFData MutableClusterInfo where
  rnf MutableClusterInfo' {..} =
    Prelude.rnf brokerEBSVolumeInfo
      `Prelude.seq` Prelude.rnf clientAuthentication
      `Prelude.seq` Prelude.rnf configurationInfo
      `Prelude.seq` Prelude.rnf connectivityInfo
      `Prelude.seq` Prelude.rnf encryptionInfo
      `Prelude.seq` Prelude.rnf enhancedMonitoring
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf kafkaVersion
      `Prelude.seq` Prelude.rnf loggingInfo
      `Prelude.seq` Prelude.rnf numberOfBrokerNodes
      `Prelude.seq` Prelude.rnf openMonitoring
      `Prelude.seq` Prelude.rnf storageMode
