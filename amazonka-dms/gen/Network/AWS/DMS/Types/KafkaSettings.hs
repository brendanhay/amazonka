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
-- Module      : Network.AWS.DMS.Types.KafkaSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.KafkaSettings where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types.KafkaSecurityProtocol
import Network.AWS.DMS.Types.MessageFormatValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information that describes an Apache Kafka endpoint. This
-- information includes the output format of records applied to the
-- endpoint and details of transaction and control table data information.
--
-- /See:/ 'newKafkaSettings' smart constructor.
data KafkaSettings = KafkaSettings'
  { -- | Set this optional parameter to @true@ to avoid adding a \'0x\' prefix to
    -- raw data in hexadecimal format. For example, by default, DMS adds a
    -- \'0x\' prefix to the LOB column type in hexadecimal format moving from
    -- an Oracle source to a Kafka target. Use the @NoHexPrefix@ endpoint
    -- setting to enable migration of RAW data type columns without adding the
    -- \'0x\' prefix.
    noHexPrefix :: Prelude.Maybe Prelude.Bool,
    -- | Include NULL and empty columns for records migrated to the endpoint. The
    -- default is @false@.
    includeNullAndEmpty :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) for the private certificate authority
    -- (CA) cert that DMS uses to securely connect to your Kafka target
    -- endpoint.
    sslCaCertificateArn :: Prelude.Maybe Prelude.Text,
    -- | The output format for the records created on the endpoint. The message
    -- format is @JSON@ (default) or @JSON_UNFORMATTED@ (a single line with no
    -- tab).
    messageFormat :: Prelude.Maybe MessageFormatValue,
    -- | The Amazon Resource Name (ARN) of the client certificate used to
    -- securely connect to a Kafka target endpoint.
    sslClientCertificateArn :: Prelude.Maybe Prelude.Text,
    -- | The password for the client private key used to securely connect to a
    -- Kafka target endpoint.
    sslClientKeyPassword :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) for the client private key used to
    -- securely connect to a Kafka target endpoint.
    sslClientKeyArn :: Prelude.Maybe Prelude.Text,
    -- | Shows detailed control information for table definition, column
    -- definition, and table and column changes in the Kafka message output.
    -- The default is @false@.
    includeControlDetails :: Prelude.Maybe Prelude.Bool,
    -- | Prefixes schema and table names to partition values, when the partition
    -- type is @primary-key-type@. Doing this increases data distribution among
    -- Kafka partitions. For example, suppose that a SysBench schema has
    -- thousands of tables and each table has only limited range for a primary
    -- key. In this case, the same primary key is sent from thousands of tables
    -- to the same partition, which causes throttling. The default is @false@.
    partitionIncludeSchemaTable :: Prelude.Maybe Prelude.Bool,
    -- | The topic to which you migrate the data. If you don\'t specify a topic,
    -- DMS specifies @\"kafka-default-topic\"@ as the migration topic.
    topic :: Prelude.Maybe Prelude.Text,
    -- | The maximum size in bytes for records created on the endpoint The
    -- default is 1,000,000.
    messageMaxBytes :: Prelude.Maybe Prelude.Int,
    -- | A comma-separated list of one or more broker locations in your Kafka
    -- cluster that host your Kafka instance. Specify each broker location in
    -- the form @ broker-hostname-or-ip:port @. For example,
    -- @\"ec2-12-345-678-901.compute-1.amazonaws.com:2345\"@. For more
    -- information and examples of specifying a list of broker locations, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kafka.html Using Apache Kafka as a target for Database Migration Service>
    -- in the /Database Migration Service User Guide/.
    broker :: Prelude.Maybe Prelude.Text,
    -- | The secure user name you created when you first set up your MSK cluster
    -- to validate a client identity and make an encrypted connection between
    -- server and client using SASL-SSL authentication.
    saslUsername :: Prelude.Maybe Prelude.Text,
    -- | Set secure connection to a Kafka target endpoint using Transport Layer
    -- Security (TLS). Options include @ssl-encryption@, @ssl-authentication@,
    -- and @sasl-ssl@. @sasl-ssl@ requires @SaslUsername@ and @SaslPassword@.
    securityProtocol :: Prelude.Maybe KafkaSecurityProtocol,
    -- | Shows the partition value within the Kafka message output unless the
    -- partition type is @schema-table-type@. The default is @false@.
    includePartitionValue :: Prelude.Maybe Prelude.Bool,
    -- | Provides detailed transaction information from the source database. This
    -- information includes a commit timestamp, a log position, and values for
    -- @transaction_id@, previous @transaction_id@, and @transaction_record_id@
    -- (the record offset within a transaction). The default is @false@.
    includeTransactionDetails :: Prelude.Maybe Prelude.Bool,
    -- | The secure password you created when you first set up your MSK cluster
    -- to validate a client identity and make an encrypted connection between
    -- server and client using SASL-SSL authentication.
    saslPassword :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Includes any data definition language (DDL) operations that change the
    -- table in the control data, such as @rename-table@, @drop-table@,
    -- @add-column@, @drop-column@, and @rename-column@. The default is
    -- @false@.
    includeTableAlterOperations :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KafkaSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'noHexPrefix', 'kafkaSettings_noHexPrefix' - Set this optional parameter to @true@ to avoid adding a \'0x\' prefix to
-- raw data in hexadecimal format. For example, by default, DMS adds a
-- \'0x\' prefix to the LOB column type in hexadecimal format moving from
-- an Oracle source to a Kafka target. Use the @NoHexPrefix@ endpoint
-- setting to enable migration of RAW data type columns without adding the
-- \'0x\' prefix.
--
-- 'includeNullAndEmpty', 'kafkaSettings_includeNullAndEmpty' - Include NULL and empty columns for records migrated to the endpoint. The
-- default is @false@.
--
-- 'sslCaCertificateArn', 'kafkaSettings_sslCaCertificateArn' - The Amazon Resource Name (ARN) for the private certificate authority
-- (CA) cert that DMS uses to securely connect to your Kafka target
-- endpoint.
--
-- 'messageFormat', 'kafkaSettings_messageFormat' - The output format for the records created on the endpoint. The message
-- format is @JSON@ (default) or @JSON_UNFORMATTED@ (a single line with no
-- tab).
--
-- 'sslClientCertificateArn', 'kafkaSettings_sslClientCertificateArn' - The Amazon Resource Name (ARN) of the client certificate used to
-- securely connect to a Kafka target endpoint.
--
-- 'sslClientKeyPassword', 'kafkaSettings_sslClientKeyPassword' - The password for the client private key used to securely connect to a
-- Kafka target endpoint.
--
-- 'sslClientKeyArn', 'kafkaSettings_sslClientKeyArn' - The Amazon Resource Name (ARN) for the client private key used to
-- securely connect to a Kafka target endpoint.
--
-- 'includeControlDetails', 'kafkaSettings_includeControlDetails' - Shows detailed control information for table definition, column
-- definition, and table and column changes in the Kafka message output.
-- The default is @false@.
--
-- 'partitionIncludeSchemaTable', 'kafkaSettings_partitionIncludeSchemaTable' - Prefixes schema and table names to partition values, when the partition
-- type is @primary-key-type@. Doing this increases data distribution among
-- Kafka partitions. For example, suppose that a SysBench schema has
-- thousands of tables and each table has only limited range for a primary
-- key. In this case, the same primary key is sent from thousands of tables
-- to the same partition, which causes throttling. The default is @false@.
--
-- 'topic', 'kafkaSettings_topic' - The topic to which you migrate the data. If you don\'t specify a topic,
-- DMS specifies @\"kafka-default-topic\"@ as the migration topic.
--
-- 'messageMaxBytes', 'kafkaSettings_messageMaxBytes' - The maximum size in bytes for records created on the endpoint The
-- default is 1,000,000.
--
-- 'broker', 'kafkaSettings_broker' - A comma-separated list of one or more broker locations in your Kafka
-- cluster that host your Kafka instance. Specify each broker location in
-- the form @ broker-hostname-or-ip:port @. For example,
-- @\"ec2-12-345-678-901.compute-1.amazonaws.com:2345\"@. For more
-- information and examples of specifying a list of broker locations, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kafka.html Using Apache Kafka as a target for Database Migration Service>
-- in the /Database Migration Service User Guide/.
--
-- 'saslUsername', 'kafkaSettings_saslUsername' - The secure user name you created when you first set up your MSK cluster
-- to validate a client identity and make an encrypted connection between
-- server and client using SASL-SSL authentication.
--
-- 'securityProtocol', 'kafkaSettings_securityProtocol' - Set secure connection to a Kafka target endpoint using Transport Layer
-- Security (TLS). Options include @ssl-encryption@, @ssl-authentication@,
-- and @sasl-ssl@. @sasl-ssl@ requires @SaslUsername@ and @SaslPassword@.
--
-- 'includePartitionValue', 'kafkaSettings_includePartitionValue' - Shows the partition value within the Kafka message output unless the
-- partition type is @schema-table-type@. The default is @false@.
--
-- 'includeTransactionDetails', 'kafkaSettings_includeTransactionDetails' - Provides detailed transaction information from the source database. This
-- information includes a commit timestamp, a log position, and values for
-- @transaction_id@, previous @transaction_id@, and @transaction_record_id@
-- (the record offset within a transaction). The default is @false@.
--
-- 'saslPassword', 'kafkaSettings_saslPassword' - The secure password you created when you first set up your MSK cluster
-- to validate a client identity and make an encrypted connection between
-- server and client using SASL-SSL authentication.
--
-- 'includeTableAlterOperations', 'kafkaSettings_includeTableAlterOperations' - Includes any data definition language (DDL) operations that change the
-- table in the control data, such as @rename-table@, @drop-table@,
-- @add-column@, @drop-column@, and @rename-column@. The default is
-- @false@.
newKafkaSettings ::
  KafkaSettings
newKafkaSettings =
  KafkaSettings'
    { noHexPrefix = Prelude.Nothing,
      includeNullAndEmpty = Prelude.Nothing,
      sslCaCertificateArn = Prelude.Nothing,
      messageFormat = Prelude.Nothing,
      sslClientCertificateArn = Prelude.Nothing,
      sslClientKeyPassword = Prelude.Nothing,
      sslClientKeyArn = Prelude.Nothing,
      includeControlDetails = Prelude.Nothing,
      partitionIncludeSchemaTable = Prelude.Nothing,
      topic = Prelude.Nothing,
      messageMaxBytes = Prelude.Nothing,
      broker = Prelude.Nothing,
      saslUsername = Prelude.Nothing,
      securityProtocol = Prelude.Nothing,
      includePartitionValue = Prelude.Nothing,
      includeTransactionDetails = Prelude.Nothing,
      saslPassword = Prelude.Nothing,
      includeTableAlterOperations = Prelude.Nothing
    }

-- | Set this optional parameter to @true@ to avoid adding a \'0x\' prefix to
-- raw data in hexadecimal format. For example, by default, DMS adds a
-- \'0x\' prefix to the LOB column type in hexadecimal format moving from
-- an Oracle source to a Kafka target. Use the @NoHexPrefix@ endpoint
-- setting to enable migration of RAW data type columns without adding the
-- \'0x\' prefix.
kafkaSettings_noHexPrefix :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Bool)
kafkaSettings_noHexPrefix = Lens.lens (\KafkaSettings' {noHexPrefix} -> noHexPrefix) (\s@KafkaSettings' {} a -> s {noHexPrefix = a} :: KafkaSettings)

-- | Include NULL and empty columns for records migrated to the endpoint. The
-- default is @false@.
kafkaSettings_includeNullAndEmpty :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Bool)
kafkaSettings_includeNullAndEmpty = Lens.lens (\KafkaSettings' {includeNullAndEmpty} -> includeNullAndEmpty) (\s@KafkaSettings' {} a -> s {includeNullAndEmpty = a} :: KafkaSettings)

-- | The Amazon Resource Name (ARN) for the private certificate authority
-- (CA) cert that DMS uses to securely connect to your Kafka target
-- endpoint.
kafkaSettings_sslCaCertificateArn :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Text)
kafkaSettings_sslCaCertificateArn = Lens.lens (\KafkaSettings' {sslCaCertificateArn} -> sslCaCertificateArn) (\s@KafkaSettings' {} a -> s {sslCaCertificateArn = a} :: KafkaSettings)

-- | The output format for the records created on the endpoint. The message
-- format is @JSON@ (default) or @JSON_UNFORMATTED@ (a single line with no
-- tab).
kafkaSettings_messageFormat :: Lens.Lens' KafkaSettings (Prelude.Maybe MessageFormatValue)
kafkaSettings_messageFormat = Lens.lens (\KafkaSettings' {messageFormat} -> messageFormat) (\s@KafkaSettings' {} a -> s {messageFormat = a} :: KafkaSettings)

-- | The Amazon Resource Name (ARN) of the client certificate used to
-- securely connect to a Kafka target endpoint.
kafkaSettings_sslClientCertificateArn :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Text)
kafkaSettings_sslClientCertificateArn = Lens.lens (\KafkaSettings' {sslClientCertificateArn} -> sslClientCertificateArn) (\s@KafkaSettings' {} a -> s {sslClientCertificateArn = a} :: KafkaSettings)

-- | The password for the client private key used to securely connect to a
-- Kafka target endpoint.
kafkaSettings_sslClientKeyPassword :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Text)
kafkaSettings_sslClientKeyPassword = Lens.lens (\KafkaSettings' {sslClientKeyPassword} -> sslClientKeyPassword) (\s@KafkaSettings' {} a -> s {sslClientKeyPassword = a} :: KafkaSettings) Prelude.. Lens.mapping Core._Sensitive

-- | The Amazon Resource Name (ARN) for the client private key used to
-- securely connect to a Kafka target endpoint.
kafkaSettings_sslClientKeyArn :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Text)
kafkaSettings_sslClientKeyArn = Lens.lens (\KafkaSettings' {sslClientKeyArn} -> sslClientKeyArn) (\s@KafkaSettings' {} a -> s {sslClientKeyArn = a} :: KafkaSettings)

-- | Shows detailed control information for table definition, column
-- definition, and table and column changes in the Kafka message output.
-- The default is @false@.
kafkaSettings_includeControlDetails :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Bool)
kafkaSettings_includeControlDetails = Lens.lens (\KafkaSettings' {includeControlDetails} -> includeControlDetails) (\s@KafkaSettings' {} a -> s {includeControlDetails = a} :: KafkaSettings)

-- | Prefixes schema and table names to partition values, when the partition
-- type is @primary-key-type@. Doing this increases data distribution among
-- Kafka partitions. For example, suppose that a SysBench schema has
-- thousands of tables and each table has only limited range for a primary
-- key. In this case, the same primary key is sent from thousands of tables
-- to the same partition, which causes throttling. The default is @false@.
kafkaSettings_partitionIncludeSchemaTable :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Bool)
kafkaSettings_partitionIncludeSchemaTable = Lens.lens (\KafkaSettings' {partitionIncludeSchemaTable} -> partitionIncludeSchemaTable) (\s@KafkaSettings' {} a -> s {partitionIncludeSchemaTable = a} :: KafkaSettings)

-- | The topic to which you migrate the data. If you don\'t specify a topic,
-- DMS specifies @\"kafka-default-topic\"@ as the migration topic.
kafkaSettings_topic :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Text)
kafkaSettings_topic = Lens.lens (\KafkaSettings' {topic} -> topic) (\s@KafkaSettings' {} a -> s {topic = a} :: KafkaSettings)

-- | The maximum size in bytes for records created on the endpoint The
-- default is 1,000,000.
kafkaSettings_messageMaxBytes :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Int)
kafkaSettings_messageMaxBytes = Lens.lens (\KafkaSettings' {messageMaxBytes} -> messageMaxBytes) (\s@KafkaSettings' {} a -> s {messageMaxBytes = a} :: KafkaSettings)

-- | A comma-separated list of one or more broker locations in your Kafka
-- cluster that host your Kafka instance. Specify each broker location in
-- the form @ broker-hostname-or-ip:port @. For example,
-- @\"ec2-12-345-678-901.compute-1.amazonaws.com:2345\"@. For more
-- information and examples of specifying a list of broker locations, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kafka.html Using Apache Kafka as a target for Database Migration Service>
-- in the /Database Migration Service User Guide/.
kafkaSettings_broker :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Text)
kafkaSettings_broker = Lens.lens (\KafkaSettings' {broker} -> broker) (\s@KafkaSettings' {} a -> s {broker = a} :: KafkaSettings)

-- | The secure user name you created when you first set up your MSK cluster
-- to validate a client identity and make an encrypted connection between
-- server and client using SASL-SSL authentication.
kafkaSettings_saslUsername :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Text)
kafkaSettings_saslUsername = Lens.lens (\KafkaSettings' {saslUsername} -> saslUsername) (\s@KafkaSettings' {} a -> s {saslUsername = a} :: KafkaSettings)

-- | Set secure connection to a Kafka target endpoint using Transport Layer
-- Security (TLS). Options include @ssl-encryption@, @ssl-authentication@,
-- and @sasl-ssl@. @sasl-ssl@ requires @SaslUsername@ and @SaslPassword@.
kafkaSettings_securityProtocol :: Lens.Lens' KafkaSettings (Prelude.Maybe KafkaSecurityProtocol)
kafkaSettings_securityProtocol = Lens.lens (\KafkaSettings' {securityProtocol} -> securityProtocol) (\s@KafkaSettings' {} a -> s {securityProtocol = a} :: KafkaSettings)

-- | Shows the partition value within the Kafka message output unless the
-- partition type is @schema-table-type@. The default is @false@.
kafkaSettings_includePartitionValue :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Bool)
kafkaSettings_includePartitionValue = Lens.lens (\KafkaSettings' {includePartitionValue} -> includePartitionValue) (\s@KafkaSettings' {} a -> s {includePartitionValue = a} :: KafkaSettings)

-- | Provides detailed transaction information from the source database. This
-- information includes a commit timestamp, a log position, and values for
-- @transaction_id@, previous @transaction_id@, and @transaction_record_id@
-- (the record offset within a transaction). The default is @false@.
kafkaSettings_includeTransactionDetails :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Bool)
kafkaSettings_includeTransactionDetails = Lens.lens (\KafkaSettings' {includeTransactionDetails} -> includeTransactionDetails) (\s@KafkaSettings' {} a -> s {includeTransactionDetails = a} :: KafkaSettings)

-- | The secure password you created when you first set up your MSK cluster
-- to validate a client identity and make an encrypted connection between
-- server and client using SASL-SSL authentication.
kafkaSettings_saslPassword :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Text)
kafkaSettings_saslPassword = Lens.lens (\KafkaSettings' {saslPassword} -> saslPassword) (\s@KafkaSettings' {} a -> s {saslPassword = a} :: KafkaSettings) Prelude.. Lens.mapping Core._Sensitive

-- | Includes any data definition language (DDL) operations that change the
-- table in the control data, such as @rename-table@, @drop-table@,
-- @add-column@, @drop-column@, and @rename-column@. The default is
-- @false@.
kafkaSettings_includeTableAlterOperations :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Bool)
kafkaSettings_includeTableAlterOperations = Lens.lens (\KafkaSettings' {includeTableAlterOperations} -> includeTableAlterOperations) (\s@KafkaSettings' {} a -> s {includeTableAlterOperations = a} :: KafkaSettings)

instance Core.FromJSON KafkaSettings where
  parseJSON =
    Core.withObject
      "KafkaSettings"
      ( \x ->
          KafkaSettings'
            Prelude.<$> (x Core..:? "NoHexPrefix")
            Prelude.<*> (x Core..:? "IncludeNullAndEmpty")
            Prelude.<*> (x Core..:? "SslCaCertificateArn")
            Prelude.<*> (x Core..:? "MessageFormat")
            Prelude.<*> (x Core..:? "SslClientCertificateArn")
            Prelude.<*> (x Core..:? "SslClientKeyPassword")
            Prelude.<*> (x Core..:? "SslClientKeyArn")
            Prelude.<*> (x Core..:? "IncludeControlDetails")
            Prelude.<*> (x Core..:? "PartitionIncludeSchemaTable")
            Prelude.<*> (x Core..:? "Topic")
            Prelude.<*> (x Core..:? "MessageMaxBytes")
            Prelude.<*> (x Core..:? "Broker")
            Prelude.<*> (x Core..:? "SaslUsername")
            Prelude.<*> (x Core..:? "SecurityProtocol")
            Prelude.<*> (x Core..:? "IncludePartitionValue")
            Prelude.<*> (x Core..:? "IncludeTransactionDetails")
            Prelude.<*> (x Core..:? "SaslPassword")
            Prelude.<*> (x Core..:? "IncludeTableAlterOperations")
      )

instance Prelude.Hashable KafkaSettings

instance Prelude.NFData KafkaSettings

instance Core.ToJSON KafkaSettings where
  toJSON KafkaSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NoHexPrefix" Core..=) Prelude.<$> noHexPrefix,
            ("IncludeNullAndEmpty" Core..=)
              Prelude.<$> includeNullAndEmpty,
            ("SslCaCertificateArn" Core..=)
              Prelude.<$> sslCaCertificateArn,
            ("MessageFormat" Core..=) Prelude.<$> messageFormat,
            ("SslClientCertificateArn" Core..=)
              Prelude.<$> sslClientCertificateArn,
            ("SslClientKeyPassword" Core..=)
              Prelude.<$> sslClientKeyPassword,
            ("SslClientKeyArn" Core..=)
              Prelude.<$> sslClientKeyArn,
            ("IncludeControlDetails" Core..=)
              Prelude.<$> includeControlDetails,
            ("PartitionIncludeSchemaTable" Core..=)
              Prelude.<$> partitionIncludeSchemaTable,
            ("Topic" Core..=) Prelude.<$> topic,
            ("MessageMaxBytes" Core..=)
              Prelude.<$> messageMaxBytes,
            ("Broker" Core..=) Prelude.<$> broker,
            ("SaslUsername" Core..=) Prelude.<$> saslUsername,
            ("SecurityProtocol" Core..=)
              Prelude.<$> securityProtocol,
            ("IncludePartitionValue" Core..=)
              Prelude.<$> includePartitionValue,
            ("IncludeTransactionDetails" Core..=)
              Prelude.<$> includeTransactionDetails,
            ("SaslPassword" Core..=) Prelude.<$> saslPassword,
            ("IncludeTableAlterOperations" Core..=)
              Prelude.<$> includeTableAlterOperations
          ]
      )
