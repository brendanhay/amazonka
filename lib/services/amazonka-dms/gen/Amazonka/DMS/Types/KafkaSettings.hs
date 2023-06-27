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
-- Module      : Amazonka.DMS.Types.KafkaSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.KafkaSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types.KafkaSaslMechanism
import Amazonka.DMS.Types.KafkaSecurityProtocol
import Amazonka.DMS.Types.MessageFormatValue
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information that describes an Apache Kafka endpoint. This
-- information includes the output format of records applied to the
-- endpoint and details of transaction and control table data information.
--
-- /See:/ 'newKafkaSettings' smart constructor.
data KafkaSettings = KafkaSettings'
  { -- | A comma-separated list of one or more broker locations in your Kafka
    -- cluster that host your Kafka instance. Specify each broker location in
    -- the form @ @/@broker-hostname-or-ip@/@:@/@port@/@ @. For example,
    -- @\"ec2-12-345-678-901.compute-1.amazonaws.com:2345\"@. For more
    -- information and examples of specifying a list of broker locations, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kafka.html Using Apache Kafka as a target for Database Migration Service>
    -- in the /Database Migration Service User Guide/.
    broker :: Prelude.Maybe Prelude.Text,
    -- | Shows detailed control information for table definition, column
    -- definition, and table and column changes in the Kafka message output.
    -- The default is @false@.
    includeControlDetails :: Prelude.Maybe Prelude.Bool,
    -- | Include NULL and empty columns for records migrated to the endpoint. The
    -- default is @false@.
    includeNullAndEmpty :: Prelude.Maybe Prelude.Bool,
    -- | Shows the partition value within the Kafka message output unless the
    -- partition type is @schema-table-type@. The default is @false@.
    includePartitionValue :: Prelude.Maybe Prelude.Bool,
    -- | Includes any data definition language (DDL) operations that change the
    -- table in the control data, such as @rename-table@, @drop-table@,
    -- @add-column@, @drop-column@, and @rename-column@. The default is
    -- @false@.
    includeTableAlterOperations :: Prelude.Maybe Prelude.Bool,
    -- | Provides detailed transaction information from the source database. This
    -- information includes a commit timestamp, a log position, and values for
    -- @transaction_id@, previous @transaction_id@, and @transaction_record_id@
    -- (the record offset within a transaction). The default is @false@.
    includeTransactionDetails :: Prelude.Maybe Prelude.Bool,
    -- | The output format for the records created on the endpoint. The message
    -- format is @JSON@ (default) or @JSON_UNFORMATTED@ (a single line with no
    -- tab).
    messageFormat :: Prelude.Maybe MessageFormatValue,
    -- | The maximum size in bytes for records created on the endpoint The
    -- default is 1,000,000.
    messageMaxBytes :: Prelude.Maybe Prelude.Int,
    -- | Set this optional parameter to @true@ to avoid adding a \'0x\' prefix to
    -- raw data in hexadecimal format. For example, by default, DMS adds a
    -- \'0x\' prefix to the LOB column type in hexadecimal format moving from
    -- an Oracle source to a Kafka target. Use the @NoHexPrefix@ endpoint
    -- setting to enable migration of RAW data type columns without adding the
    -- \'0x\' prefix.
    noHexPrefix :: Prelude.Maybe Prelude.Bool,
    -- | Prefixes schema and table names to partition values, when the partition
    -- type is @primary-key-type@. Doing this increases data distribution among
    -- Kafka partitions. For example, suppose that a SysBench schema has
    -- thousands of tables and each table has only limited range for a primary
    -- key. In this case, the same primary key is sent from thousands of tables
    -- to the same partition, which causes throttling. The default is @false@.
    partitionIncludeSchemaTable :: Prelude.Maybe Prelude.Bool,
    -- | For SASL\/SSL authentication, DMS supports the @SCRAM-SHA-512@ mechanism
    -- by default. DMS versions 3.5.0 and later also support the @PLAIN@
    -- mechanism. To use the @PLAIN@ mechanism, set this parameter to @PLAIN.@
    saslMechanism :: Prelude.Maybe KafkaSaslMechanism,
    -- | The secure password you created when you first set up your MSK cluster
    -- to validate a client identity and make an encrypted connection between
    -- server and client using SASL-SSL authentication.
    saslPassword :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The secure user name you created when you first set up your MSK cluster
    -- to validate a client identity and make an encrypted connection between
    -- server and client using SASL-SSL authentication.
    saslUsername :: Prelude.Maybe Prelude.Text,
    -- | Set secure connection to a Kafka target endpoint using Transport Layer
    -- Security (TLS). Options include @ssl-encryption@, @ssl-authentication@,
    -- and @sasl-ssl@. @sasl-ssl@ requires @SaslUsername@ and @SaslPassword@.
    securityProtocol :: Prelude.Maybe KafkaSecurityProtocol,
    -- | The Amazon Resource Name (ARN) for the private certificate authority
    -- (CA) cert that DMS uses to securely connect to your Kafka target
    -- endpoint.
    sslCaCertificateArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the client certificate used to
    -- securely connect to a Kafka target endpoint.
    sslClientCertificateArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the client private key used to
    -- securely connect to a Kafka target endpoint.
    sslClientKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The password for the client private key used to securely connect to a
    -- Kafka target endpoint.
    sslClientKeyPassword :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The topic to which you migrate the data. If you don\'t specify a topic,
    -- DMS specifies @\"kafka-default-topic\"@ as the migration topic.
    topic :: Prelude.Maybe Prelude.Text
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
-- 'broker', 'kafkaSettings_broker' - A comma-separated list of one or more broker locations in your Kafka
-- cluster that host your Kafka instance. Specify each broker location in
-- the form @ @/@broker-hostname-or-ip@/@:@/@port@/@ @. For example,
-- @\"ec2-12-345-678-901.compute-1.amazonaws.com:2345\"@. For more
-- information and examples of specifying a list of broker locations, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kafka.html Using Apache Kafka as a target for Database Migration Service>
-- in the /Database Migration Service User Guide/.
--
-- 'includeControlDetails', 'kafkaSettings_includeControlDetails' - Shows detailed control information for table definition, column
-- definition, and table and column changes in the Kafka message output.
-- The default is @false@.
--
-- 'includeNullAndEmpty', 'kafkaSettings_includeNullAndEmpty' - Include NULL and empty columns for records migrated to the endpoint. The
-- default is @false@.
--
-- 'includePartitionValue', 'kafkaSettings_includePartitionValue' - Shows the partition value within the Kafka message output unless the
-- partition type is @schema-table-type@. The default is @false@.
--
-- 'includeTableAlterOperations', 'kafkaSettings_includeTableAlterOperations' - Includes any data definition language (DDL) operations that change the
-- table in the control data, such as @rename-table@, @drop-table@,
-- @add-column@, @drop-column@, and @rename-column@. The default is
-- @false@.
--
-- 'includeTransactionDetails', 'kafkaSettings_includeTransactionDetails' - Provides detailed transaction information from the source database. This
-- information includes a commit timestamp, a log position, and values for
-- @transaction_id@, previous @transaction_id@, and @transaction_record_id@
-- (the record offset within a transaction). The default is @false@.
--
-- 'messageFormat', 'kafkaSettings_messageFormat' - The output format for the records created on the endpoint. The message
-- format is @JSON@ (default) or @JSON_UNFORMATTED@ (a single line with no
-- tab).
--
-- 'messageMaxBytes', 'kafkaSettings_messageMaxBytes' - The maximum size in bytes for records created on the endpoint The
-- default is 1,000,000.
--
-- 'noHexPrefix', 'kafkaSettings_noHexPrefix' - Set this optional parameter to @true@ to avoid adding a \'0x\' prefix to
-- raw data in hexadecimal format. For example, by default, DMS adds a
-- \'0x\' prefix to the LOB column type in hexadecimal format moving from
-- an Oracle source to a Kafka target. Use the @NoHexPrefix@ endpoint
-- setting to enable migration of RAW data type columns without adding the
-- \'0x\' prefix.
--
-- 'partitionIncludeSchemaTable', 'kafkaSettings_partitionIncludeSchemaTable' - Prefixes schema and table names to partition values, when the partition
-- type is @primary-key-type@. Doing this increases data distribution among
-- Kafka partitions. For example, suppose that a SysBench schema has
-- thousands of tables and each table has only limited range for a primary
-- key. In this case, the same primary key is sent from thousands of tables
-- to the same partition, which causes throttling. The default is @false@.
--
-- 'saslMechanism', 'kafkaSettings_saslMechanism' - For SASL\/SSL authentication, DMS supports the @SCRAM-SHA-512@ mechanism
-- by default. DMS versions 3.5.0 and later also support the @PLAIN@
-- mechanism. To use the @PLAIN@ mechanism, set this parameter to @PLAIN.@
--
-- 'saslPassword', 'kafkaSettings_saslPassword' - The secure password you created when you first set up your MSK cluster
-- to validate a client identity and make an encrypted connection between
-- server and client using SASL-SSL authentication.
--
-- 'saslUsername', 'kafkaSettings_saslUsername' - The secure user name you created when you first set up your MSK cluster
-- to validate a client identity and make an encrypted connection between
-- server and client using SASL-SSL authentication.
--
-- 'securityProtocol', 'kafkaSettings_securityProtocol' - Set secure connection to a Kafka target endpoint using Transport Layer
-- Security (TLS). Options include @ssl-encryption@, @ssl-authentication@,
-- and @sasl-ssl@. @sasl-ssl@ requires @SaslUsername@ and @SaslPassword@.
--
-- 'sslCaCertificateArn', 'kafkaSettings_sslCaCertificateArn' - The Amazon Resource Name (ARN) for the private certificate authority
-- (CA) cert that DMS uses to securely connect to your Kafka target
-- endpoint.
--
-- 'sslClientCertificateArn', 'kafkaSettings_sslClientCertificateArn' - The Amazon Resource Name (ARN) of the client certificate used to
-- securely connect to a Kafka target endpoint.
--
-- 'sslClientKeyArn', 'kafkaSettings_sslClientKeyArn' - The Amazon Resource Name (ARN) for the client private key used to
-- securely connect to a Kafka target endpoint.
--
-- 'sslClientKeyPassword', 'kafkaSettings_sslClientKeyPassword' - The password for the client private key used to securely connect to a
-- Kafka target endpoint.
--
-- 'topic', 'kafkaSettings_topic' - The topic to which you migrate the data. If you don\'t specify a topic,
-- DMS specifies @\"kafka-default-topic\"@ as the migration topic.
newKafkaSettings ::
  KafkaSettings
newKafkaSettings =
  KafkaSettings'
    { broker = Prelude.Nothing,
      includeControlDetails = Prelude.Nothing,
      includeNullAndEmpty = Prelude.Nothing,
      includePartitionValue = Prelude.Nothing,
      includeTableAlterOperations = Prelude.Nothing,
      includeTransactionDetails = Prelude.Nothing,
      messageFormat = Prelude.Nothing,
      messageMaxBytes = Prelude.Nothing,
      noHexPrefix = Prelude.Nothing,
      partitionIncludeSchemaTable = Prelude.Nothing,
      saslMechanism = Prelude.Nothing,
      saslPassword = Prelude.Nothing,
      saslUsername = Prelude.Nothing,
      securityProtocol = Prelude.Nothing,
      sslCaCertificateArn = Prelude.Nothing,
      sslClientCertificateArn = Prelude.Nothing,
      sslClientKeyArn = Prelude.Nothing,
      sslClientKeyPassword = Prelude.Nothing,
      topic = Prelude.Nothing
    }

-- | A comma-separated list of one or more broker locations in your Kafka
-- cluster that host your Kafka instance. Specify each broker location in
-- the form @ @/@broker-hostname-or-ip@/@:@/@port@/@ @. For example,
-- @\"ec2-12-345-678-901.compute-1.amazonaws.com:2345\"@. For more
-- information and examples of specifying a list of broker locations, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Kafka.html Using Apache Kafka as a target for Database Migration Service>
-- in the /Database Migration Service User Guide/.
kafkaSettings_broker :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Text)
kafkaSettings_broker = Lens.lens (\KafkaSettings' {broker} -> broker) (\s@KafkaSettings' {} a -> s {broker = a} :: KafkaSettings)

-- | Shows detailed control information for table definition, column
-- definition, and table and column changes in the Kafka message output.
-- The default is @false@.
kafkaSettings_includeControlDetails :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Bool)
kafkaSettings_includeControlDetails = Lens.lens (\KafkaSettings' {includeControlDetails} -> includeControlDetails) (\s@KafkaSettings' {} a -> s {includeControlDetails = a} :: KafkaSettings)

-- | Include NULL and empty columns for records migrated to the endpoint. The
-- default is @false@.
kafkaSettings_includeNullAndEmpty :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Bool)
kafkaSettings_includeNullAndEmpty = Lens.lens (\KafkaSettings' {includeNullAndEmpty} -> includeNullAndEmpty) (\s@KafkaSettings' {} a -> s {includeNullAndEmpty = a} :: KafkaSettings)

-- | Shows the partition value within the Kafka message output unless the
-- partition type is @schema-table-type@. The default is @false@.
kafkaSettings_includePartitionValue :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Bool)
kafkaSettings_includePartitionValue = Lens.lens (\KafkaSettings' {includePartitionValue} -> includePartitionValue) (\s@KafkaSettings' {} a -> s {includePartitionValue = a} :: KafkaSettings)

-- | Includes any data definition language (DDL) operations that change the
-- table in the control data, such as @rename-table@, @drop-table@,
-- @add-column@, @drop-column@, and @rename-column@. The default is
-- @false@.
kafkaSettings_includeTableAlterOperations :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Bool)
kafkaSettings_includeTableAlterOperations = Lens.lens (\KafkaSettings' {includeTableAlterOperations} -> includeTableAlterOperations) (\s@KafkaSettings' {} a -> s {includeTableAlterOperations = a} :: KafkaSettings)

-- | Provides detailed transaction information from the source database. This
-- information includes a commit timestamp, a log position, and values for
-- @transaction_id@, previous @transaction_id@, and @transaction_record_id@
-- (the record offset within a transaction). The default is @false@.
kafkaSettings_includeTransactionDetails :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Bool)
kafkaSettings_includeTransactionDetails = Lens.lens (\KafkaSettings' {includeTransactionDetails} -> includeTransactionDetails) (\s@KafkaSettings' {} a -> s {includeTransactionDetails = a} :: KafkaSettings)

-- | The output format for the records created on the endpoint. The message
-- format is @JSON@ (default) or @JSON_UNFORMATTED@ (a single line with no
-- tab).
kafkaSettings_messageFormat :: Lens.Lens' KafkaSettings (Prelude.Maybe MessageFormatValue)
kafkaSettings_messageFormat = Lens.lens (\KafkaSettings' {messageFormat} -> messageFormat) (\s@KafkaSettings' {} a -> s {messageFormat = a} :: KafkaSettings)

-- | The maximum size in bytes for records created on the endpoint The
-- default is 1,000,000.
kafkaSettings_messageMaxBytes :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Int)
kafkaSettings_messageMaxBytes = Lens.lens (\KafkaSettings' {messageMaxBytes} -> messageMaxBytes) (\s@KafkaSettings' {} a -> s {messageMaxBytes = a} :: KafkaSettings)

-- | Set this optional parameter to @true@ to avoid adding a \'0x\' prefix to
-- raw data in hexadecimal format. For example, by default, DMS adds a
-- \'0x\' prefix to the LOB column type in hexadecimal format moving from
-- an Oracle source to a Kafka target. Use the @NoHexPrefix@ endpoint
-- setting to enable migration of RAW data type columns without adding the
-- \'0x\' prefix.
kafkaSettings_noHexPrefix :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Bool)
kafkaSettings_noHexPrefix = Lens.lens (\KafkaSettings' {noHexPrefix} -> noHexPrefix) (\s@KafkaSettings' {} a -> s {noHexPrefix = a} :: KafkaSettings)

-- | Prefixes schema and table names to partition values, when the partition
-- type is @primary-key-type@. Doing this increases data distribution among
-- Kafka partitions. For example, suppose that a SysBench schema has
-- thousands of tables and each table has only limited range for a primary
-- key. In this case, the same primary key is sent from thousands of tables
-- to the same partition, which causes throttling. The default is @false@.
kafkaSettings_partitionIncludeSchemaTable :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Bool)
kafkaSettings_partitionIncludeSchemaTable = Lens.lens (\KafkaSettings' {partitionIncludeSchemaTable} -> partitionIncludeSchemaTable) (\s@KafkaSettings' {} a -> s {partitionIncludeSchemaTable = a} :: KafkaSettings)

-- | For SASL\/SSL authentication, DMS supports the @SCRAM-SHA-512@ mechanism
-- by default. DMS versions 3.5.0 and later also support the @PLAIN@
-- mechanism. To use the @PLAIN@ mechanism, set this parameter to @PLAIN.@
kafkaSettings_saslMechanism :: Lens.Lens' KafkaSettings (Prelude.Maybe KafkaSaslMechanism)
kafkaSettings_saslMechanism = Lens.lens (\KafkaSettings' {saslMechanism} -> saslMechanism) (\s@KafkaSettings' {} a -> s {saslMechanism = a} :: KafkaSettings)

-- | The secure password you created when you first set up your MSK cluster
-- to validate a client identity and make an encrypted connection between
-- server and client using SASL-SSL authentication.
kafkaSettings_saslPassword :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Text)
kafkaSettings_saslPassword = Lens.lens (\KafkaSettings' {saslPassword} -> saslPassword) (\s@KafkaSettings' {} a -> s {saslPassword = a} :: KafkaSettings) Prelude.. Lens.mapping Data._Sensitive

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

-- | The Amazon Resource Name (ARN) for the private certificate authority
-- (CA) cert that DMS uses to securely connect to your Kafka target
-- endpoint.
kafkaSettings_sslCaCertificateArn :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Text)
kafkaSettings_sslCaCertificateArn = Lens.lens (\KafkaSettings' {sslCaCertificateArn} -> sslCaCertificateArn) (\s@KafkaSettings' {} a -> s {sslCaCertificateArn = a} :: KafkaSettings)

-- | The Amazon Resource Name (ARN) of the client certificate used to
-- securely connect to a Kafka target endpoint.
kafkaSettings_sslClientCertificateArn :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Text)
kafkaSettings_sslClientCertificateArn = Lens.lens (\KafkaSettings' {sslClientCertificateArn} -> sslClientCertificateArn) (\s@KafkaSettings' {} a -> s {sslClientCertificateArn = a} :: KafkaSettings)

-- | The Amazon Resource Name (ARN) for the client private key used to
-- securely connect to a Kafka target endpoint.
kafkaSettings_sslClientKeyArn :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Text)
kafkaSettings_sslClientKeyArn = Lens.lens (\KafkaSettings' {sslClientKeyArn} -> sslClientKeyArn) (\s@KafkaSettings' {} a -> s {sslClientKeyArn = a} :: KafkaSettings)

-- | The password for the client private key used to securely connect to a
-- Kafka target endpoint.
kafkaSettings_sslClientKeyPassword :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Text)
kafkaSettings_sslClientKeyPassword = Lens.lens (\KafkaSettings' {sslClientKeyPassword} -> sslClientKeyPassword) (\s@KafkaSettings' {} a -> s {sslClientKeyPassword = a} :: KafkaSettings) Prelude.. Lens.mapping Data._Sensitive

-- | The topic to which you migrate the data. If you don\'t specify a topic,
-- DMS specifies @\"kafka-default-topic\"@ as the migration topic.
kafkaSettings_topic :: Lens.Lens' KafkaSettings (Prelude.Maybe Prelude.Text)
kafkaSettings_topic = Lens.lens (\KafkaSettings' {topic} -> topic) (\s@KafkaSettings' {} a -> s {topic = a} :: KafkaSettings)

instance Data.FromJSON KafkaSettings where
  parseJSON =
    Data.withObject
      "KafkaSettings"
      ( \x ->
          KafkaSettings'
            Prelude.<$> (x Data..:? "Broker")
            Prelude.<*> (x Data..:? "IncludeControlDetails")
            Prelude.<*> (x Data..:? "IncludeNullAndEmpty")
            Prelude.<*> (x Data..:? "IncludePartitionValue")
            Prelude.<*> (x Data..:? "IncludeTableAlterOperations")
            Prelude.<*> (x Data..:? "IncludeTransactionDetails")
            Prelude.<*> (x Data..:? "MessageFormat")
            Prelude.<*> (x Data..:? "MessageMaxBytes")
            Prelude.<*> (x Data..:? "NoHexPrefix")
            Prelude.<*> (x Data..:? "PartitionIncludeSchemaTable")
            Prelude.<*> (x Data..:? "SaslMechanism")
            Prelude.<*> (x Data..:? "SaslPassword")
            Prelude.<*> (x Data..:? "SaslUsername")
            Prelude.<*> (x Data..:? "SecurityProtocol")
            Prelude.<*> (x Data..:? "SslCaCertificateArn")
            Prelude.<*> (x Data..:? "SslClientCertificateArn")
            Prelude.<*> (x Data..:? "SslClientKeyArn")
            Prelude.<*> (x Data..:? "SslClientKeyPassword")
            Prelude.<*> (x Data..:? "Topic")
      )

instance Prelude.Hashable KafkaSettings where
  hashWithSalt _salt KafkaSettings' {..} =
    _salt
      `Prelude.hashWithSalt` broker
      `Prelude.hashWithSalt` includeControlDetails
      `Prelude.hashWithSalt` includeNullAndEmpty
      `Prelude.hashWithSalt` includePartitionValue
      `Prelude.hashWithSalt` includeTableAlterOperations
      `Prelude.hashWithSalt` includeTransactionDetails
      `Prelude.hashWithSalt` messageFormat
      `Prelude.hashWithSalt` messageMaxBytes
      `Prelude.hashWithSalt` noHexPrefix
      `Prelude.hashWithSalt` partitionIncludeSchemaTable
      `Prelude.hashWithSalt` saslMechanism
      `Prelude.hashWithSalt` saslPassword
      `Prelude.hashWithSalt` saslUsername
      `Prelude.hashWithSalt` securityProtocol
      `Prelude.hashWithSalt` sslCaCertificateArn
      `Prelude.hashWithSalt` sslClientCertificateArn
      `Prelude.hashWithSalt` sslClientKeyArn
      `Prelude.hashWithSalt` sslClientKeyPassword
      `Prelude.hashWithSalt` topic

instance Prelude.NFData KafkaSettings where
  rnf KafkaSettings' {..} =
    Prelude.rnf broker
      `Prelude.seq` Prelude.rnf includeControlDetails
      `Prelude.seq` Prelude.rnf includeNullAndEmpty
      `Prelude.seq` Prelude.rnf includePartitionValue
      `Prelude.seq` Prelude.rnf includeTableAlterOperations
      `Prelude.seq` Prelude.rnf includeTransactionDetails
      `Prelude.seq` Prelude.rnf messageFormat
      `Prelude.seq` Prelude.rnf messageMaxBytes
      `Prelude.seq` Prelude.rnf noHexPrefix
      `Prelude.seq` Prelude.rnf partitionIncludeSchemaTable
      `Prelude.seq` Prelude.rnf saslMechanism
      `Prelude.seq` Prelude.rnf saslPassword
      `Prelude.seq` Prelude.rnf saslUsername
      `Prelude.seq` Prelude.rnf securityProtocol
      `Prelude.seq` Prelude.rnf sslCaCertificateArn
      `Prelude.seq` Prelude.rnf sslClientCertificateArn
      `Prelude.seq` Prelude.rnf sslClientKeyArn
      `Prelude.seq` Prelude.rnf sslClientKeyPassword
      `Prelude.seq` Prelude.rnf topic

instance Data.ToJSON KafkaSettings where
  toJSON KafkaSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Broker" Data..=) Prelude.<$> broker,
            ("IncludeControlDetails" Data..=)
              Prelude.<$> includeControlDetails,
            ("IncludeNullAndEmpty" Data..=)
              Prelude.<$> includeNullAndEmpty,
            ("IncludePartitionValue" Data..=)
              Prelude.<$> includePartitionValue,
            ("IncludeTableAlterOperations" Data..=)
              Prelude.<$> includeTableAlterOperations,
            ("IncludeTransactionDetails" Data..=)
              Prelude.<$> includeTransactionDetails,
            ("MessageFormat" Data..=) Prelude.<$> messageFormat,
            ("MessageMaxBytes" Data..=)
              Prelude.<$> messageMaxBytes,
            ("NoHexPrefix" Data..=) Prelude.<$> noHexPrefix,
            ("PartitionIncludeSchemaTable" Data..=)
              Prelude.<$> partitionIncludeSchemaTable,
            ("SaslMechanism" Data..=) Prelude.<$> saslMechanism,
            ("SaslPassword" Data..=) Prelude.<$> saslPassword,
            ("SaslUsername" Data..=) Prelude.<$> saslUsername,
            ("SecurityProtocol" Data..=)
              Prelude.<$> securityProtocol,
            ("SslCaCertificateArn" Data..=)
              Prelude.<$> sslCaCertificateArn,
            ("SslClientCertificateArn" Data..=)
              Prelude.<$> sslClientCertificateArn,
            ("SslClientKeyArn" Data..=)
              Prelude.<$> sslClientKeyArn,
            ("SslClientKeyPassword" Data..=)
              Prelude.<$> sslClientKeyPassword,
            ("Topic" Data..=) Prelude.<$> topic
          ]
      )
