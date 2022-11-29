{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.KeySpaces.RestoreTable
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores the specified table to the specified point in time within the
-- @earliest_restorable_timestamp@ and the current time. For more
-- information about restore points, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/PointInTimeRecovery_HowItWorks.html#howitworks_backup_window Time window for PITR continuous backups>
-- in the /Amazon Keyspaces Developer Guide/.
--
-- Any number of users can execute up to 4 concurrent restores (any type of
-- restore) in a given account.
--
-- When you restore using point in time recovery, Amazon Keyspaces restores
-- your source table\'s schema and data to the state based on the selected
-- timestamp @(day:hour:minute:second)@ to a new table. The Time to Live
-- (TTL) settings are also restored to the state based on the selected
-- timestamp.
--
-- In addition to the table\'s schema, data, and TTL settings,
-- @RestoreTable@ restores the capacity mode, encryption, and point-in-time
-- recovery settings from the source table. Unlike the table\'s schema data
-- and TTL settings, which are restored based on the selected timestamp,
-- these settings are always restored based on the table\'s settings as of
-- the current time or when the table was deleted.
--
-- You can also overwrite these settings during restore:
--
-- • Read\/write capacity mode
--
-- • Provisioned throughput capacity settings
--
-- • Point-in-time (PITR) settings
--
-- • Tags
--
-- For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/PointInTimeRecovery_HowItWorks.html#howitworks_backup_settings PITR restore settings>
-- in the /Amazon Keyspaces Developer Guide/.
--
-- Note that the following settings are not restored, and you must
-- configure them manually for the new table:
--
-- • Automatic scaling policies (for tables that use provisioned capacity
-- mode)
--
-- • Identity and Access Management (IAM) policies
--
-- • Amazon CloudWatch metrics and alarms
module Amazonka.KeySpaces.RestoreTable
  ( -- * Creating a Request
    RestoreTable (..),
    newRestoreTable,

    -- * Request Lenses
    restoreTable_encryptionSpecificationOverride,
    restoreTable_capacitySpecificationOverride,
    restoreTable_tagsOverride,
    restoreTable_restoreTimestamp,
    restoreTable_pointInTimeRecoveryOverride,
    restoreTable_sourceKeyspaceName,
    restoreTable_sourceTableName,
    restoreTable_targetKeyspaceName,
    restoreTable_targetTableName,

    -- * Destructuring the Response
    RestoreTableResponse (..),
    newRestoreTableResponse,

    -- * Response Lenses
    restoreTableResponse_httpStatus,
    restoreTableResponse_restoredTableARN,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KeySpaces.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRestoreTable' smart constructor.
data RestoreTable = RestoreTable'
  { -- | Specifies the encryption settings for the target table. You can choose
    -- one of the following KMS key (KMS key):
    --
    -- • @type:AWS_OWNED_KMS_KEY@ - This key is owned by Amazon Keyspaces.
    --
    -- • @type:CUSTOMER_MANAGED_KMS_KEY@ - This key is stored in your account
    -- and is created, owned, and managed by you. This option requires the
    -- @kms_key_identifier@ of the KMS key in Amazon Resource Name (ARN) format
    -- as input.
    --
    -- The default is @type:AWS_OWNED_KMS_KEY@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/keyspaces/latest/devguide/EncryptionAtRest.html Encryption at rest>
    -- in the /Amazon Keyspaces Developer Guide/.
    encryptionSpecificationOverride :: Prelude.Maybe EncryptionSpecification,
    -- | Specifies the read\/write throughput capacity mode for the target table.
    -- The options are:
    --
    -- • @throughputMode:PAY_PER_REQUEST@
    --
    -- • @throughputMode:PROVISIONED@ - Provisioned capacity mode requires
    -- @readCapacityUnits@ and @writeCapacityUnits@ as input.
    --
    -- The default is @throughput_mode:PAY_PER_REQUEST@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/keyspaces/latest/devguide/ReadWriteCapacityMode.html Read\/write capacity modes>
    -- in the /Amazon Keyspaces Developer Guide/.
    capacitySpecificationOverride :: Prelude.Maybe CapacitySpecification,
    -- | A list of key-value pair tags to be attached to the restored table.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/keyspaces/latest/devguide/tagging-keyspaces.html Adding tags and labels to Amazon Keyspaces resources>
    -- in the /Amazon Keyspaces Developer Guide/.
    tagsOverride :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The restore timestamp in ISO 8601 format.
    restoreTimestamp :: Prelude.Maybe Core.POSIX,
    -- | Specifies the @pointInTimeRecovery@ settings for the target table. The
    -- options are:
    --
    -- • @ENABLED@
    --
    -- • @DISABLED@
    --
    -- If it\'s not specified, the default is @DISABLED@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/keyspaces/latest/devguide/PointInTimeRecovery.html Point-in-time recovery>
    -- in the /Amazon Keyspaces Developer Guide/.
    pointInTimeRecoveryOverride :: Prelude.Maybe PointInTimeRecovery,
    -- | The keyspace name of the source table.
    sourceKeyspaceName :: Prelude.Text,
    -- | The name of the source table.
    sourceTableName :: Prelude.Text,
    -- | The name of the target keyspace.
    targetKeyspaceName :: Prelude.Text,
    -- | The name of the target table.
    targetTableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionSpecificationOverride', 'restoreTable_encryptionSpecificationOverride' - Specifies the encryption settings for the target table. You can choose
-- one of the following KMS key (KMS key):
--
-- • @type:AWS_OWNED_KMS_KEY@ - This key is owned by Amazon Keyspaces.
--
-- • @type:CUSTOMER_MANAGED_KMS_KEY@ - This key is stored in your account
-- and is created, owned, and managed by you. This option requires the
-- @kms_key_identifier@ of the KMS key in Amazon Resource Name (ARN) format
-- as input.
--
-- The default is @type:AWS_OWNED_KMS_KEY@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/EncryptionAtRest.html Encryption at rest>
-- in the /Amazon Keyspaces Developer Guide/.
--
-- 'capacitySpecificationOverride', 'restoreTable_capacitySpecificationOverride' - Specifies the read\/write throughput capacity mode for the target table.
-- The options are:
--
-- • @throughputMode:PAY_PER_REQUEST@
--
-- • @throughputMode:PROVISIONED@ - Provisioned capacity mode requires
-- @readCapacityUnits@ and @writeCapacityUnits@ as input.
--
-- The default is @throughput_mode:PAY_PER_REQUEST@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/ReadWriteCapacityMode.html Read\/write capacity modes>
-- in the /Amazon Keyspaces Developer Guide/.
--
-- 'tagsOverride', 'restoreTable_tagsOverride' - A list of key-value pair tags to be attached to the restored table.
--
-- For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/tagging-keyspaces.html Adding tags and labels to Amazon Keyspaces resources>
-- in the /Amazon Keyspaces Developer Guide/.
--
-- 'restoreTimestamp', 'restoreTable_restoreTimestamp' - The restore timestamp in ISO 8601 format.
--
-- 'pointInTimeRecoveryOverride', 'restoreTable_pointInTimeRecoveryOverride' - Specifies the @pointInTimeRecovery@ settings for the target table. The
-- options are:
--
-- • @ENABLED@
--
-- • @DISABLED@
--
-- If it\'s not specified, the default is @DISABLED@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/PointInTimeRecovery.html Point-in-time recovery>
-- in the /Amazon Keyspaces Developer Guide/.
--
-- 'sourceKeyspaceName', 'restoreTable_sourceKeyspaceName' - The keyspace name of the source table.
--
-- 'sourceTableName', 'restoreTable_sourceTableName' - The name of the source table.
--
-- 'targetKeyspaceName', 'restoreTable_targetKeyspaceName' - The name of the target keyspace.
--
-- 'targetTableName', 'restoreTable_targetTableName' - The name of the target table.
newRestoreTable ::
  -- | 'sourceKeyspaceName'
  Prelude.Text ->
  -- | 'sourceTableName'
  Prelude.Text ->
  -- | 'targetKeyspaceName'
  Prelude.Text ->
  -- | 'targetTableName'
  Prelude.Text ->
  RestoreTable
newRestoreTable
  pSourceKeyspaceName_
  pSourceTableName_
  pTargetKeyspaceName_
  pTargetTableName_ =
    RestoreTable'
      { encryptionSpecificationOverride =
          Prelude.Nothing,
        capacitySpecificationOverride = Prelude.Nothing,
        tagsOverride = Prelude.Nothing,
        restoreTimestamp = Prelude.Nothing,
        pointInTimeRecoveryOverride = Prelude.Nothing,
        sourceKeyspaceName = pSourceKeyspaceName_,
        sourceTableName = pSourceTableName_,
        targetKeyspaceName = pTargetKeyspaceName_,
        targetTableName = pTargetTableName_
      }

-- | Specifies the encryption settings for the target table. You can choose
-- one of the following KMS key (KMS key):
--
-- • @type:AWS_OWNED_KMS_KEY@ - This key is owned by Amazon Keyspaces.
--
-- • @type:CUSTOMER_MANAGED_KMS_KEY@ - This key is stored in your account
-- and is created, owned, and managed by you. This option requires the
-- @kms_key_identifier@ of the KMS key in Amazon Resource Name (ARN) format
-- as input.
--
-- The default is @type:AWS_OWNED_KMS_KEY@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/EncryptionAtRest.html Encryption at rest>
-- in the /Amazon Keyspaces Developer Guide/.
restoreTable_encryptionSpecificationOverride :: Lens.Lens' RestoreTable (Prelude.Maybe EncryptionSpecification)
restoreTable_encryptionSpecificationOverride = Lens.lens (\RestoreTable' {encryptionSpecificationOverride} -> encryptionSpecificationOverride) (\s@RestoreTable' {} a -> s {encryptionSpecificationOverride = a} :: RestoreTable)

-- | Specifies the read\/write throughput capacity mode for the target table.
-- The options are:
--
-- • @throughputMode:PAY_PER_REQUEST@
--
-- • @throughputMode:PROVISIONED@ - Provisioned capacity mode requires
-- @readCapacityUnits@ and @writeCapacityUnits@ as input.
--
-- The default is @throughput_mode:PAY_PER_REQUEST@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/ReadWriteCapacityMode.html Read\/write capacity modes>
-- in the /Amazon Keyspaces Developer Guide/.
restoreTable_capacitySpecificationOverride :: Lens.Lens' RestoreTable (Prelude.Maybe CapacitySpecification)
restoreTable_capacitySpecificationOverride = Lens.lens (\RestoreTable' {capacitySpecificationOverride} -> capacitySpecificationOverride) (\s@RestoreTable' {} a -> s {capacitySpecificationOverride = a} :: RestoreTable)

-- | A list of key-value pair tags to be attached to the restored table.
--
-- For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/tagging-keyspaces.html Adding tags and labels to Amazon Keyspaces resources>
-- in the /Amazon Keyspaces Developer Guide/.
restoreTable_tagsOverride :: Lens.Lens' RestoreTable (Prelude.Maybe (Prelude.NonEmpty Tag))
restoreTable_tagsOverride = Lens.lens (\RestoreTable' {tagsOverride} -> tagsOverride) (\s@RestoreTable' {} a -> s {tagsOverride = a} :: RestoreTable) Prelude.. Lens.mapping Lens.coerced

-- | The restore timestamp in ISO 8601 format.
restoreTable_restoreTimestamp :: Lens.Lens' RestoreTable (Prelude.Maybe Prelude.UTCTime)
restoreTable_restoreTimestamp = Lens.lens (\RestoreTable' {restoreTimestamp} -> restoreTimestamp) (\s@RestoreTable' {} a -> s {restoreTimestamp = a} :: RestoreTable) Prelude.. Lens.mapping Core._Time

-- | Specifies the @pointInTimeRecovery@ settings for the target table. The
-- options are:
--
-- • @ENABLED@
--
-- • @DISABLED@
--
-- If it\'s not specified, the default is @DISABLED@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/PointInTimeRecovery.html Point-in-time recovery>
-- in the /Amazon Keyspaces Developer Guide/.
restoreTable_pointInTimeRecoveryOverride :: Lens.Lens' RestoreTable (Prelude.Maybe PointInTimeRecovery)
restoreTable_pointInTimeRecoveryOverride = Lens.lens (\RestoreTable' {pointInTimeRecoveryOverride} -> pointInTimeRecoveryOverride) (\s@RestoreTable' {} a -> s {pointInTimeRecoveryOverride = a} :: RestoreTable)

-- | The keyspace name of the source table.
restoreTable_sourceKeyspaceName :: Lens.Lens' RestoreTable Prelude.Text
restoreTable_sourceKeyspaceName = Lens.lens (\RestoreTable' {sourceKeyspaceName} -> sourceKeyspaceName) (\s@RestoreTable' {} a -> s {sourceKeyspaceName = a} :: RestoreTable)

-- | The name of the source table.
restoreTable_sourceTableName :: Lens.Lens' RestoreTable Prelude.Text
restoreTable_sourceTableName = Lens.lens (\RestoreTable' {sourceTableName} -> sourceTableName) (\s@RestoreTable' {} a -> s {sourceTableName = a} :: RestoreTable)

-- | The name of the target keyspace.
restoreTable_targetKeyspaceName :: Lens.Lens' RestoreTable Prelude.Text
restoreTable_targetKeyspaceName = Lens.lens (\RestoreTable' {targetKeyspaceName} -> targetKeyspaceName) (\s@RestoreTable' {} a -> s {targetKeyspaceName = a} :: RestoreTable)

-- | The name of the target table.
restoreTable_targetTableName :: Lens.Lens' RestoreTable Prelude.Text
restoreTable_targetTableName = Lens.lens (\RestoreTable' {targetTableName} -> targetTableName) (\s@RestoreTable' {} a -> s {targetTableName = a} :: RestoreTable)

instance Core.AWSRequest RestoreTable where
  type AWSResponse RestoreTable = RestoreTableResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RestoreTableResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "restoredTableARN")
      )

instance Prelude.Hashable RestoreTable where
  hashWithSalt _salt RestoreTable' {..} =
    _salt
      `Prelude.hashWithSalt` encryptionSpecificationOverride
      `Prelude.hashWithSalt` capacitySpecificationOverride
      `Prelude.hashWithSalt` tagsOverride
      `Prelude.hashWithSalt` restoreTimestamp
      `Prelude.hashWithSalt` pointInTimeRecoveryOverride
      `Prelude.hashWithSalt` sourceKeyspaceName
      `Prelude.hashWithSalt` sourceTableName
      `Prelude.hashWithSalt` targetKeyspaceName
      `Prelude.hashWithSalt` targetTableName

instance Prelude.NFData RestoreTable where
  rnf RestoreTable' {..} =
    Prelude.rnf encryptionSpecificationOverride
      `Prelude.seq` Prelude.rnf capacitySpecificationOverride
      `Prelude.seq` Prelude.rnf tagsOverride
      `Prelude.seq` Prelude.rnf restoreTimestamp
      `Prelude.seq` Prelude.rnf pointInTimeRecoveryOverride
      `Prelude.seq` Prelude.rnf sourceKeyspaceName
      `Prelude.seq` Prelude.rnf sourceTableName
      `Prelude.seq` Prelude.rnf targetKeyspaceName
      `Prelude.seq` Prelude.rnf targetTableName

instance Core.ToHeaders RestoreTable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "KeyspacesService.RestoreTable" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RestoreTable where
  toJSON RestoreTable' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("encryptionSpecificationOverride" Core..=)
              Prelude.<$> encryptionSpecificationOverride,
            ("capacitySpecificationOverride" Core..=)
              Prelude.<$> capacitySpecificationOverride,
            ("tagsOverride" Core..=) Prelude.<$> tagsOverride,
            ("restoreTimestamp" Core..=)
              Prelude.<$> restoreTimestamp,
            ("pointInTimeRecoveryOverride" Core..=)
              Prelude.<$> pointInTimeRecoveryOverride,
            Prelude.Just
              ("sourceKeyspaceName" Core..= sourceKeyspaceName),
            Prelude.Just
              ("sourceTableName" Core..= sourceTableName),
            Prelude.Just
              ("targetKeyspaceName" Core..= targetKeyspaceName),
            Prelude.Just
              ("targetTableName" Core..= targetTableName)
          ]
      )

instance Core.ToPath RestoreTable where
  toPath = Prelude.const "/"

instance Core.ToQuery RestoreTable where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRestoreTableResponse' smart constructor.
data RestoreTableResponse = RestoreTableResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the restored table.
    restoredTableARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'restoreTableResponse_httpStatus' - The response's http status code.
--
-- 'restoredTableARN', 'restoreTableResponse_restoredTableARN' - The Amazon Resource Name (ARN) of the restored table.
newRestoreTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'restoredTableARN'
  Prelude.Text ->
  RestoreTableResponse
newRestoreTableResponse
  pHttpStatus_
  pRestoredTableARN_ =
    RestoreTableResponse'
      { httpStatus = pHttpStatus_,
        restoredTableARN = pRestoredTableARN_
      }

-- | The response's http status code.
restoreTableResponse_httpStatus :: Lens.Lens' RestoreTableResponse Prelude.Int
restoreTableResponse_httpStatus = Lens.lens (\RestoreTableResponse' {httpStatus} -> httpStatus) (\s@RestoreTableResponse' {} a -> s {httpStatus = a} :: RestoreTableResponse)

-- | The Amazon Resource Name (ARN) of the restored table.
restoreTableResponse_restoredTableARN :: Lens.Lens' RestoreTableResponse Prelude.Text
restoreTableResponse_restoredTableARN = Lens.lens (\RestoreTableResponse' {restoredTableARN} -> restoredTableARN) (\s@RestoreTableResponse' {} a -> s {restoredTableARN = a} :: RestoreTableResponse)

instance Prelude.NFData RestoreTableResponse where
  rnf RestoreTableResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf restoredTableARN
