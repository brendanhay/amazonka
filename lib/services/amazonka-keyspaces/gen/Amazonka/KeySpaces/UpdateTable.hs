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
-- Module      : Amazonka.KeySpaces.UpdateTable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds new columns to the table or updates one of the table\'s settings,
-- for example capacity mode, encryption, point-in-time recovery, or ttl
-- settings. Note that you can only update one specific table setting per
-- update operation.
module Amazonka.KeySpaces.UpdateTable
  ( -- * Creating a Request
    UpdateTable (..),
    newUpdateTable,

    -- * Request Lenses
    updateTable_addColumns,
    updateTable_capacitySpecification,
    updateTable_defaultTimeToLive,
    updateTable_encryptionSpecification,
    updateTable_pointInTimeRecovery,
    updateTable_ttl,
    updateTable_keyspaceName,
    updateTable_tableName,

    -- * Destructuring the Response
    UpdateTableResponse (..),
    newUpdateTableResponse,

    -- * Response Lenses
    updateTableResponse_httpStatus,
    updateTableResponse_resourceArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KeySpaces.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateTable' smart constructor.
data UpdateTable = UpdateTable'
  { -- | For each column to be added to the specified table:
    --
    -- • @name@ - The name of the column.
    --
    -- • @type@ - An Amazon Keyspaces data type. For more information, see
    -- <https://docs.aws.amazon.com/keyspaces/latest/devguide/cql.elements.html#cql.data-types Data types>
    -- in the /Amazon Keyspaces Developer Guide/.
    addColumns :: Prelude.Maybe (Prelude.NonEmpty ColumnDefinition),
    -- | Modifies the read\/write throughput capacity mode for the table. The
    -- options are:
    --
    -- • @throughputMode:PAY_PER_REQUEST@ and
    --
    -- • @throughputMode:PROVISIONED@ - Provisioned capacity mode requires
    -- @readCapacityUnits@ and @writeCapacityUnits@ as input.
    --
    -- The default is @throughput_mode:PAY_PER_REQUEST@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/keyspaces/latest/devguide/ReadWriteCapacityMode.html Read\/write capacity modes>
    -- in the /Amazon Keyspaces Developer Guide/.
    capacitySpecification :: Prelude.Maybe CapacitySpecification,
    -- | The default Time to Live setting in seconds for the table.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/keyspaces/latest/devguide/TTL-how-it-works.html#ttl-howitworks_default_ttl Setting the default TTL value for a table>
    -- in the /Amazon Keyspaces Developer Guide/.
    defaultTimeToLive :: Prelude.Maybe Prelude.Natural,
    -- | Modifies the encryption settings of the table. You can choose one of the
    -- following KMS key (KMS key):
    --
    -- • @type:AWS_OWNED_KMS_KEY@ - This key is owned by Amazon Keyspaces.
    --
    -- • @type:CUSTOMER_MANAGED_KMS_KEY@ - This key is stored in your account
    -- and is created, owned, and managed by you. This option requires the
    -- @kms_key_identifier@ of the KMS key in Amazon Resource Name (ARN) format
    -- as input.
    --
    -- The default is @AWS_OWNED_KMS_KEY@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/keyspaces/latest/devguide/EncryptionAtRest.html Encryption at rest>
    -- in the /Amazon Keyspaces Developer Guide/.
    encryptionSpecification :: Prelude.Maybe EncryptionSpecification,
    -- | Modifies the @pointInTimeRecovery@ settings of the table. The options
    -- are:
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
    pointInTimeRecovery :: Prelude.Maybe PointInTimeRecovery,
    -- | Modifies Time to Live custom settings for the table. The options are:
    --
    -- • @status:enabled@
    --
    -- • @status:disabled@
    --
    -- The default is @status:disabled@. After @ttl@ is enabled, you can\'t
    -- disable it for the table.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/keyspaces/latest/devguide/TTL.html Expiring data by using Amazon Keyspaces Time to Live (TTL)>
    -- in the /Amazon Keyspaces Developer Guide/.
    ttl :: Prelude.Maybe TimeToLive,
    -- | The name of the keyspace the specified table is stored in.
    keyspaceName :: Prelude.Text,
    -- | The name of the table.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addColumns', 'updateTable_addColumns' - For each column to be added to the specified table:
--
-- • @name@ - The name of the column.
--
-- • @type@ - An Amazon Keyspaces data type. For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/cql.elements.html#cql.data-types Data types>
-- in the /Amazon Keyspaces Developer Guide/.
--
-- 'capacitySpecification', 'updateTable_capacitySpecification' - Modifies the read\/write throughput capacity mode for the table. The
-- options are:
--
-- • @throughputMode:PAY_PER_REQUEST@ and
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
-- 'defaultTimeToLive', 'updateTable_defaultTimeToLive' - The default Time to Live setting in seconds for the table.
--
-- For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/TTL-how-it-works.html#ttl-howitworks_default_ttl Setting the default TTL value for a table>
-- in the /Amazon Keyspaces Developer Guide/.
--
-- 'encryptionSpecification', 'updateTable_encryptionSpecification' - Modifies the encryption settings of the table. You can choose one of the
-- following KMS key (KMS key):
--
-- • @type:AWS_OWNED_KMS_KEY@ - This key is owned by Amazon Keyspaces.
--
-- • @type:CUSTOMER_MANAGED_KMS_KEY@ - This key is stored in your account
-- and is created, owned, and managed by you. This option requires the
-- @kms_key_identifier@ of the KMS key in Amazon Resource Name (ARN) format
-- as input.
--
-- The default is @AWS_OWNED_KMS_KEY@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/EncryptionAtRest.html Encryption at rest>
-- in the /Amazon Keyspaces Developer Guide/.
--
-- 'pointInTimeRecovery', 'updateTable_pointInTimeRecovery' - Modifies the @pointInTimeRecovery@ settings of the table. The options
-- are:
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
-- 'ttl', 'updateTable_ttl' - Modifies Time to Live custom settings for the table. The options are:
--
-- • @status:enabled@
--
-- • @status:disabled@
--
-- The default is @status:disabled@. After @ttl@ is enabled, you can\'t
-- disable it for the table.
--
-- For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/TTL.html Expiring data by using Amazon Keyspaces Time to Live (TTL)>
-- in the /Amazon Keyspaces Developer Guide/.
--
-- 'keyspaceName', 'updateTable_keyspaceName' - The name of the keyspace the specified table is stored in.
--
-- 'tableName', 'updateTable_tableName' - The name of the table.
newUpdateTable ::
  -- | 'keyspaceName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  UpdateTable
newUpdateTable pKeyspaceName_ pTableName_ =
  UpdateTable'
    { addColumns = Prelude.Nothing,
      capacitySpecification = Prelude.Nothing,
      defaultTimeToLive = Prelude.Nothing,
      encryptionSpecification = Prelude.Nothing,
      pointInTimeRecovery = Prelude.Nothing,
      ttl = Prelude.Nothing,
      keyspaceName = pKeyspaceName_,
      tableName = pTableName_
    }

-- | For each column to be added to the specified table:
--
-- • @name@ - The name of the column.
--
-- • @type@ - An Amazon Keyspaces data type. For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/cql.elements.html#cql.data-types Data types>
-- in the /Amazon Keyspaces Developer Guide/.
updateTable_addColumns :: Lens.Lens' UpdateTable (Prelude.Maybe (Prelude.NonEmpty ColumnDefinition))
updateTable_addColumns = Lens.lens (\UpdateTable' {addColumns} -> addColumns) (\s@UpdateTable' {} a -> s {addColumns = a} :: UpdateTable) Prelude.. Lens.mapping Lens.coerced

-- | Modifies the read\/write throughput capacity mode for the table. The
-- options are:
--
-- • @throughputMode:PAY_PER_REQUEST@ and
--
-- • @throughputMode:PROVISIONED@ - Provisioned capacity mode requires
-- @readCapacityUnits@ and @writeCapacityUnits@ as input.
--
-- The default is @throughput_mode:PAY_PER_REQUEST@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/ReadWriteCapacityMode.html Read\/write capacity modes>
-- in the /Amazon Keyspaces Developer Guide/.
updateTable_capacitySpecification :: Lens.Lens' UpdateTable (Prelude.Maybe CapacitySpecification)
updateTable_capacitySpecification = Lens.lens (\UpdateTable' {capacitySpecification} -> capacitySpecification) (\s@UpdateTable' {} a -> s {capacitySpecification = a} :: UpdateTable)

-- | The default Time to Live setting in seconds for the table.
--
-- For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/TTL-how-it-works.html#ttl-howitworks_default_ttl Setting the default TTL value for a table>
-- in the /Amazon Keyspaces Developer Guide/.
updateTable_defaultTimeToLive :: Lens.Lens' UpdateTable (Prelude.Maybe Prelude.Natural)
updateTable_defaultTimeToLive = Lens.lens (\UpdateTable' {defaultTimeToLive} -> defaultTimeToLive) (\s@UpdateTable' {} a -> s {defaultTimeToLive = a} :: UpdateTable)

-- | Modifies the encryption settings of the table. You can choose one of the
-- following KMS key (KMS key):
--
-- • @type:AWS_OWNED_KMS_KEY@ - This key is owned by Amazon Keyspaces.
--
-- • @type:CUSTOMER_MANAGED_KMS_KEY@ - This key is stored in your account
-- and is created, owned, and managed by you. This option requires the
-- @kms_key_identifier@ of the KMS key in Amazon Resource Name (ARN) format
-- as input.
--
-- The default is @AWS_OWNED_KMS_KEY@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/EncryptionAtRest.html Encryption at rest>
-- in the /Amazon Keyspaces Developer Guide/.
updateTable_encryptionSpecification :: Lens.Lens' UpdateTable (Prelude.Maybe EncryptionSpecification)
updateTable_encryptionSpecification = Lens.lens (\UpdateTable' {encryptionSpecification} -> encryptionSpecification) (\s@UpdateTable' {} a -> s {encryptionSpecification = a} :: UpdateTable)

-- | Modifies the @pointInTimeRecovery@ settings of the table. The options
-- are:
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
updateTable_pointInTimeRecovery :: Lens.Lens' UpdateTable (Prelude.Maybe PointInTimeRecovery)
updateTable_pointInTimeRecovery = Lens.lens (\UpdateTable' {pointInTimeRecovery} -> pointInTimeRecovery) (\s@UpdateTable' {} a -> s {pointInTimeRecovery = a} :: UpdateTable)

-- | Modifies Time to Live custom settings for the table. The options are:
--
-- • @status:enabled@
--
-- • @status:disabled@
--
-- The default is @status:disabled@. After @ttl@ is enabled, you can\'t
-- disable it for the table.
--
-- For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/TTL.html Expiring data by using Amazon Keyspaces Time to Live (TTL)>
-- in the /Amazon Keyspaces Developer Guide/.
updateTable_ttl :: Lens.Lens' UpdateTable (Prelude.Maybe TimeToLive)
updateTable_ttl = Lens.lens (\UpdateTable' {ttl} -> ttl) (\s@UpdateTable' {} a -> s {ttl = a} :: UpdateTable)

-- | The name of the keyspace the specified table is stored in.
updateTable_keyspaceName :: Lens.Lens' UpdateTable Prelude.Text
updateTable_keyspaceName = Lens.lens (\UpdateTable' {keyspaceName} -> keyspaceName) (\s@UpdateTable' {} a -> s {keyspaceName = a} :: UpdateTable)

-- | The name of the table.
updateTable_tableName :: Lens.Lens' UpdateTable Prelude.Text
updateTable_tableName = Lens.lens (\UpdateTable' {tableName} -> tableName) (\s@UpdateTable' {} a -> s {tableName = a} :: UpdateTable)

instance Core.AWSRequest UpdateTable where
  type AWSResponse UpdateTable = UpdateTableResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTableResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "resourceArn")
      )

instance Prelude.Hashable UpdateTable where
  hashWithSalt _salt UpdateTable' {..} =
    _salt
      `Prelude.hashWithSalt` addColumns
      `Prelude.hashWithSalt` capacitySpecification
      `Prelude.hashWithSalt` defaultTimeToLive
      `Prelude.hashWithSalt` encryptionSpecification
      `Prelude.hashWithSalt` pointInTimeRecovery
      `Prelude.hashWithSalt` ttl
      `Prelude.hashWithSalt` keyspaceName
      `Prelude.hashWithSalt` tableName

instance Prelude.NFData UpdateTable where
  rnf UpdateTable' {..} =
    Prelude.rnf addColumns `Prelude.seq`
      Prelude.rnf capacitySpecification `Prelude.seq`
        Prelude.rnf defaultTimeToLive `Prelude.seq`
          Prelude.rnf encryptionSpecification `Prelude.seq`
            Prelude.rnf pointInTimeRecovery `Prelude.seq`
              Prelude.rnf ttl `Prelude.seq`
                Prelude.rnf keyspaceName `Prelude.seq`
                  Prelude.rnf tableName

instance Data.ToHeaders UpdateTable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "KeyspacesService.UpdateTable" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateTable where
  toJSON UpdateTable' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("addColumns" Data..=) Prelude.<$> addColumns,
            ("capacitySpecification" Data..=)
              Prelude.<$> capacitySpecification,
            ("defaultTimeToLive" Data..=)
              Prelude.<$> defaultTimeToLive,
            ("encryptionSpecification" Data..=)
              Prelude.<$> encryptionSpecification,
            ("pointInTimeRecovery" Data..=)
              Prelude.<$> pointInTimeRecovery,
            ("ttl" Data..=) Prelude.<$> ttl,
            Prelude.Just ("keyspaceName" Data..= keyspaceName),
            Prelude.Just ("tableName" Data..= tableName)
          ]
      )

instance Data.ToPath UpdateTable where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateTable where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTableResponse' smart constructor.
data UpdateTableResponse = UpdateTableResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the modified table.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateTableResponse_httpStatus' - The response's http status code.
--
-- 'resourceArn', 'updateTableResponse_resourceArn' - The Amazon Resource Name (ARN) of the modified table.
newUpdateTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'resourceArn'
  Prelude.Text ->
  UpdateTableResponse
newUpdateTableResponse pHttpStatus_ pResourceArn_ =
  UpdateTableResponse'
    { httpStatus = pHttpStatus_,
      resourceArn = pResourceArn_
    }

-- | The response's http status code.
updateTableResponse_httpStatus :: Lens.Lens' UpdateTableResponse Prelude.Int
updateTableResponse_httpStatus = Lens.lens (\UpdateTableResponse' {httpStatus} -> httpStatus) (\s@UpdateTableResponse' {} a -> s {httpStatus = a} :: UpdateTableResponse)

-- | The Amazon Resource Name (ARN) of the modified table.
updateTableResponse_resourceArn :: Lens.Lens' UpdateTableResponse Prelude.Text
updateTableResponse_resourceArn = Lens.lens (\UpdateTableResponse' {resourceArn} -> resourceArn) (\s@UpdateTableResponse' {} a -> s {resourceArn = a} :: UpdateTableResponse)

instance Prelude.NFData UpdateTableResponse where
  rnf UpdateTableResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf resourceArn
