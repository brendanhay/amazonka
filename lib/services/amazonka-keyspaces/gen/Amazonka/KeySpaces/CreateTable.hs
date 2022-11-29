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
-- Module      : Amazonka.KeySpaces.CreateTable
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @CreateTable@ operation adds a new table to the specified keyspace.
-- Within a keyspace, table names must be unique.
--
-- @CreateTable@ is an asynchronous operation. When the request is
-- received, the status of the table is set to @CREATING@. You can monitor
-- the creation status of the new table by using the @GetTable@ operation,
-- which returns the current @status@ of the table. You can start using a
-- table when the status is @ACTIVE@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/working-with-tables.html#tables-create Creating tables>
-- in the /Amazon Keyspaces Developer Guide/.
module Amazonka.KeySpaces.CreateTable
  ( -- * Creating a Request
    CreateTable (..),
    newCreateTable,

    -- * Request Lenses
    createTable_tags,
    createTable_ttl,
    createTable_pointInTimeRecovery,
    createTable_capacitySpecification,
    createTable_encryptionSpecification,
    createTable_comment,
    createTable_defaultTimeToLive,
    createTable_keyspaceName,
    createTable_tableName,
    createTable_schemaDefinition,

    -- * Destructuring the Response
    CreateTableResponse (..),
    newCreateTableResponse,

    -- * Response Lenses
    createTableResponse_httpStatus,
    createTableResponse_resourceArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KeySpaces.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTable' smart constructor.
data CreateTable = CreateTable'
  { -- | A list of key-value pair tags to be attached to the resource.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/keyspaces/latest/devguide/tagging-keyspaces.html Adding tags and labels to Amazon Keyspaces resources>
    -- in the /Amazon Keyspaces Developer Guide/.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | Enables Time to Live custom settings for the table. The options are:
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
    -- | Specifies if @pointInTimeRecovery@ is enabled or disabled for the table.
    -- The options are:
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
    -- | Specifies the read\/write throughput capacity mode for the table. The
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
    -- | Specifies how the encryption key for encryption at rest is managed for
    -- the table. You can choose one of the following KMS key (KMS key):
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
    encryptionSpecification :: Prelude.Maybe EncryptionSpecification,
    -- | This parameter allows to enter a description of the table.
    comment :: Prelude.Maybe Comment,
    -- | The default Time to Live setting in seconds for the table.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/keyspaces/latest/devguide/TTL-how-it-works.html#ttl-howitworks_default_ttl Setting the default TTL value for a table>
    -- in the /Amazon Keyspaces Developer Guide/.
    defaultTimeToLive :: Prelude.Maybe Prelude.Natural,
    -- | The name of the keyspace that the table is going to be created in.
    keyspaceName :: Prelude.Text,
    -- | The name of the table.
    tableName :: Prelude.Text,
    -- | The @schemaDefinition@ consists of the following parameters.
    --
    -- For each column to be created:
    --
    -- • @name@ - The name of the column.
    --
    -- • @type@ - An Amazon Keyspaces data type. For more information, see
    -- <https://docs.aws.amazon.com/keyspaces/latest/devguide/cql.elements.html#cql.data-types Data types>
    -- in the /Amazon Keyspaces Developer Guide/.
    --
    -- The primary key of the table consists of the following columns:
    --
    -- • @partitionKeys@ - The partition key can be a single column, or it can
    -- be a compound value composed of two or more columns. The partition key
    -- portion of the primary key is required and determines how Amazon
    -- Keyspaces stores your data.
    --
    -- • @name@ - The name of each partition key column.
    --
    -- • @clusteringKeys@ - The optional clustering column portion of your
    -- primary key determines how the data is clustered and sorted within each
    -- partition.
    --
    -- • @name@ - The name of the clustering column.
    --
    -- • @orderBy@ - Sets the ascendant (@ASC@) or descendant (@DESC@) order
    -- modifier.
    --
    -- To define a column as static use @staticColumns@ - Static columns store
    -- values that are shared by all rows in the same partition:
    --
    -- • @name@ - The name of the column.
    --
    -- • @type@ - An Amazon Keyspaces data type.
    schemaDefinition :: SchemaDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createTable_tags' - A list of key-value pair tags to be attached to the resource.
--
-- For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/tagging-keyspaces.html Adding tags and labels to Amazon Keyspaces resources>
-- in the /Amazon Keyspaces Developer Guide/.
--
-- 'ttl', 'createTable_ttl' - Enables Time to Live custom settings for the table. The options are:
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
-- 'pointInTimeRecovery', 'createTable_pointInTimeRecovery' - Specifies if @pointInTimeRecovery@ is enabled or disabled for the table.
-- The options are:
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
-- 'capacitySpecification', 'createTable_capacitySpecification' - Specifies the read\/write throughput capacity mode for the table. The
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
-- 'encryptionSpecification', 'createTable_encryptionSpecification' - Specifies how the encryption key for encryption at rest is managed for
-- the table. You can choose one of the following KMS key (KMS key):
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
-- 'comment', 'createTable_comment' - This parameter allows to enter a description of the table.
--
-- 'defaultTimeToLive', 'createTable_defaultTimeToLive' - The default Time to Live setting in seconds for the table.
--
-- For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/TTL-how-it-works.html#ttl-howitworks_default_ttl Setting the default TTL value for a table>
-- in the /Amazon Keyspaces Developer Guide/.
--
-- 'keyspaceName', 'createTable_keyspaceName' - The name of the keyspace that the table is going to be created in.
--
-- 'tableName', 'createTable_tableName' - The name of the table.
--
-- 'schemaDefinition', 'createTable_schemaDefinition' - The @schemaDefinition@ consists of the following parameters.
--
-- For each column to be created:
--
-- • @name@ - The name of the column.
--
-- • @type@ - An Amazon Keyspaces data type. For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/cql.elements.html#cql.data-types Data types>
-- in the /Amazon Keyspaces Developer Guide/.
--
-- The primary key of the table consists of the following columns:
--
-- • @partitionKeys@ - The partition key can be a single column, or it can
-- be a compound value composed of two or more columns. The partition key
-- portion of the primary key is required and determines how Amazon
-- Keyspaces stores your data.
--
-- • @name@ - The name of each partition key column.
--
-- • @clusteringKeys@ - The optional clustering column portion of your
-- primary key determines how the data is clustered and sorted within each
-- partition.
--
-- • @name@ - The name of the clustering column.
--
-- • @orderBy@ - Sets the ascendant (@ASC@) or descendant (@DESC@) order
-- modifier.
--
-- To define a column as static use @staticColumns@ - Static columns store
-- values that are shared by all rows in the same partition:
--
-- • @name@ - The name of the column.
--
-- • @type@ - An Amazon Keyspaces data type.
newCreateTable ::
  -- | 'keyspaceName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  -- | 'schemaDefinition'
  SchemaDefinition ->
  CreateTable
newCreateTable
  pKeyspaceName_
  pTableName_
  pSchemaDefinition_ =
    CreateTable'
      { tags = Prelude.Nothing,
        ttl = Prelude.Nothing,
        pointInTimeRecovery = Prelude.Nothing,
        capacitySpecification = Prelude.Nothing,
        encryptionSpecification = Prelude.Nothing,
        comment = Prelude.Nothing,
        defaultTimeToLive = Prelude.Nothing,
        keyspaceName = pKeyspaceName_,
        tableName = pTableName_,
        schemaDefinition = pSchemaDefinition_
      }

-- | A list of key-value pair tags to be attached to the resource.
--
-- For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/tagging-keyspaces.html Adding tags and labels to Amazon Keyspaces resources>
-- in the /Amazon Keyspaces Developer Guide/.
createTable_tags :: Lens.Lens' CreateTable (Prelude.Maybe (Prelude.NonEmpty Tag))
createTable_tags = Lens.lens (\CreateTable' {tags} -> tags) (\s@CreateTable' {} a -> s {tags = a} :: CreateTable) Prelude.. Lens.mapping Lens.coerced

-- | Enables Time to Live custom settings for the table. The options are:
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
createTable_ttl :: Lens.Lens' CreateTable (Prelude.Maybe TimeToLive)
createTable_ttl = Lens.lens (\CreateTable' {ttl} -> ttl) (\s@CreateTable' {} a -> s {ttl = a} :: CreateTable)

-- | Specifies if @pointInTimeRecovery@ is enabled or disabled for the table.
-- The options are:
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
createTable_pointInTimeRecovery :: Lens.Lens' CreateTable (Prelude.Maybe PointInTimeRecovery)
createTable_pointInTimeRecovery = Lens.lens (\CreateTable' {pointInTimeRecovery} -> pointInTimeRecovery) (\s@CreateTable' {} a -> s {pointInTimeRecovery = a} :: CreateTable)

-- | Specifies the read\/write throughput capacity mode for the table. The
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
createTable_capacitySpecification :: Lens.Lens' CreateTable (Prelude.Maybe CapacitySpecification)
createTable_capacitySpecification = Lens.lens (\CreateTable' {capacitySpecification} -> capacitySpecification) (\s@CreateTable' {} a -> s {capacitySpecification = a} :: CreateTable)

-- | Specifies how the encryption key for encryption at rest is managed for
-- the table. You can choose one of the following KMS key (KMS key):
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
createTable_encryptionSpecification :: Lens.Lens' CreateTable (Prelude.Maybe EncryptionSpecification)
createTable_encryptionSpecification = Lens.lens (\CreateTable' {encryptionSpecification} -> encryptionSpecification) (\s@CreateTable' {} a -> s {encryptionSpecification = a} :: CreateTable)

-- | This parameter allows to enter a description of the table.
createTable_comment :: Lens.Lens' CreateTable (Prelude.Maybe Comment)
createTable_comment = Lens.lens (\CreateTable' {comment} -> comment) (\s@CreateTable' {} a -> s {comment = a} :: CreateTable)

-- | The default Time to Live setting in seconds for the table.
--
-- For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/TTL-how-it-works.html#ttl-howitworks_default_ttl Setting the default TTL value for a table>
-- in the /Amazon Keyspaces Developer Guide/.
createTable_defaultTimeToLive :: Lens.Lens' CreateTable (Prelude.Maybe Prelude.Natural)
createTable_defaultTimeToLive = Lens.lens (\CreateTable' {defaultTimeToLive} -> defaultTimeToLive) (\s@CreateTable' {} a -> s {defaultTimeToLive = a} :: CreateTable)

-- | The name of the keyspace that the table is going to be created in.
createTable_keyspaceName :: Lens.Lens' CreateTable Prelude.Text
createTable_keyspaceName = Lens.lens (\CreateTable' {keyspaceName} -> keyspaceName) (\s@CreateTable' {} a -> s {keyspaceName = a} :: CreateTable)

-- | The name of the table.
createTable_tableName :: Lens.Lens' CreateTable Prelude.Text
createTable_tableName = Lens.lens (\CreateTable' {tableName} -> tableName) (\s@CreateTable' {} a -> s {tableName = a} :: CreateTable)

-- | The @schemaDefinition@ consists of the following parameters.
--
-- For each column to be created:
--
-- • @name@ - The name of the column.
--
-- • @type@ - An Amazon Keyspaces data type. For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/cql.elements.html#cql.data-types Data types>
-- in the /Amazon Keyspaces Developer Guide/.
--
-- The primary key of the table consists of the following columns:
--
-- • @partitionKeys@ - The partition key can be a single column, or it can
-- be a compound value composed of two or more columns. The partition key
-- portion of the primary key is required and determines how Amazon
-- Keyspaces stores your data.
--
-- • @name@ - The name of each partition key column.
--
-- • @clusteringKeys@ - The optional clustering column portion of your
-- primary key determines how the data is clustered and sorted within each
-- partition.
--
-- • @name@ - The name of the clustering column.
--
-- • @orderBy@ - Sets the ascendant (@ASC@) or descendant (@DESC@) order
-- modifier.
--
-- To define a column as static use @staticColumns@ - Static columns store
-- values that are shared by all rows in the same partition:
--
-- • @name@ - The name of the column.
--
-- • @type@ - An Amazon Keyspaces data type.
createTable_schemaDefinition :: Lens.Lens' CreateTable SchemaDefinition
createTable_schemaDefinition = Lens.lens (\CreateTable' {schemaDefinition} -> schemaDefinition) (\s@CreateTable' {} a -> s {schemaDefinition = a} :: CreateTable)

instance Core.AWSRequest CreateTable where
  type AWSResponse CreateTable = CreateTableResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTableResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "resourceArn")
      )

instance Prelude.Hashable CreateTable where
  hashWithSalt _salt CreateTable' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` ttl
      `Prelude.hashWithSalt` pointInTimeRecovery
      `Prelude.hashWithSalt` capacitySpecification
      `Prelude.hashWithSalt` encryptionSpecification
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` defaultTimeToLive
      `Prelude.hashWithSalt` keyspaceName
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` schemaDefinition

instance Prelude.NFData CreateTable where
  rnf CreateTable' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf ttl
      `Prelude.seq` Prelude.rnf pointInTimeRecovery
      `Prelude.seq` Prelude.rnf capacitySpecification
      `Prelude.seq` Prelude.rnf encryptionSpecification
      `Prelude.seq` Prelude.rnf comment
      `Prelude.seq` Prelude.rnf defaultTimeToLive
      `Prelude.seq` Prelude.rnf keyspaceName
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf schemaDefinition

instance Core.ToHeaders CreateTable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "KeyspacesService.CreateTable" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateTable where
  toJSON CreateTable' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("ttl" Core..=) Prelude.<$> ttl,
            ("pointInTimeRecovery" Core..=)
              Prelude.<$> pointInTimeRecovery,
            ("capacitySpecification" Core..=)
              Prelude.<$> capacitySpecification,
            ("encryptionSpecification" Core..=)
              Prelude.<$> encryptionSpecification,
            ("comment" Core..=) Prelude.<$> comment,
            ("defaultTimeToLive" Core..=)
              Prelude.<$> defaultTimeToLive,
            Prelude.Just ("keyspaceName" Core..= keyspaceName),
            Prelude.Just ("tableName" Core..= tableName),
            Prelude.Just
              ("schemaDefinition" Core..= schemaDefinition)
          ]
      )

instance Core.ToPath CreateTable where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateTable where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTableResponse' smart constructor.
data CreateTableResponse = CreateTableResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique identifier of the table in the format of an Amazon Resource
    -- Name (ARN).
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createTableResponse_httpStatus' - The response's http status code.
--
-- 'resourceArn', 'createTableResponse_resourceArn' - The unique identifier of the table in the format of an Amazon Resource
-- Name (ARN).
newCreateTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'resourceArn'
  Prelude.Text ->
  CreateTableResponse
newCreateTableResponse pHttpStatus_ pResourceArn_ =
  CreateTableResponse'
    { httpStatus = pHttpStatus_,
      resourceArn = pResourceArn_
    }

-- | The response's http status code.
createTableResponse_httpStatus :: Lens.Lens' CreateTableResponse Prelude.Int
createTableResponse_httpStatus = Lens.lens (\CreateTableResponse' {httpStatus} -> httpStatus) (\s@CreateTableResponse' {} a -> s {httpStatus = a} :: CreateTableResponse)

-- | The unique identifier of the table in the format of an Amazon Resource
-- Name (ARN).
createTableResponse_resourceArn :: Lens.Lens' CreateTableResponse Prelude.Text
createTableResponse_resourceArn = Lens.lens (\CreateTableResponse' {resourceArn} -> resourceArn) (\s@CreateTableResponse' {} a -> s {resourceArn = a} :: CreateTableResponse)

instance Prelude.NFData CreateTableResponse where
  rnf CreateTableResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf resourceArn
