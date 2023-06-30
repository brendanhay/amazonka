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
-- Module      : Amazonka.IoT.Types.DynamoDBAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.DynamoDBAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.DynamoKeyType
import qualified Amazonka.Prelude as Prelude

-- | Describes an action to write to a DynamoDB table.
--
-- The @tableName@, @hashKeyField@, and @rangeKeyField@ values must match
-- the values used when you created the table.
--
-- The @hashKeyValue@ and @rangeKeyvalue@ fields use a substitution
-- template syntax. These templates provide data at runtime. The syntax is
-- as follows: ${/sql-expression/}.
--
-- You can specify any valid expression in a WHERE or SELECT clause,
-- including JSON properties, comparisons, calculations, and functions. For
-- example, the following field uses the third level of the topic:
--
-- @\"hashKeyValue\": \"${topic(3)}\"@
--
-- The following field uses the timestamp:
--
-- @\"rangeKeyValue\": \"${timestamp()}\"@
--
-- /See:/ 'newDynamoDBAction' smart constructor.
data DynamoDBAction = DynamoDBAction'
  { -- | The hash key type. Valid values are \"STRING\" or \"NUMBER\"
    hashKeyType :: Prelude.Maybe DynamoKeyType,
    -- | The type of operation to be performed. This follows the substitution
    -- template, so it can be @${operation}@, but the substitution must result
    -- in one of the following: @INSERT@, @UPDATE@, or @DELETE@.
    operation :: Prelude.Maybe Prelude.Text,
    -- | The action payload. This name can be customized.
    payloadField :: Prelude.Maybe Prelude.Text,
    -- | The range key name.
    rangeKeyField :: Prelude.Maybe Prelude.Text,
    -- | The range key type. Valid values are \"STRING\" or \"NUMBER\"
    rangeKeyType :: Prelude.Maybe DynamoKeyType,
    -- | The range key value.
    rangeKeyValue :: Prelude.Maybe Prelude.Text,
    -- | The name of the DynamoDB table.
    tableName :: Prelude.Text,
    -- | The ARN of the IAM role that grants access to the DynamoDB table.
    roleArn :: Prelude.Text,
    -- | The hash key name.
    hashKeyField :: Prelude.Text,
    -- | The hash key value.
    hashKeyValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DynamoDBAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hashKeyType', 'dynamoDBAction_hashKeyType' - The hash key type. Valid values are \"STRING\" or \"NUMBER\"
--
-- 'operation', 'dynamoDBAction_operation' - The type of operation to be performed. This follows the substitution
-- template, so it can be @${operation}@, but the substitution must result
-- in one of the following: @INSERT@, @UPDATE@, or @DELETE@.
--
-- 'payloadField', 'dynamoDBAction_payloadField' - The action payload. This name can be customized.
--
-- 'rangeKeyField', 'dynamoDBAction_rangeKeyField' - The range key name.
--
-- 'rangeKeyType', 'dynamoDBAction_rangeKeyType' - The range key type. Valid values are \"STRING\" or \"NUMBER\"
--
-- 'rangeKeyValue', 'dynamoDBAction_rangeKeyValue' - The range key value.
--
-- 'tableName', 'dynamoDBAction_tableName' - The name of the DynamoDB table.
--
-- 'roleArn', 'dynamoDBAction_roleArn' - The ARN of the IAM role that grants access to the DynamoDB table.
--
-- 'hashKeyField', 'dynamoDBAction_hashKeyField' - The hash key name.
--
-- 'hashKeyValue', 'dynamoDBAction_hashKeyValue' - The hash key value.
newDynamoDBAction ::
  -- | 'tableName'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'hashKeyField'
  Prelude.Text ->
  -- | 'hashKeyValue'
  Prelude.Text ->
  DynamoDBAction
newDynamoDBAction
  pTableName_
  pRoleArn_
  pHashKeyField_
  pHashKeyValue_ =
    DynamoDBAction'
      { hashKeyType = Prelude.Nothing,
        operation = Prelude.Nothing,
        payloadField = Prelude.Nothing,
        rangeKeyField = Prelude.Nothing,
        rangeKeyType = Prelude.Nothing,
        rangeKeyValue = Prelude.Nothing,
        tableName = pTableName_,
        roleArn = pRoleArn_,
        hashKeyField = pHashKeyField_,
        hashKeyValue = pHashKeyValue_
      }

-- | The hash key type. Valid values are \"STRING\" or \"NUMBER\"
dynamoDBAction_hashKeyType :: Lens.Lens' DynamoDBAction (Prelude.Maybe DynamoKeyType)
dynamoDBAction_hashKeyType = Lens.lens (\DynamoDBAction' {hashKeyType} -> hashKeyType) (\s@DynamoDBAction' {} a -> s {hashKeyType = a} :: DynamoDBAction)

-- | The type of operation to be performed. This follows the substitution
-- template, so it can be @${operation}@, but the substitution must result
-- in one of the following: @INSERT@, @UPDATE@, or @DELETE@.
dynamoDBAction_operation :: Lens.Lens' DynamoDBAction (Prelude.Maybe Prelude.Text)
dynamoDBAction_operation = Lens.lens (\DynamoDBAction' {operation} -> operation) (\s@DynamoDBAction' {} a -> s {operation = a} :: DynamoDBAction)

-- | The action payload. This name can be customized.
dynamoDBAction_payloadField :: Lens.Lens' DynamoDBAction (Prelude.Maybe Prelude.Text)
dynamoDBAction_payloadField = Lens.lens (\DynamoDBAction' {payloadField} -> payloadField) (\s@DynamoDBAction' {} a -> s {payloadField = a} :: DynamoDBAction)

-- | The range key name.
dynamoDBAction_rangeKeyField :: Lens.Lens' DynamoDBAction (Prelude.Maybe Prelude.Text)
dynamoDBAction_rangeKeyField = Lens.lens (\DynamoDBAction' {rangeKeyField} -> rangeKeyField) (\s@DynamoDBAction' {} a -> s {rangeKeyField = a} :: DynamoDBAction)

-- | The range key type. Valid values are \"STRING\" or \"NUMBER\"
dynamoDBAction_rangeKeyType :: Lens.Lens' DynamoDBAction (Prelude.Maybe DynamoKeyType)
dynamoDBAction_rangeKeyType = Lens.lens (\DynamoDBAction' {rangeKeyType} -> rangeKeyType) (\s@DynamoDBAction' {} a -> s {rangeKeyType = a} :: DynamoDBAction)

-- | The range key value.
dynamoDBAction_rangeKeyValue :: Lens.Lens' DynamoDBAction (Prelude.Maybe Prelude.Text)
dynamoDBAction_rangeKeyValue = Lens.lens (\DynamoDBAction' {rangeKeyValue} -> rangeKeyValue) (\s@DynamoDBAction' {} a -> s {rangeKeyValue = a} :: DynamoDBAction)

-- | The name of the DynamoDB table.
dynamoDBAction_tableName :: Lens.Lens' DynamoDBAction Prelude.Text
dynamoDBAction_tableName = Lens.lens (\DynamoDBAction' {tableName} -> tableName) (\s@DynamoDBAction' {} a -> s {tableName = a} :: DynamoDBAction)

-- | The ARN of the IAM role that grants access to the DynamoDB table.
dynamoDBAction_roleArn :: Lens.Lens' DynamoDBAction Prelude.Text
dynamoDBAction_roleArn = Lens.lens (\DynamoDBAction' {roleArn} -> roleArn) (\s@DynamoDBAction' {} a -> s {roleArn = a} :: DynamoDBAction)

-- | The hash key name.
dynamoDBAction_hashKeyField :: Lens.Lens' DynamoDBAction Prelude.Text
dynamoDBAction_hashKeyField = Lens.lens (\DynamoDBAction' {hashKeyField} -> hashKeyField) (\s@DynamoDBAction' {} a -> s {hashKeyField = a} :: DynamoDBAction)

-- | The hash key value.
dynamoDBAction_hashKeyValue :: Lens.Lens' DynamoDBAction Prelude.Text
dynamoDBAction_hashKeyValue = Lens.lens (\DynamoDBAction' {hashKeyValue} -> hashKeyValue) (\s@DynamoDBAction' {} a -> s {hashKeyValue = a} :: DynamoDBAction)

instance Data.FromJSON DynamoDBAction where
  parseJSON =
    Data.withObject
      "DynamoDBAction"
      ( \x ->
          DynamoDBAction'
            Prelude.<$> (x Data..:? "hashKeyType")
            Prelude.<*> (x Data..:? "operation")
            Prelude.<*> (x Data..:? "payloadField")
            Prelude.<*> (x Data..:? "rangeKeyField")
            Prelude.<*> (x Data..:? "rangeKeyType")
            Prelude.<*> (x Data..:? "rangeKeyValue")
            Prelude.<*> (x Data..: "tableName")
            Prelude.<*> (x Data..: "roleArn")
            Prelude.<*> (x Data..: "hashKeyField")
            Prelude.<*> (x Data..: "hashKeyValue")
      )

instance Prelude.Hashable DynamoDBAction where
  hashWithSalt _salt DynamoDBAction' {..} =
    _salt
      `Prelude.hashWithSalt` hashKeyType
      `Prelude.hashWithSalt` operation
      `Prelude.hashWithSalt` payloadField
      `Prelude.hashWithSalt` rangeKeyField
      `Prelude.hashWithSalt` rangeKeyType
      `Prelude.hashWithSalt` rangeKeyValue
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` hashKeyField
      `Prelude.hashWithSalt` hashKeyValue

instance Prelude.NFData DynamoDBAction where
  rnf DynamoDBAction' {..} =
    Prelude.rnf hashKeyType
      `Prelude.seq` Prelude.rnf operation
      `Prelude.seq` Prelude.rnf payloadField
      `Prelude.seq` Prelude.rnf rangeKeyField
      `Prelude.seq` Prelude.rnf rangeKeyType
      `Prelude.seq` Prelude.rnf rangeKeyValue
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf hashKeyField
      `Prelude.seq` Prelude.rnf hashKeyValue

instance Data.ToJSON DynamoDBAction where
  toJSON DynamoDBAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("hashKeyType" Data..=) Prelude.<$> hashKeyType,
            ("operation" Data..=) Prelude.<$> operation,
            ("payloadField" Data..=) Prelude.<$> payloadField,
            ("rangeKeyField" Data..=) Prelude.<$> rangeKeyField,
            ("rangeKeyType" Data..=) Prelude.<$> rangeKeyType,
            ("rangeKeyValue" Data..=) Prelude.<$> rangeKeyValue,
            Prelude.Just ("tableName" Data..= tableName),
            Prelude.Just ("roleArn" Data..= roleArn),
            Prelude.Just ("hashKeyField" Data..= hashKeyField),
            Prelude.Just ("hashKeyValue" Data..= hashKeyValue)
          ]
      )
