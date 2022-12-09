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
-- Module      : Amazonka.IoTEvents.Types.DynamoDBAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.DynamoDBAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types.Payload
import qualified Amazonka.Prelude as Prelude

-- | Defines an action to write to the Amazon DynamoDB table that you
-- created. The standard action payload contains all the information about
-- the detector model instance and the event that triggered the action. You
-- can customize the
-- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_Payload.html payload>.
-- One column of the DynamoDB table receives all attribute-value pairs in
-- the payload that you specify.
--
-- You must use expressions for all parameters in @DynamoDBAction@. The
-- expressions accept literals, operators, functions, references, and
-- substitution templates.
--
-- __Examples__
--
-- -   For literal values, the expressions must contain single quotes. For
--     example, the value for the @hashKeyType@ parameter can be
--     @\'STRING\'@.
--
-- -   For references, you must specify either variables or input values.
--     For example, the value for the @hashKeyField@ parameter can be
--     @$input.GreenhouseInput.name@.
--
-- -   For a substitution template, you must use @${}@, and the template
--     must be in single quotes. A substitution template can also contain a
--     combination of literals, operators, functions, references, and
--     substitution templates.
--
--     In the following example, the value for the @hashKeyValue@ parameter
--     uses a substitution template.
--
--     @\'${$input.GreenhouseInput.temperature * 6 \/ 5 + 32} in Fahrenheit\'@
--
-- -   For a string concatenation, you must use @+@. A string concatenation
--     can also contain a combination of literals, operators, functions,
--     references, and substitution templates.
--
--     In the following example, the value for the @tableName@ parameter
--     uses a string concatenation.
--
--     @\'GreenhouseTemperatureTable \' + $input.GreenhouseInput.date@
--
-- For more information, see
-- <https://docs.aws.amazon.com/iotevents/latest/developerguide/iotevents-expressions.html Expressions>
-- in the /AWS IoT Events Developer Guide/.
--
-- If the defined payload type is a string, @DynamoDBAction@ writes
-- non-JSON data to the DynamoDB table as binary data. The DynamoDB console
-- displays the data as Base64-encoded text. The value for the
-- @payloadField@ parameter is @\<payload-field>_raw@.
--
-- /See:/ 'newDynamoDBAction' smart constructor.
data DynamoDBAction = DynamoDBAction'
  { -- | The data type for the hash key (also called the partition key). You can
    -- specify the following values:
    --
    -- -   @\'STRING\'@ - The hash key is a string.
    --
    -- -   @\'NUMBER\'@ - The hash key is a number.
    --
    -- If you don\'t specify @hashKeyType@, the default value is @\'STRING\'@.
    hashKeyType :: Prelude.Maybe Prelude.Text,
    -- | The type of operation to perform. You can specify the following values:
    --
    -- -   @\'INSERT\'@ - Insert data as a new item into the DynamoDB table.
    --     This item uses the specified hash key as a partition key. If you
    --     specified a range key, the item uses the range key as a sort key.
    --
    -- -   @\'UPDATE\'@ - Update an existing item of the DynamoDB table with
    --     new data. This item\'s partition key must match the specified hash
    --     key. If you specified a range key, the range key must match the
    --     item\'s sort key.
    --
    -- -   @\'DELETE\'@ - Delete an existing item of the DynamoDB table. This
    --     item\'s partition key must match the specified hash key. If you
    --     specified a range key, the range key must match the item\'s sort
    --     key.
    --
    -- If you don\'t specify this parameter, AWS IoT Events triggers the
    -- @\'INSERT\'@ operation.
    operation :: Prelude.Maybe Prelude.Text,
    payload :: Prelude.Maybe Payload,
    -- | The name of the DynamoDB column that receives the action payload.
    --
    -- If you don\'t specify this parameter, the name of the DynamoDB column is
    -- @payload@.
    payloadField :: Prelude.Maybe Prelude.Text,
    -- | The name of the range key (also called the sort key). The
    -- @rangeKeyField@ value must match the sort key of the target DynamoDB
    -- table.
    rangeKeyField :: Prelude.Maybe Prelude.Text,
    -- | The data type for the range key (also called the sort key), You can
    -- specify the following values:
    --
    -- -   @\'STRING\'@ - The range key is a string.
    --
    -- -   @\'NUMBER\'@ - The range key is number.
    --
    -- If you don\'t specify @rangeKeyField@, the default value is
    -- @\'STRING\'@.
    rangeKeyType :: Prelude.Maybe Prelude.Text,
    -- | The value of the range key (also called the sort key).
    rangeKeyValue :: Prelude.Maybe Prelude.Text,
    -- | The name of the hash key (also called the partition key). The
    -- @hashKeyField@ value must match the partition key of the target DynamoDB
    -- table.
    hashKeyField :: Prelude.Text,
    -- | The value of the hash key (also called the partition key).
    hashKeyValue :: Prelude.Text,
    -- | The name of the DynamoDB table. The @tableName@ value must match the
    -- table name of the target DynamoDB table.
    tableName :: Prelude.Text
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
-- 'hashKeyType', 'dynamoDBAction_hashKeyType' - The data type for the hash key (also called the partition key). You can
-- specify the following values:
--
-- -   @\'STRING\'@ - The hash key is a string.
--
-- -   @\'NUMBER\'@ - The hash key is a number.
--
-- If you don\'t specify @hashKeyType@, the default value is @\'STRING\'@.
--
-- 'operation', 'dynamoDBAction_operation' - The type of operation to perform. You can specify the following values:
--
-- -   @\'INSERT\'@ - Insert data as a new item into the DynamoDB table.
--     This item uses the specified hash key as a partition key. If you
--     specified a range key, the item uses the range key as a sort key.
--
-- -   @\'UPDATE\'@ - Update an existing item of the DynamoDB table with
--     new data. This item\'s partition key must match the specified hash
--     key. If you specified a range key, the range key must match the
--     item\'s sort key.
--
-- -   @\'DELETE\'@ - Delete an existing item of the DynamoDB table. This
--     item\'s partition key must match the specified hash key. If you
--     specified a range key, the range key must match the item\'s sort
--     key.
--
-- If you don\'t specify this parameter, AWS IoT Events triggers the
-- @\'INSERT\'@ operation.
--
-- 'payload', 'dynamoDBAction_payload' - Undocumented member.
--
-- 'payloadField', 'dynamoDBAction_payloadField' - The name of the DynamoDB column that receives the action payload.
--
-- If you don\'t specify this parameter, the name of the DynamoDB column is
-- @payload@.
--
-- 'rangeKeyField', 'dynamoDBAction_rangeKeyField' - The name of the range key (also called the sort key). The
-- @rangeKeyField@ value must match the sort key of the target DynamoDB
-- table.
--
-- 'rangeKeyType', 'dynamoDBAction_rangeKeyType' - The data type for the range key (also called the sort key), You can
-- specify the following values:
--
-- -   @\'STRING\'@ - The range key is a string.
--
-- -   @\'NUMBER\'@ - The range key is number.
--
-- If you don\'t specify @rangeKeyField@, the default value is
-- @\'STRING\'@.
--
-- 'rangeKeyValue', 'dynamoDBAction_rangeKeyValue' - The value of the range key (also called the sort key).
--
-- 'hashKeyField', 'dynamoDBAction_hashKeyField' - The name of the hash key (also called the partition key). The
-- @hashKeyField@ value must match the partition key of the target DynamoDB
-- table.
--
-- 'hashKeyValue', 'dynamoDBAction_hashKeyValue' - The value of the hash key (also called the partition key).
--
-- 'tableName', 'dynamoDBAction_tableName' - The name of the DynamoDB table. The @tableName@ value must match the
-- table name of the target DynamoDB table.
newDynamoDBAction ::
  -- | 'hashKeyField'
  Prelude.Text ->
  -- | 'hashKeyValue'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  DynamoDBAction
newDynamoDBAction
  pHashKeyField_
  pHashKeyValue_
  pTableName_ =
    DynamoDBAction'
      { hashKeyType = Prelude.Nothing,
        operation = Prelude.Nothing,
        payload = Prelude.Nothing,
        payloadField = Prelude.Nothing,
        rangeKeyField = Prelude.Nothing,
        rangeKeyType = Prelude.Nothing,
        rangeKeyValue = Prelude.Nothing,
        hashKeyField = pHashKeyField_,
        hashKeyValue = pHashKeyValue_,
        tableName = pTableName_
      }

-- | The data type for the hash key (also called the partition key). You can
-- specify the following values:
--
-- -   @\'STRING\'@ - The hash key is a string.
--
-- -   @\'NUMBER\'@ - The hash key is a number.
--
-- If you don\'t specify @hashKeyType@, the default value is @\'STRING\'@.
dynamoDBAction_hashKeyType :: Lens.Lens' DynamoDBAction (Prelude.Maybe Prelude.Text)
dynamoDBAction_hashKeyType = Lens.lens (\DynamoDBAction' {hashKeyType} -> hashKeyType) (\s@DynamoDBAction' {} a -> s {hashKeyType = a} :: DynamoDBAction)

-- | The type of operation to perform. You can specify the following values:
--
-- -   @\'INSERT\'@ - Insert data as a new item into the DynamoDB table.
--     This item uses the specified hash key as a partition key. If you
--     specified a range key, the item uses the range key as a sort key.
--
-- -   @\'UPDATE\'@ - Update an existing item of the DynamoDB table with
--     new data. This item\'s partition key must match the specified hash
--     key. If you specified a range key, the range key must match the
--     item\'s sort key.
--
-- -   @\'DELETE\'@ - Delete an existing item of the DynamoDB table. This
--     item\'s partition key must match the specified hash key. If you
--     specified a range key, the range key must match the item\'s sort
--     key.
--
-- If you don\'t specify this parameter, AWS IoT Events triggers the
-- @\'INSERT\'@ operation.
dynamoDBAction_operation :: Lens.Lens' DynamoDBAction (Prelude.Maybe Prelude.Text)
dynamoDBAction_operation = Lens.lens (\DynamoDBAction' {operation} -> operation) (\s@DynamoDBAction' {} a -> s {operation = a} :: DynamoDBAction)

-- | Undocumented member.
dynamoDBAction_payload :: Lens.Lens' DynamoDBAction (Prelude.Maybe Payload)
dynamoDBAction_payload = Lens.lens (\DynamoDBAction' {payload} -> payload) (\s@DynamoDBAction' {} a -> s {payload = a} :: DynamoDBAction)

-- | The name of the DynamoDB column that receives the action payload.
--
-- If you don\'t specify this parameter, the name of the DynamoDB column is
-- @payload@.
dynamoDBAction_payloadField :: Lens.Lens' DynamoDBAction (Prelude.Maybe Prelude.Text)
dynamoDBAction_payloadField = Lens.lens (\DynamoDBAction' {payloadField} -> payloadField) (\s@DynamoDBAction' {} a -> s {payloadField = a} :: DynamoDBAction)

-- | The name of the range key (also called the sort key). The
-- @rangeKeyField@ value must match the sort key of the target DynamoDB
-- table.
dynamoDBAction_rangeKeyField :: Lens.Lens' DynamoDBAction (Prelude.Maybe Prelude.Text)
dynamoDBAction_rangeKeyField = Lens.lens (\DynamoDBAction' {rangeKeyField} -> rangeKeyField) (\s@DynamoDBAction' {} a -> s {rangeKeyField = a} :: DynamoDBAction)

-- | The data type for the range key (also called the sort key), You can
-- specify the following values:
--
-- -   @\'STRING\'@ - The range key is a string.
--
-- -   @\'NUMBER\'@ - The range key is number.
--
-- If you don\'t specify @rangeKeyField@, the default value is
-- @\'STRING\'@.
dynamoDBAction_rangeKeyType :: Lens.Lens' DynamoDBAction (Prelude.Maybe Prelude.Text)
dynamoDBAction_rangeKeyType = Lens.lens (\DynamoDBAction' {rangeKeyType} -> rangeKeyType) (\s@DynamoDBAction' {} a -> s {rangeKeyType = a} :: DynamoDBAction)

-- | The value of the range key (also called the sort key).
dynamoDBAction_rangeKeyValue :: Lens.Lens' DynamoDBAction (Prelude.Maybe Prelude.Text)
dynamoDBAction_rangeKeyValue = Lens.lens (\DynamoDBAction' {rangeKeyValue} -> rangeKeyValue) (\s@DynamoDBAction' {} a -> s {rangeKeyValue = a} :: DynamoDBAction)

-- | The name of the hash key (also called the partition key). The
-- @hashKeyField@ value must match the partition key of the target DynamoDB
-- table.
dynamoDBAction_hashKeyField :: Lens.Lens' DynamoDBAction Prelude.Text
dynamoDBAction_hashKeyField = Lens.lens (\DynamoDBAction' {hashKeyField} -> hashKeyField) (\s@DynamoDBAction' {} a -> s {hashKeyField = a} :: DynamoDBAction)

-- | The value of the hash key (also called the partition key).
dynamoDBAction_hashKeyValue :: Lens.Lens' DynamoDBAction Prelude.Text
dynamoDBAction_hashKeyValue = Lens.lens (\DynamoDBAction' {hashKeyValue} -> hashKeyValue) (\s@DynamoDBAction' {} a -> s {hashKeyValue = a} :: DynamoDBAction)

-- | The name of the DynamoDB table. The @tableName@ value must match the
-- table name of the target DynamoDB table.
dynamoDBAction_tableName :: Lens.Lens' DynamoDBAction Prelude.Text
dynamoDBAction_tableName = Lens.lens (\DynamoDBAction' {tableName} -> tableName) (\s@DynamoDBAction' {} a -> s {tableName = a} :: DynamoDBAction)

instance Data.FromJSON DynamoDBAction where
  parseJSON =
    Data.withObject
      "DynamoDBAction"
      ( \x ->
          DynamoDBAction'
            Prelude.<$> (x Data..:? "hashKeyType")
            Prelude.<*> (x Data..:? "operation")
            Prelude.<*> (x Data..:? "payload")
            Prelude.<*> (x Data..:? "payloadField")
            Prelude.<*> (x Data..:? "rangeKeyField")
            Prelude.<*> (x Data..:? "rangeKeyType")
            Prelude.<*> (x Data..:? "rangeKeyValue")
            Prelude.<*> (x Data..: "hashKeyField")
            Prelude.<*> (x Data..: "hashKeyValue")
            Prelude.<*> (x Data..: "tableName")
      )

instance Prelude.Hashable DynamoDBAction where
  hashWithSalt _salt DynamoDBAction' {..} =
    _salt `Prelude.hashWithSalt` hashKeyType
      `Prelude.hashWithSalt` operation
      `Prelude.hashWithSalt` payload
      `Prelude.hashWithSalt` payloadField
      `Prelude.hashWithSalt` rangeKeyField
      `Prelude.hashWithSalt` rangeKeyType
      `Prelude.hashWithSalt` rangeKeyValue
      `Prelude.hashWithSalt` hashKeyField
      `Prelude.hashWithSalt` hashKeyValue
      `Prelude.hashWithSalt` tableName

instance Prelude.NFData DynamoDBAction where
  rnf DynamoDBAction' {..} =
    Prelude.rnf hashKeyType
      `Prelude.seq` Prelude.rnf operation
      `Prelude.seq` Prelude.rnf payload
      `Prelude.seq` Prelude.rnf payloadField
      `Prelude.seq` Prelude.rnf rangeKeyField
      `Prelude.seq` Prelude.rnf rangeKeyType
      `Prelude.seq` Prelude.rnf rangeKeyValue
      `Prelude.seq` Prelude.rnf hashKeyField
      `Prelude.seq` Prelude.rnf hashKeyValue
      `Prelude.seq` Prelude.rnf tableName

instance Data.ToJSON DynamoDBAction where
  toJSON DynamoDBAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("hashKeyType" Data..=) Prelude.<$> hashKeyType,
            ("operation" Data..=) Prelude.<$> operation,
            ("payload" Data..=) Prelude.<$> payload,
            ("payloadField" Data..=) Prelude.<$> payloadField,
            ("rangeKeyField" Data..=) Prelude.<$> rangeKeyField,
            ("rangeKeyType" Data..=) Prelude.<$> rangeKeyType,
            ("rangeKeyValue" Data..=) Prelude.<$> rangeKeyValue,
            Prelude.Just ("hashKeyField" Data..= hashKeyField),
            Prelude.Just ("hashKeyValue" Data..= hashKeyValue),
            Prelude.Just ("tableName" Data..= tableName)
          ]
      )
