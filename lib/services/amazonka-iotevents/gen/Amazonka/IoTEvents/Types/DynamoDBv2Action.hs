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
-- Module      : Amazonka.IoTEvents.Types.DynamoDBv2Action
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.DynamoDBv2Action where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types.Payload
import qualified Amazonka.Prelude as Prelude

-- | Defines an action to write to the Amazon DynamoDB table that you
-- created. The default action payload contains all the information about
-- the detector model instance and the event that triggered the action. You
-- can customize the
-- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_Payload.html payload>.
-- A separate column of the DynamoDB table receives one attribute-value
-- pair in the payload that you specify.
--
-- You must use expressions for all parameters in @DynamoDBv2Action@. The
-- expressions accept literals, operators, functions, references, and
-- substitution templates.
--
-- __Examples__
--
-- -   For literal values, the expressions must contain single quotes. For
--     example, the value for the @tableName@ parameter can be
--     @\'GreenhouseTemperatureTable\'@.
--
-- -   For references, you must specify either variables or input values.
--     For example, the value for the @tableName@ parameter can be
--     @$variable.ddbtableName@.
--
-- -   For a substitution template, you must use @${}@, and the template
--     must be in single quotes. A substitution template can also contain a
--     combination of literals, operators, functions, references, and
--     substitution templates.
--
--     In the following example, the value for the @contentExpression@
--     parameter in @Payload@ uses a substitution template.
--
--     @\'{\\\"sensorID\\\": \\\"${$input.GreenhouseInput.sensor_id}\\\", \\\"temperature\\\": \\\"${$input.GreenhouseInput.temperature * 9 \/ 5 + 32}\\\"}\'@
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
-- The value for the @type@ parameter in @Payload@ must be @JSON@.
--
-- /See:/ 'newDynamoDBv2Action' smart constructor.
data DynamoDBv2Action = DynamoDBv2Action'
  { payload :: Prelude.Maybe Payload,
    -- | The name of the DynamoDB table.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DynamoDBv2Action' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'payload', 'dynamoDBv2Action_payload' - Undocumented member.
--
-- 'tableName', 'dynamoDBv2Action_tableName' - The name of the DynamoDB table.
newDynamoDBv2Action ::
  -- | 'tableName'
  Prelude.Text ->
  DynamoDBv2Action
newDynamoDBv2Action pTableName_ =
  DynamoDBv2Action'
    { payload = Prelude.Nothing,
      tableName = pTableName_
    }

-- | Undocumented member.
dynamoDBv2Action_payload :: Lens.Lens' DynamoDBv2Action (Prelude.Maybe Payload)
dynamoDBv2Action_payload = Lens.lens (\DynamoDBv2Action' {payload} -> payload) (\s@DynamoDBv2Action' {} a -> s {payload = a} :: DynamoDBv2Action)

-- | The name of the DynamoDB table.
dynamoDBv2Action_tableName :: Lens.Lens' DynamoDBv2Action Prelude.Text
dynamoDBv2Action_tableName = Lens.lens (\DynamoDBv2Action' {tableName} -> tableName) (\s@DynamoDBv2Action' {} a -> s {tableName = a} :: DynamoDBv2Action)

instance Data.FromJSON DynamoDBv2Action where
  parseJSON =
    Data.withObject
      "DynamoDBv2Action"
      ( \x ->
          DynamoDBv2Action'
            Prelude.<$> (x Data..:? "payload")
            Prelude.<*> (x Data..: "tableName")
      )

instance Prelude.Hashable DynamoDBv2Action where
  hashWithSalt _salt DynamoDBv2Action' {..} =
    _salt
      `Prelude.hashWithSalt` payload
      `Prelude.hashWithSalt` tableName

instance Prelude.NFData DynamoDBv2Action where
  rnf DynamoDBv2Action' {..} =
    Prelude.rnf payload
      `Prelude.seq` Prelude.rnf tableName

instance Data.ToJSON DynamoDBv2Action where
  toJSON DynamoDBv2Action' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("payload" Data..=) Prelude.<$> payload,
            Prelude.Just ("tableName" Data..= tableName)
          ]
      )
