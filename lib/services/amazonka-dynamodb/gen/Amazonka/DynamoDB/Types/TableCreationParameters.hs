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
-- Module      : Amazonka.DynamoDB.Types.TableCreationParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.TableCreationParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DynamoDB.Types.AttributeDefinition
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.BillingMode
import Amazonka.DynamoDB.Types.GlobalSecondaryIndex
import Amazonka.DynamoDB.Types.KeySchemaElement
import Amazonka.DynamoDB.Types.ProvisionedThroughput
import Amazonka.DynamoDB.Types.SSESpecification
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | The parameters for the table created as part of the import operation.
--
-- /See:/ 'newTableCreationParameters' smart constructor.
data TableCreationParameters = TableCreationParameters'
  { -- | The billing mode for provisioning the table created as part of the
    -- import operation.
    billingMode :: Prelude.Maybe BillingMode,
    provisionedThroughput :: Prelude.Maybe ProvisionedThroughput,
    sSESpecification :: Prelude.Maybe SSESpecification,
    -- | The Global Secondary Indexes (GSI) of the table to be created as part of
    -- the import operation.
    globalSecondaryIndexes :: Prelude.Maybe [GlobalSecondaryIndex],
    -- | The name of the table created as part of the import operation.
    tableName :: Prelude.Text,
    -- | The attributes of the table created as part of the import operation.
    attributeDefinitions :: [AttributeDefinition],
    -- | The primary key and option sort key of the table created as part of the
    -- import operation.
    keySchema :: Prelude.NonEmpty KeySchemaElement
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableCreationParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingMode', 'tableCreationParameters_billingMode' - The billing mode for provisioning the table created as part of the
-- import operation.
--
-- 'provisionedThroughput', 'tableCreationParameters_provisionedThroughput' - Undocumented member.
--
-- 'sSESpecification', 'tableCreationParameters_sSESpecification' - Undocumented member.
--
-- 'globalSecondaryIndexes', 'tableCreationParameters_globalSecondaryIndexes' - The Global Secondary Indexes (GSI) of the table to be created as part of
-- the import operation.
--
-- 'tableName', 'tableCreationParameters_tableName' - The name of the table created as part of the import operation.
--
-- 'attributeDefinitions', 'tableCreationParameters_attributeDefinitions' - The attributes of the table created as part of the import operation.
--
-- 'keySchema', 'tableCreationParameters_keySchema' - The primary key and option sort key of the table created as part of the
-- import operation.
newTableCreationParameters ::
  -- | 'tableName'
  Prelude.Text ->
  -- | 'keySchema'
  Prelude.NonEmpty KeySchemaElement ->
  TableCreationParameters
newTableCreationParameters pTableName_ pKeySchema_ =
  TableCreationParameters'
    { billingMode =
        Prelude.Nothing,
      provisionedThroughput = Prelude.Nothing,
      sSESpecification = Prelude.Nothing,
      globalSecondaryIndexes = Prelude.Nothing,
      tableName = pTableName_,
      attributeDefinitions = Prelude.mempty,
      keySchema = Lens.coerced Lens.# pKeySchema_
    }

-- | The billing mode for provisioning the table created as part of the
-- import operation.
tableCreationParameters_billingMode :: Lens.Lens' TableCreationParameters (Prelude.Maybe BillingMode)
tableCreationParameters_billingMode = Lens.lens (\TableCreationParameters' {billingMode} -> billingMode) (\s@TableCreationParameters' {} a -> s {billingMode = a} :: TableCreationParameters)

-- | Undocumented member.
tableCreationParameters_provisionedThroughput :: Lens.Lens' TableCreationParameters (Prelude.Maybe ProvisionedThroughput)
tableCreationParameters_provisionedThroughput = Lens.lens (\TableCreationParameters' {provisionedThroughput} -> provisionedThroughput) (\s@TableCreationParameters' {} a -> s {provisionedThroughput = a} :: TableCreationParameters)

-- | Undocumented member.
tableCreationParameters_sSESpecification :: Lens.Lens' TableCreationParameters (Prelude.Maybe SSESpecification)
tableCreationParameters_sSESpecification = Lens.lens (\TableCreationParameters' {sSESpecification} -> sSESpecification) (\s@TableCreationParameters' {} a -> s {sSESpecification = a} :: TableCreationParameters)

-- | The Global Secondary Indexes (GSI) of the table to be created as part of
-- the import operation.
tableCreationParameters_globalSecondaryIndexes :: Lens.Lens' TableCreationParameters (Prelude.Maybe [GlobalSecondaryIndex])
tableCreationParameters_globalSecondaryIndexes = Lens.lens (\TableCreationParameters' {globalSecondaryIndexes} -> globalSecondaryIndexes) (\s@TableCreationParameters' {} a -> s {globalSecondaryIndexes = a} :: TableCreationParameters) Prelude.. Lens.mapping Lens.coerced

-- | The name of the table created as part of the import operation.
tableCreationParameters_tableName :: Lens.Lens' TableCreationParameters Prelude.Text
tableCreationParameters_tableName = Lens.lens (\TableCreationParameters' {tableName} -> tableName) (\s@TableCreationParameters' {} a -> s {tableName = a} :: TableCreationParameters)

-- | The attributes of the table created as part of the import operation.
tableCreationParameters_attributeDefinitions :: Lens.Lens' TableCreationParameters [AttributeDefinition]
tableCreationParameters_attributeDefinitions = Lens.lens (\TableCreationParameters' {attributeDefinitions} -> attributeDefinitions) (\s@TableCreationParameters' {} a -> s {attributeDefinitions = a} :: TableCreationParameters) Prelude.. Lens.coerced

-- | The primary key and option sort key of the table created as part of the
-- import operation.
tableCreationParameters_keySchema :: Lens.Lens' TableCreationParameters (Prelude.NonEmpty KeySchemaElement)
tableCreationParameters_keySchema = Lens.lens (\TableCreationParameters' {keySchema} -> keySchema) (\s@TableCreationParameters' {} a -> s {keySchema = a} :: TableCreationParameters) Prelude.. Lens.coerced

instance Core.FromJSON TableCreationParameters where
  parseJSON =
    Core.withObject
      "TableCreationParameters"
      ( \x ->
          TableCreationParameters'
            Prelude.<$> (x Core..:? "BillingMode")
            Prelude.<*> (x Core..:? "ProvisionedThroughput")
            Prelude.<*> (x Core..:? "SSESpecification")
            Prelude.<*> ( x Core..:? "GlobalSecondaryIndexes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "TableName")
            Prelude.<*> ( x Core..:? "AttributeDefinitions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "KeySchema")
      )

instance Prelude.Hashable TableCreationParameters where
  hashWithSalt _salt TableCreationParameters' {..} =
    _salt `Prelude.hashWithSalt` billingMode
      `Prelude.hashWithSalt` provisionedThroughput
      `Prelude.hashWithSalt` sSESpecification
      `Prelude.hashWithSalt` globalSecondaryIndexes
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` attributeDefinitions
      `Prelude.hashWithSalt` keySchema

instance Prelude.NFData TableCreationParameters where
  rnf TableCreationParameters' {..} =
    Prelude.rnf billingMode
      `Prelude.seq` Prelude.rnf provisionedThroughput
      `Prelude.seq` Prelude.rnf sSESpecification
      `Prelude.seq` Prelude.rnf globalSecondaryIndexes
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf attributeDefinitions
      `Prelude.seq` Prelude.rnf keySchema

instance Core.ToJSON TableCreationParameters where
  toJSON TableCreationParameters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("BillingMode" Core..=) Prelude.<$> billingMode,
            ("ProvisionedThroughput" Core..=)
              Prelude.<$> provisionedThroughput,
            ("SSESpecification" Core..=)
              Prelude.<$> sSESpecification,
            ("GlobalSecondaryIndexes" Core..=)
              Prelude.<$> globalSecondaryIndexes,
            Prelude.Just ("TableName" Core..= tableName),
            Prelude.Just
              ( "AttributeDefinitions"
                  Core..= attributeDefinitions
              ),
            Prelude.Just ("KeySchema" Core..= keySchema)
          ]
      )
