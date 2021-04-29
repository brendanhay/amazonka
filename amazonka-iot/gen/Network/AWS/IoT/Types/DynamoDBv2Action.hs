{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.Types.DynamoDBv2Action
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.DynamoDBv2Action where

import Network.AWS.IoT.Types.PutItemInput
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an action to write to a DynamoDB table.
--
-- This DynamoDB action writes each attribute in the message payload into
-- it\'s own column in the DynamoDB table.
--
-- /See:/ 'newDynamoDBv2Action' smart constructor.
data DynamoDBv2Action = DynamoDBv2Action'
  { -- | The ARN of the IAM role that grants access to the DynamoDB table.
    roleArn :: Prelude.Text,
    -- | Specifies the DynamoDB table to which the message data will be written.
    -- For example:
    --
    -- @{ \"dynamoDBv2\": { \"roleArn\": \"aws:iam:12341251:my-role\" \"putItem\": { \"tableName\": \"my-table\" } } }@
    --
    -- Each attribute in the message payload will be written to a separate
    -- column in the DynamoDB database.
    putItem :: PutItemInput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DynamoDBv2Action' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'dynamoDBv2Action_roleArn' - The ARN of the IAM role that grants access to the DynamoDB table.
--
-- 'putItem', 'dynamoDBv2Action_putItem' - Specifies the DynamoDB table to which the message data will be written.
-- For example:
--
-- @{ \"dynamoDBv2\": { \"roleArn\": \"aws:iam:12341251:my-role\" \"putItem\": { \"tableName\": \"my-table\" } } }@
--
-- Each attribute in the message payload will be written to a separate
-- column in the DynamoDB database.
newDynamoDBv2Action ::
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'putItem'
  PutItemInput ->
  DynamoDBv2Action
newDynamoDBv2Action pRoleArn_ pPutItem_ =
  DynamoDBv2Action'
    { roleArn = pRoleArn_,
      putItem = pPutItem_
    }

-- | The ARN of the IAM role that grants access to the DynamoDB table.
dynamoDBv2Action_roleArn :: Lens.Lens' DynamoDBv2Action Prelude.Text
dynamoDBv2Action_roleArn = Lens.lens (\DynamoDBv2Action' {roleArn} -> roleArn) (\s@DynamoDBv2Action' {} a -> s {roleArn = a} :: DynamoDBv2Action)

-- | Specifies the DynamoDB table to which the message data will be written.
-- For example:
--
-- @{ \"dynamoDBv2\": { \"roleArn\": \"aws:iam:12341251:my-role\" \"putItem\": { \"tableName\": \"my-table\" } } }@
--
-- Each attribute in the message payload will be written to a separate
-- column in the DynamoDB database.
dynamoDBv2Action_putItem :: Lens.Lens' DynamoDBv2Action PutItemInput
dynamoDBv2Action_putItem = Lens.lens (\DynamoDBv2Action' {putItem} -> putItem) (\s@DynamoDBv2Action' {} a -> s {putItem = a} :: DynamoDBv2Action)

instance Prelude.FromJSON DynamoDBv2Action where
  parseJSON =
    Prelude.withObject
      "DynamoDBv2Action"
      ( \x ->
          DynamoDBv2Action'
            Prelude.<$> (x Prelude..: "roleArn")
            Prelude.<*> (x Prelude..: "putItem")
      )

instance Prelude.Hashable DynamoDBv2Action

instance Prelude.NFData DynamoDBv2Action

instance Prelude.ToJSON DynamoDBv2Action where
  toJSON DynamoDBv2Action' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("roleArn" Prelude..= roleArn),
            Prelude.Just ("putItem" Prelude..= putItem)
          ]
      )
