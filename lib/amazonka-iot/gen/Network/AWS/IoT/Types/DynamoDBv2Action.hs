{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.DynamoDBv2Action
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.DynamoDBv2Action
  ( DynamoDBv2Action (..),

    -- * Smart constructor
    mkDynamoDBv2Action,

    -- * Lenses
    dRoleArn,
    dPutItem,
  )
where

import qualified Network.AWS.IoT.Types.AwsArn as Types
import qualified Network.AWS.IoT.Types.PutItemInput as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an action to write to a DynamoDB table.
--
-- This DynamoDB action writes each attribute in the message payload into it's own column in the DynamoDB table.
--
-- /See:/ 'mkDynamoDBv2Action' smart constructor.
data DynamoDBv2Action = DynamoDBv2Action'
  { -- | The ARN of the IAM role that grants access to the DynamoDB table.
    roleArn :: Types.AwsArn,
    -- | Specifies the DynamoDB table to which the message data will be written. For example:
    --
    -- @{ "dynamoDBv2": { "roleArn": "aws:iam:12341251:my-role" "putItem": { "tableName": "my-table" } } }@
    -- Each attribute in the message payload will be written to a separate column in the DynamoDB database.
    putItem :: Types.PutItemInput
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DynamoDBv2Action' value with any optional fields omitted.
mkDynamoDBv2Action ::
  -- | 'roleArn'
  Types.AwsArn ->
  -- | 'putItem'
  Types.PutItemInput ->
  DynamoDBv2Action
mkDynamoDBv2Action roleArn putItem =
  DynamoDBv2Action' {roleArn, putItem}

-- | The ARN of the IAM role that grants access to the DynamoDB table.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dRoleArn :: Lens.Lens' DynamoDBv2Action Types.AwsArn
dRoleArn = Lens.field @"roleArn"
{-# DEPRECATED dRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | Specifies the DynamoDB table to which the message data will be written. For example:
--
-- @{ "dynamoDBv2": { "roleArn": "aws:iam:12341251:my-role" "putItem": { "tableName": "my-table" } } }@
-- Each attribute in the message payload will be written to a separate column in the DynamoDB database.
--
-- /Note:/ Consider using 'putItem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dPutItem :: Lens.Lens' DynamoDBv2Action Types.PutItemInput
dPutItem = Lens.field @"putItem"
{-# DEPRECATED dPutItem "Use generic-lens or generic-optics with 'putItem' instead." #-}

instance Core.FromJSON DynamoDBv2Action where
  toJSON DynamoDBv2Action {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("roleArn" Core..= roleArn),
            Core.Just ("putItem" Core..= putItem)
          ]
      )

instance Core.FromJSON DynamoDBv2Action where
  parseJSON =
    Core.withObject "DynamoDBv2Action" Core.$
      \x ->
        DynamoDBv2Action'
          Core.<$> (x Core..: "roleArn") Core.<*> (x Core..: "putItem")
