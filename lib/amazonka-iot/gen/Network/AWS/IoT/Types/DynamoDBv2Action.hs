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
    ddaRoleARN,
    ddaPutItem,
  )
where

import Network.AWS.IoT.Types.PutItemInput
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an action to write to a DynamoDB table.
--
-- This DynamoDB action writes each attribute in the message payload into it's own column in the DynamoDB table.
--
-- /See:/ 'mkDynamoDBv2Action' smart constructor.
data DynamoDBv2Action = DynamoDBv2Action'
  { roleARN :: Lude.Text,
    putItem :: PutItemInput
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DynamoDBv2Action' with the minimum fields required to make a request.
--
-- * 'putItem' - Specifies the DynamoDB table to which the message data will be written. For example:
--
-- @{ "dynamoDBv2": { "roleArn": "aws:iam:12341251:my-role" "putItem": { "tableName": "my-table" } } }@
-- Each attribute in the message payload will be written to a separate column in the DynamoDB database.
-- * 'roleARN' - The ARN of the IAM role that grants access to the DynamoDB table.
mkDynamoDBv2Action ::
  -- | 'roleARN'
  Lude.Text ->
  -- | 'putItem'
  PutItemInput ->
  DynamoDBv2Action
mkDynamoDBv2Action pRoleARN_ pPutItem_ =
  DynamoDBv2Action' {roleARN = pRoleARN_, putItem = pPutItem_}

-- | The ARN of the IAM role that grants access to the DynamoDB table.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddaRoleARN :: Lens.Lens' DynamoDBv2Action Lude.Text
ddaRoleARN = Lens.lens (roleARN :: DynamoDBv2Action -> Lude.Text) (\s a -> s {roleARN = a} :: DynamoDBv2Action)
{-# DEPRECATED ddaRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | Specifies the DynamoDB table to which the message data will be written. For example:
--
-- @{ "dynamoDBv2": { "roleArn": "aws:iam:12341251:my-role" "putItem": { "tableName": "my-table" } } }@
-- Each attribute in the message payload will be written to a separate column in the DynamoDB database.
--
-- /Note:/ Consider using 'putItem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddaPutItem :: Lens.Lens' DynamoDBv2Action PutItemInput
ddaPutItem = Lens.lens (putItem :: DynamoDBv2Action -> PutItemInput) (\s a -> s {putItem = a} :: DynamoDBv2Action)
{-# DEPRECATED ddaPutItem "Use generic-lens or generic-optics with 'putItem' instead." #-}

instance Lude.FromJSON DynamoDBv2Action where
  parseJSON =
    Lude.withObject
      "DynamoDBv2Action"
      ( \x ->
          DynamoDBv2Action'
            Lude.<$> (x Lude..: "roleArn") Lude.<*> (x Lude..: "putItem")
      )

instance Lude.ToJSON DynamoDBv2Action where
  toJSON DynamoDBv2Action' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("roleArn" Lude..= roleARN),
            Lude.Just ("putItem" Lude..= putItem)
          ]
      )
