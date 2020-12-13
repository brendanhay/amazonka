{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.DynamoDBAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.DynamoDBAction
  ( DynamoDBAction (..),

    -- * Smart constructor
    mkDynamoDBAction,

    -- * Lenses
    ddbaHashKeyField,
    ddbaHashKeyType,
    ddbaOperation,
    ddbaRangeKeyType,
    ddbaPayloadField,
    ddbaRangeKeyField,
    ddbaRangeKeyValue,
    ddbaHashKeyValue,
    ddbaTableName,
    ddbaRoleARN,
  )
where

import Network.AWS.IoT.Types.DynamoKeyType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an action to write to a DynamoDB table.
--
-- The @tableName@ , @hashKeyField@ , and @rangeKeyField@ values must match the values used when you created the table.
-- The @hashKeyValue@ and @rangeKeyvalue@ fields use a substitution template syntax. These templates provide data at runtime. The syntax is as follows: ${/sql-expression/ }.
-- You can specify any valid expression in a WHERE or SELECT clause, including JSON properties, comparisons, calculations, and functions. For example, the following field uses the third level of the topic:
-- @"hashKeyValue": "${topic(3)}"@
-- The following field uses the timestamp:
-- @"rangeKeyValue": "${timestamp()}"@
--
-- /See:/ 'mkDynamoDBAction' smart constructor.
data DynamoDBAction = DynamoDBAction'
  { -- | The hash key name.
    hashKeyField :: Lude.Text,
    -- | The hash key type. Valid values are "STRING" or "NUMBER"
    hashKeyType :: Lude.Maybe DynamoKeyType,
    -- | The type of operation to be performed. This follows the substitution template, so it can be @> {operation}@ , but the substitution must result in one of the following: @INSERT@ , @UPDATE@ , or @DELETE@ .
    operation :: Lude.Maybe Lude.Text,
    -- | The range key type. Valid values are "STRING" or "NUMBER"
    rangeKeyType :: Lude.Maybe DynamoKeyType,
    -- | The action payload. This name can be customized.
    payloadField :: Lude.Maybe Lude.Text,
    -- | The range key name.
    rangeKeyField :: Lude.Maybe Lude.Text,
    -- | The range key value.
    rangeKeyValue :: Lude.Maybe Lude.Text,
    -- | The hash key value.
    hashKeyValue :: Lude.Text,
    -- | The name of the DynamoDB table.
    tableName :: Lude.Text,
    -- | The ARN of the IAM role that grants access to the DynamoDB table.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DynamoDBAction' with the minimum fields required to make a request.
--
-- * 'hashKeyField' - The hash key name.
-- * 'hashKeyType' - The hash key type. Valid values are "STRING" or "NUMBER"
-- * 'operation' - The type of operation to be performed. This follows the substitution template, so it can be @> {operation}@ , but the substitution must result in one of the following: @INSERT@ , @UPDATE@ , or @DELETE@ .
-- * 'rangeKeyType' - The range key type. Valid values are "STRING" or "NUMBER"
-- * 'payloadField' - The action payload. This name can be customized.
-- * 'rangeKeyField' - The range key name.
-- * 'rangeKeyValue' - The range key value.
-- * 'hashKeyValue' - The hash key value.
-- * 'tableName' - The name of the DynamoDB table.
-- * 'roleARN' - The ARN of the IAM role that grants access to the DynamoDB table.
mkDynamoDBAction ::
  -- | 'hashKeyField'
  Lude.Text ->
  -- | 'hashKeyValue'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  DynamoDBAction
mkDynamoDBAction
  pHashKeyField_
  pHashKeyValue_
  pTableName_
  pRoleARN_ =
    DynamoDBAction'
      { hashKeyField = pHashKeyField_,
        hashKeyType = Lude.Nothing,
        operation = Lude.Nothing,
        rangeKeyType = Lude.Nothing,
        payloadField = Lude.Nothing,
        rangeKeyField = Lude.Nothing,
        rangeKeyValue = Lude.Nothing,
        hashKeyValue = pHashKeyValue_,
        tableName = pTableName_,
        roleARN = pRoleARN_
      }

-- | The hash key name.
--
-- /Note:/ Consider using 'hashKeyField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbaHashKeyField :: Lens.Lens' DynamoDBAction Lude.Text
ddbaHashKeyField = Lens.lens (hashKeyField :: DynamoDBAction -> Lude.Text) (\s a -> s {hashKeyField = a} :: DynamoDBAction)
{-# DEPRECATED ddbaHashKeyField "Use generic-lens or generic-optics with 'hashKeyField' instead." #-}

-- | The hash key type. Valid values are "STRING" or "NUMBER"
--
-- /Note:/ Consider using 'hashKeyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbaHashKeyType :: Lens.Lens' DynamoDBAction (Lude.Maybe DynamoKeyType)
ddbaHashKeyType = Lens.lens (hashKeyType :: DynamoDBAction -> Lude.Maybe DynamoKeyType) (\s a -> s {hashKeyType = a} :: DynamoDBAction)
{-# DEPRECATED ddbaHashKeyType "Use generic-lens or generic-optics with 'hashKeyType' instead." #-}

-- | The type of operation to be performed. This follows the substitution template, so it can be @> {operation}@ , but the substitution must result in one of the following: @INSERT@ , @UPDATE@ , or @DELETE@ .
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbaOperation :: Lens.Lens' DynamoDBAction (Lude.Maybe Lude.Text)
ddbaOperation = Lens.lens (operation :: DynamoDBAction -> Lude.Maybe Lude.Text) (\s a -> s {operation = a} :: DynamoDBAction)
{-# DEPRECATED ddbaOperation "Use generic-lens or generic-optics with 'operation' instead." #-}

-- | The range key type. Valid values are "STRING" or "NUMBER"
--
-- /Note:/ Consider using 'rangeKeyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbaRangeKeyType :: Lens.Lens' DynamoDBAction (Lude.Maybe DynamoKeyType)
ddbaRangeKeyType = Lens.lens (rangeKeyType :: DynamoDBAction -> Lude.Maybe DynamoKeyType) (\s a -> s {rangeKeyType = a} :: DynamoDBAction)
{-# DEPRECATED ddbaRangeKeyType "Use generic-lens or generic-optics with 'rangeKeyType' instead." #-}

-- | The action payload. This name can be customized.
--
-- /Note:/ Consider using 'payloadField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbaPayloadField :: Lens.Lens' DynamoDBAction (Lude.Maybe Lude.Text)
ddbaPayloadField = Lens.lens (payloadField :: DynamoDBAction -> Lude.Maybe Lude.Text) (\s a -> s {payloadField = a} :: DynamoDBAction)
{-# DEPRECATED ddbaPayloadField "Use generic-lens or generic-optics with 'payloadField' instead." #-}

-- | The range key name.
--
-- /Note:/ Consider using 'rangeKeyField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbaRangeKeyField :: Lens.Lens' DynamoDBAction (Lude.Maybe Lude.Text)
ddbaRangeKeyField = Lens.lens (rangeKeyField :: DynamoDBAction -> Lude.Maybe Lude.Text) (\s a -> s {rangeKeyField = a} :: DynamoDBAction)
{-# DEPRECATED ddbaRangeKeyField "Use generic-lens or generic-optics with 'rangeKeyField' instead." #-}

-- | The range key value.
--
-- /Note:/ Consider using 'rangeKeyValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbaRangeKeyValue :: Lens.Lens' DynamoDBAction (Lude.Maybe Lude.Text)
ddbaRangeKeyValue = Lens.lens (rangeKeyValue :: DynamoDBAction -> Lude.Maybe Lude.Text) (\s a -> s {rangeKeyValue = a} :: DynamoDBAction)
{-# DEPRECATED ddbaRangeKeyValue "Use generic-lens or generic-optics with 'rangeKeyValue' instead." #-}

-- | The hash key value.
--
-- /Note:/ Consider using 'hashKeyValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbaHashKeyValue :: Lens.Lens' DynamoDBAction Lude.Text
ddbaHashKeyValue = Lens.lens (hashKeyValue :: DynamoDBAction -> Lude.Text) (\s a -> s {hashKeyValue = a} :: DynamoDBAction)
{-# DEPRECATED ddbaHashKeyValue "Use generic-lens or generic-optics with 'hashKeyValue' instead." #-}

-- | The name of the DynamoDB table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbaTableName :: Lens.Lens' DynamoDBAction Lude.Text
ddbaTableName = Lens.lens (tableName :: DynamoDBAction -> Lude.Text) (\s a -> s {tableName = a} :: DynamoDBAction)
{-# DEPRECATED ddbaTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | The ARN of the IAM role that grants access to the DynamoDB table.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbaRoleARN :: Lens.Lens' DynamoDBAction Lude.Text
ddbaRoleARN = Lens.lens (roleARN :: DynamoDBAction -> Lude.Text) (\s a -> s {roleARN = a} :: DynamoDBAction)
{-# DEPRECATED ddbaRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON DynamoDBAction where
  parseJSON =
    Lude.withObject
      "DynamoDBAction"
      ( \x ->
          DynamoDBAction'
            Lude.<$> (x Lude..: "hashKeyField")
            Lude.<*> (x Lude..:? "hashKeyType")
            Lude.<*> (x Lude..:? "operation")
            Lude.<*> (x Lude..:? "rangeKeyType")
            Lude.<*> (x Lude..:? "payloadField")
            Lude.<*> (x Lude..:? "rangeKeyField")
            Lude.<*> (x Lude..:? "rangeKeyValue")
            Lude.<*> (x Lude..: "hashKeyValue")
            Lude.<*> (x Lude..: "tableName")
            Lude.<*> (x Lude..: "roleArn")
      )

instance Lude.ToJSON DynamoDBAction where
  toJSON DynamoDBAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("hashKeyField" Lude..= hashKeyField),
            ("hashKeyType" Lude..=) Lude.<$> hashKeyType,
            ("operation" Lude..=) Lude.<$> operation,
            ("rangeKeyType" Lude..=) Lude.<$> rangeKeyType,
            ("payloadField" Lude..=) Lude.<$> payloadField,
            ("rangeKeyField" Lude..=) Lude.<$> rangeKeyField,
            ("rangeKeyValue" Lude..=) Lude.<$> rangeKeyValue,
            Lude.Just ("hashKeyValue" Lude..= hashKeyValue),
            Lude.Just ("tableName" Lude..= tableName),
            Lude.Just ("roleArn" Lude..= roleARN)
          ]
      )
