{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.DynamoDBAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.DynamoDBAction
  ( DynamoDBAction (..)
  -- * Smart constructor
  , mkDynamoDBAction
  -- * Lenses
  , ddbaTableName
  , ddbaRoleArn
  , ddbaHashKeyField
  , ddbaHashKeyValue
  , ddbaHashKeyType
  , ddbaOperation
  , ddbaPayloadField
  , ddbaRangeKeyField
  , ddbaRangeKeyType
  , ddbaRangeKeyValue
  ) where

import qualified Network.AWS.IoT.Types.AwsArn as Types
import qualified Network.AWS.IoT.Types.DynamoKeyType as Types
import qualified Network.AWS.IoT.Types.HashKeyField as Types
import qualified Network.AWS.IoT.Types.HashKeyValue as Types
import qualified Network.AWS.IoT.Types.Operation as Types
import qualified Network.AWS.IoT.Types.PayloadField as Types
import qualified Network.AWS.IoT.Types.RangeKeyField as Types
import qualified Network.AWS.IoT.Types.RangeKeyValue as Types
import qualified Network.AWS.IoT.Types.TableName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

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
  { tableName :: Types.TableName
    -- ^ The name of the DynamoDB table.
  , roleArn :: Types.AwsArn
    -- ^ The ARN of the IAM role that grants access to the DynamoDB table.
  , hashKeyField :: Types.HashKeyField
    -- ^ The hash key name.
  , hashKeyValue :: Types.HashKeyValue
    -- ^ The hash key value.
  , hashKeyType :: Core.Maybe Types.DynamoKeyType
    -- ^ The hash key type. Valid values are "STRING" or "NUMBER"
  , operation :: Core.Maybe Types.Operation
    -- ^ The type of operation to be performed. This follows the substitution template, so it can be @> {operation}@ , but the substitution must result in one of the following: @INSERT@ , @UPDATE@ , or @DELETE@ .
  , payloadField :: Core.Maybe Types.PayloadField
    -- ^ The action payload. This name can be customized.
  , rangeKeyField :: Core.Maybe Types.RangeKeyField
    -- ^ The range key name.
  , rangeKeyType :: Core.Maybe Types.DynamoKeyType
    -- ^ The range key type. Valid values are "STRING" or "NUMBER"
  , rangeKeyValue :: Core.Maybe Types.RangeKeyValue
    -- ^ The range key value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DynamoDBAction' value with any optional fields omitted.
mkDynamoDBAction
    :: Types.TableName -- ^ 'tableName'
    -> Types.AwsArn -- ^ 'roleArn'
    -> Types.HashKeyField -- ^ 'hashKeyField'
    -> Types.HashKeyValue -- ^ 'hashKeyValue'
    -> DynamoDBAction
mkDynamoDBAction tableName roleArn hashKeyField hashKeyValue
  = DynamoDBAction'{tableName, roleArn, hashKeyField, hashKeyValue,
                    hashKeyType = Core.Nothing, operation = Core.Nothing,
                    payloadField = Core.Nothing, rangeKeyField = Core.Nothing,
                    rangeKeyType = Core.Nothing, rangeKeyValue = Core.Nothing}

-- | The name of the DynamoDB table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbaTableName :: Lens.Lens' DynamoDBAction Types.TableName
ddbaTableName = Lens.field @"tableName"
{-# INLINEABLE ddbaTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

-- | The ARN of the IAM role that grants access to the DynamoDB table.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbaRoleArn :: Lens.Lens' DynamoDBAction Types.AwsArn
ddbaRoleArn = Lens.field @"roleArn"
{-# INLINEABLE ddbaRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The hash key name.
--
-- /Note:/ Consider using 'hashKeyField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbaHashKeyField :: Lens.Lens' DynamoDBAction Types.HashKeyField
ddbaHashKeyField = Lens.field @"hashKeyField"
{-# INLINEABLE ddbaHashKeyField #-}
{-# DEPRECATED hashKeyField "Use generic-lens or generic-optics with 'hashKeyField' instead"  #-}

-- | The hash key value.
--
-- /Note:/ Consider using 'hashKeyValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbaHashKeyValue :: Lens.Lens' DynamoDBAction Types.HashKeyValue
ddbaHashKeyValue = Lens.field @"hashKeyValue"
{-# INLINEABLE ddbaHashKeyValue #-}
{-# DEPRECATED hashKeyValue "Use generic-lens or generic-optics with 'hashKeyValue' instead"  #-}

-- | The hash key type. Valid values are "STRING" or "NUMBER"
--
-- /Note:/ Consider using 'hashKeyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbaHashKeyType :: Lens.Lens' DynamoDBAction (Core.Maybe Types.DynamoKeyType)
ddbaHashKeyType = Lens.field @"hashKeyType"
{-# INLINEABLE ddbaHashKeyType #-}
{-# DEPRECATED hashKeyType "Use generic-lens or generic-optics with 'hashKeyType' instead"  #-}

-- | The type of operation to be performed. This follows the substitution template, so it can be @> {operation}@ , but the substitution must result in one of the following: @INSERT@ , @UPDATE@ , or @DELETE@ .
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbaOperation :: Lens.Lens' DynamoDBAction (Core.Maybe Types.Operation)
ddbaOperation = Lens.field @"operation"
{-# INLINEABLE ddbaOperation #-}
{-# DEPRECATED operation "Use generic-lens or generic-optics with 'operation' instead"  #-}

-- | The action payload. This name can be customized.
--
-- /Note:/ Consider using 'payloadField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbaPayloadField :: Lens.Lens' DynamoDBAction (Core.Maybe Types.PayloadField)
ddbaPayloadField = Lens.field @"payloadField"
{-# INLINEABLE ddbaPayloadField #-}
{-# DEPRECATED payloadField "Use generic-lens or generic-optics with 'payloadField' instead"  #-}

-- | The range key name.
--
-- /Note:/ Consider using 'rangeKeyField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbaRangeKeyField :: Lens.Lens' DynamoDBAction (Core.Maybe Types.RangeKeyField)
ddbaRangeKeyField = Lens.field @"rangeKeyField"
{-# INLINEABLE ddbaRangeKeyField #-}
{-# DEPRECATED rangeKeyField "Use generic-lens or generic-optics with 'rangeKeyField' instead"  #-}

-- | The range key type. Valid values are "STRING" or "NUMBER"
--
-- /Note:/ Consider using 'rangeKeyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbaRangeKeyType :: Lens.Lens' DynamoDBAction (Core.Maybe Types.DynamoKeyType)
ddbaRangeKeyType = Lens.field @"rangeKeyType"
{-# INLINEABLE ddbaRangeKeyType #-}
{-# DEPRECATED rangeKeyType "Use generic-lens or generic-optics with 'rangeKeyType' instead"  #-}

-- | The range key value.
--
-- /Note:/ Consider using 'rangeKeyValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbaRangeKeyValue :: Lens.Lens' DynamoDBAction (Core.Maybe Types.RangeKeyValue)
ddbaRangeKeyValue = Lens.field @"rangeKeyValue"
{-# INLINEABLE ddbaRangeKeyValue #-}
{-# DEPRECATED rangeKeyValue "Use generic-lens or generic-optics with 'rangeKeyValue' instead"  #-}

instance Core.FromJSON DynamoDBAction where
        toJSON DynamoDBAction{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("tableName" Core..= tableName),
                  Core.Just ("roleArn" Core..= roleArn),
                  Core.Just ("hashKeyField" Core..= hashKeyField),
                  Core.Just ("hashKeyValue" Core..= hashKeyValue),
                  ("hashKeyType" Core..=) Core.<$> hashKeyType,
                  ("operation" Core..=) Core.<$> operation,
                  ("payloadField" Core..=) Core.<$> payloadField,
                  ("rangeKeyField" Core..=) Core.<$> rangeKeyField,
                  ("rangeKeyType" Core..=) Core.<$> rangeKeyType,
                  ("rangeKeyValue" Core..=) Core.<$> rangeKeyValue])

instance Core.FromJSON DynamoDBAction where
        parseJSON
          = Core.withObject "DynamoDBAction" Core.$
              \ x ->
                DynamoDBAction' Core.<$>
                  (x Core..: "tableName") Core.<*> x Core..: "roleArn" Core.<*>
                    x Core..: "hashKeyField"
                    Core.<*> x Core..: "hashKeyValue"
                    Core.<*> x Core..:? "hashKeyType"
                    Core.<*> x Core..:? "operation"
                    Core.<*> x Core..:? "payloadField"
                    Core.<*> x Core..:? "rangeKeyField"
                    Core.<*> x Core..:? "rangeKeyType"
                    Core.<*> x Core..:? "rangeKeyValue"
