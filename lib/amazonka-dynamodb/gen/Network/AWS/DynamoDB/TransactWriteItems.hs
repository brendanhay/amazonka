{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.TransactWriteItems
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- @TransactWriteItems@ is a synchronous write operation that groups up to 25 action requests. These actions can target items in different tables, but not in different AWS accounts or Regions, and no two actions can target the same item. For example, you cannot both @ConditionCheck@ and @Update@ the same item. The aggregate size of the items in the transaction cannot exceed 4 MB.
--
-- The actions are completed atomically so that either all of them succeed, or all of them fail. They are defined by the following objects:
--
--     * @Put@    Initiates a @PutItem@ operation to write a new item. This structure specifies the primary key of the item to be written, the name of the table to write it in, an optional condition expression that must be satisfied for the write to succeed, a list of the item's attributes, and a field indicating whether to retrieve the item's attributes if the condition is not met.
--
--
--     * @Update@    Initiates an @UpdateItem@ operation to update an existing item. This structure specifies the primary key of the item to be updated, the name of the table where it resides, an optional condition expression that must be satisfied for the update to succeed, an expression that defines one or more attributes to be updated, and a field indicating whether to retrieve the item's attributes if the condition is not met.
--
--
--     * @Delete@    Initiates a @DeleteItem@ operation to delete an existing item. This structure specifies the primary key of the item to be deleted, the name of the table where it resides, an optional condition expression that must be satisfied for the deletion to succeed, and a field indicating whether to retrieve the item's attributes if the condition is not met.
--
--
--     * @ConditionCheck@    Applies a condition to an item that is not being modified by the transaction. This structure specifies the primary key of the item to be checked, the name of the table where it resides, a condition expression that must be satisfied for the transaction to succeed, and a field indicating whether to retrieve the item's attributes if the condition is not met.
--
--
-- DynamoDB rejects the entire @TransactWriteItems@ request if any of the following is true:
--
--     * A condition in one of the condition expressions is not met.
--
--
--     * An ongoing operation is in the process of updating the same item.
--
--
--     * There is insufficient provisioned capacity for the transaction to be completed.
--
--
--     * An item size becomes too large (bigger than 400 KB), a local secondary index (LSI) becomes too large, or a similar validation error occurs because of changes made by the transaction.
--
--
--     * The aggregate size of the items in the transaction exceeds 4 MB.
--
--
--     * There is a user error, such as an invalid data format.
--
--
module Network.AWS.DynamoDB.TransactWriteItems
    (
    -- * Creating a request
      TransactWriteItems (..)
    , mkTransactWriteItems
    -- ** Request lenses
    , twiTransactItems
    , twiClientRequestToken
    , twiReturnConsumedCapacity
    , twiReturnItemCollectionMetrics

    -- * Destructuring the response
    , TransactWriteItemsResponse (..)
    , mkTransactWriteItemsResponse
    -- ** Response lenses
    , twirrsConsumedCapacity
    , twirrsItemCollectionMetrics
    , twirrsResponseStatus
    ) where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkTransactWriteItems' smart constructor.
data TransactWriteItems = TransactWriteItems'
  { transactItems :: Core.NonEmpty Types.TransactWriteItem
    -- ^ An ordered array of up to 25 @TransactWriteItem@ objects, each of which contains a @ConditionCheck@ , @Put@ , @Update@ , or @Delete@ object. These can operate on items in different tables, but the tables must reside in the same AWS account and Region, and no two of them can operate on the same item. 
  , clientRequestToken :: Core.Maybe Types.ClientRequestToken
    -- ^ Providing a @ClientRequestToken@ makes the call to @TransactWriteItems@ idempotent, meaning that multiple identical calls have the same effect as one single call.
--
-- Although multiple identical calls using the same client request token produce the same result on the server (no side effects), the responses to the calls might not be the same. If the @ReturnConsumedCapacity>@ parameter is set, then the initial @TransactWriteItems@ call returns the amount of write capacity units consumed in making the changes. Subsequent @TransactWriteItems@ calls with the same client token return the number of read capacity units consumed in reading the item.
-- A client request token is valid for 10 minutes after the first request that uses it is completed. After 10 minutes, any request with the same client token is treated as a new request. Do not resubmit the same request with the same client token for more than 10 minutes, or the result might not be idempotent.
-- If you submit a request with the same client token but a change in other parameters within the 10-minute idempotency window, DynamoDB returns an @IdempotentParameterMismatch@ exception.
  , returnConsumedCapacity :: Core.Maybe Types.ReturnConsumedCapacity
  , returnItemCollectionMetrics :: Core.Maybe Types.ReturnItemCollectionMetrics
    -- ^ Determines whether item collection metrics are returned. If set to @SIZE@ , the response includes statistics about item collections (if any), that were modified during the operation and are returned in the response. If set to @NONE@ (the default), no statistics are returned. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransactWriteItems' value with any optional fields omitted.
mkTransactWriteItems
    :: Core.NonEmpty Types.TransactWriteItem -- ^ 'transactItems'
    -> TransactWriteItems
mkTransactWriteItems transactItems
  = TransactWriteItems'{transactItems,
                        clientRequestToken = Core.Nothing,
                        returnConsumedCapacity = Core.Nothing,
                        returnItemCollectionMetrics = Core.Nothing}

-- | An ordered array of up to 25 @TransactWriteItem@ objects, each of which contains a @ConditionCheck@ , @Put@ , @Update@ , or @Delete@ object. These can operate on items in different tables, but the tables must reside in the same AWS account and Region, and no two of them can operate on the same item. 
--
-- /Note:/ Consider using 'transactItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
twiTransactItems :: Lens.Lens' TransactWriteItems (Core.NonEmpty Types.TransactWriteItem)
twiTransactItems = Lens.field @"transactItems"
{-# INLINEABLE twiTransactItems #-}
{-# DEPRECATED transactItems "Use generic-lens or generic-optics with 'transactItems' instead"  #-}

-- | Providing a @ClientRequestToken@ makes the call to @TransactWriteItems@ idempotent, meaning that multiple identical calls have the same effect as one single call.
--
-- Although multiple identical calls using the same client request token produce the same result on the server (no side effects), the responses to the calls might not be the same. If the @ReturnConsumedCapacity>@ parameter is set, then the initial @TransactWriteItems@ call returns the amount of write capacity units consumed in making the changes. Subsequent @TransactWriteItems@ calls with the same client token return the number of read capacity units consumed in reading the item.
-- A client request token is valid for 10 minutes after the first request that uses it is completed. After 10 minutes, any request with the same client token is treated as a new request. Do not resubmit the same request with the same client token for more than 10 minutes, or the result might not be idempotent.
-- If you submit a request with the same client token but a change in other parameters within the 10-minute idempotency window, DynamoDB returns an @IdempotentParameterMismatch@ exception.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
twiClientRequestToken :: Lens.Lens' TransactWriteItems (Core.Maybe Types.ClientRequestToken)
twiClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE twiClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'returnConsumedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
twiReturnConsumedCapacity :: Lens.Lens' TransactWriteItems (Core.Maybe Types.ReturnConsumedCapacity)
twiReturnConsumedCapacity = Lens.field @"returnConsumedCapacity"
{-# INLINEABLE twiReturnConsumedCapacity #-}
{-# DEPRECATED returnConsumedCapacity "Use generic-lens or generic-optics with 'returnConsumedCapacity' instead"  #-}

-- | Determines whether item collection metrics are returned. If set to @SIZE@ , the response includes statistics about item collections (if any), that were modified during the operation and are returned in the response. If set to @NONE@ (the default), no statistics are returned. 
--
-- /Note:/ Consider using 'returnItemCollectionMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
twiReturnItemCollectionMetrics :: Lens.Lens' TransactWriteItems (Core.Maybe Types.ReturnItemCollectionMetrics)
twiReturnItemCollectionMetrics = Lens.field @"returnItemCollectionMetrics"
{-# INLINEABLE twiReturnItemCollectionMetrics #-}
{-# DEPRECATED returnItemCollectionMetrics "Use generic-lens or generic-optics with 'returnItemCollectionMetrics' instead"  #-}

instance Core.ToQuery TransactWriteItems where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders TransactWriteItems where
        toHeaders TransactWriteItems{..}
          = Core.pure
              ("X-Amz-Target", "DynamoDB_20120810.TransactWriteItems")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON TransactWriteItems where
        toJSON TransactWriteItems{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TransactItems" Core..= transactItems),
                  ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
                  ("ReturnConsumedCapacity" Core..=) Core.<$> returnConsumedCapacity,
                  ("ReturnItemCollectionMetrics" Core..=) Core.<$>
                    returnItemCollectionMetrics])

instance Core.AWSRequest TransactWriteItems where
        type Rs TransactWriteItems = TransactWriteItemsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 TransactWriteItemsResponse' Core.<$>
                   (x Core..:? "ConsumedCapacity") Core.<*>
                     x Core..:? "ItemCollectionMetrics"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkTransactWriteItemsResponse' smart constructor.
data TransactWriteItemsResponse = TransactWriteItemsResponse'
  { consumedCapacity :: Core.Maybe [Types.ConsumedCapacity]
    -- ^ The capacity units consumed by the entire @TransactWriteItems@ operation. The values of the list are ordered according to the ordering of the @TransactItems@ request parameter. 
  , itemCollectionMetrics :: Core.Maybe (Core.HashMap Types.TableName [Types.ItemCollectionMetrics])
    -- ^ A list of tables that were processed by @TransactWriteItems@ and, for each table, information about any item collections that were affected by individual @UpdateItem@ , @PutItem@ , or @DeleteItem@ operations. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransactWriteItemsResponse' value with any optional fields omitted.
mkTransactWriteItemsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> TransactWriteItemsResponse
mkTransactWriteItemsResponse responseStatus
  = TransactWriteItemsResponse'{consumedCapacity = Core.Nothing,
                                itemCollectionMetrics = Core.Nothing, responseStatus}

-- | The capacity units consumed by the entire @TransactWriteItems@ operation. The values of the list are ordered according to the ordering of the @TransactItems@ request parameter. 
--
-- /Note:/ Consider using 'consumedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
twirrsConsumedCapacity :: Lens.Lens' TransactWriteItemsResponse (Core.Maybe [Types.ConsumedCapacity])
twirrsConsumedCapacity = Lens.field @"consumedCapacity"
{-# INLINEABLE twirrsConsumedCapacity #-}
{-# DEPRECATED consumedCapacity "Use generic-lens or generic-optics with 'consumedCapacity' instead"  #-}

-- | A list of tables that were processed by @TransactWriteItems@ and, for each table, information about any item collections that were affected by individual @UpdateItem@ , @PutItem@ , or @DeleteItem@ operations. 
--
-- /Note:/ Consider using 'itemCollectionMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
twirrsItemCollectionMetrics :: Lens.Lens' TransactWriteItemsResponse (Core.Maybe (Core.HashMap Types.TableName [Types.ItemCollectionMetrics]))
twirrsItemCollectionMetrics = Lens.field @"itemCollectionMetrics"
{-# INLINEABLE twirrsItemCollectionMetrics #-}
{-# DEPRECATED itemCollectionMetrics "Use generic-lens or generic-optics with 'itemCollectionMetrics' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
twirrsResponseStatus :: Lens.Lens' TransactWriteItemsResponse Core.Int
twirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE twirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
