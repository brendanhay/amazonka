{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.DescribeLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current provisioned-capacity quotas for your AWS account in a Region, both for the Region as a whole and for any one DynamoDB table that you create there.
--
-- When you establish an AWS account, the account has initial quotas on the maximum read capacity units and write capacity units that you can provision across all of your DynamoDB tables in a given Region. Also, there are per-table quotas that apply when you create a table there. For more information, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> page in the /Amazon DynamoDB Developer Guide/ .
-- Although you can increase these quotas by filing a case at <https://console.aws.amazon.com/support/home#/ AWS Support Center> , obtaining the increase is not instantaneous. The @DescribeLimits@ action lets you write code to compare the capacity you are currently using to those quotas imposed by your account so that you have enough time to apply for an increase before you hit a quota.
-- For example, you could use one of the AWS SDKs to do the following:
--
--     * Call @DescribeLimits@ for a particular Region to obtain your current account quotas on provisioned capacity there.
--
--
--     * Create a variable to hold the aggregate read capacity units provisioned for all your tables in that Region, and one to hold the aggregate write capacity units. Zero them both.
--
--
--     * Call @ListTables@ to obtain a list of all your DynamoDB tables.
--
--
--     * For each table name listed by @ListTables@ , do the following:
--
--     * Call @DescribeTable@ with the table name.
--
--
--     * Use the data returned by @DescribeTable@ to add the read capacity units and write capacity units provisioned for the table itself to your variables.
--
--
--     * If the table has one or more global secondary indexes (GSIs), loop over these GSIs and add their provisioned capacity values to your variables as well.
--
--
--
--
--     * Report the account quotas for that Region returned by @DescribeLimits@ , along with the total current provisioned capacity levels you have calculated.
--
--
-- This will let you see whether you are getting close to your account-level quotas.
-- The per-table quotas apply only when you are creating a new table. They restrict the sum of the provisioned capacity of the new table itself and all its global secondary indexes.
-- For existing tables and their GSIs, DynamoDB doesn't let you increase provisioned capacity extremely rapidly, but the only quota that applies is that the aggregate provisioned capacity over all your tables and GSIs cannot exceed either of the per-account quotas.
-- The @DescribeLimits@ Request element has no content.
module Network.AWS.DynamoDB.DescribeLimits
  ( -- * Creating a request
    DescribeLimits (..),
    mkDescribeLimits,

    -- * Destructuring the response
    DescribeLimitsResponse (..),
    mkDescribeLimitsResponse,

    -- ** Response lenses
    dlrrsAccountMaxReadCapacityUnits,
    dlrrsAccountMaxWriteCapacityUnits,
    dlrrsTableMaxReadCapacityUnits,
    dlrrsTableMaxWriteCapacityUnits,
    dlrrsResponseStatus,
  )
where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DescribeLimits@ operation. Has no content.
--
-- /See:/ 'mkDescribeLimits' smart constructor.
data DescribeLimits = DescribeLimits'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLimits' value with any optional fields omitted.
mkDescribeLimits ::
  DescribeLimits
mkDescribeLimits = DescribeLimits'

instance Core.FromJSON DescribeLimits where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DescribeLimits where
  type Rs DescribeLimits = DescribeLimitsResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DynamoDB_20120810.DescribeLimits")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLimitsResponse'
            Core.<$> (x Core..:? "AccountMaxReadCapacityUnits")
            Core.<*> (x Core..:? "AccountMaxWriteCapacityUnits")
            Core.<*> (x Core..:? "TableMaxReadCapacityUnits")
            Core.<*> (x Core..:? "TableMaxWriteCapacityUnits")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a @DescribeLimits@ operation.
--
-- /See:/ 'mkDescribeLimitsResponse' smart constructor.
data DescribeLimitsResponse = DescribeLimitsResponse'
  { -- | The maximum total read capacity units that your account allows you to provision across all of your tables in this Region.
    accountMaxReadCapacityUnits :: Core.Maybe Core.Natural,
    -- | The maximum total write capacity units that your account allows you to provision across all of your tables in this Region.
    accountMaxWriteCapacityUnits :: Core.Maybe Core.Natural,
    -- | The maximum read capacity units that your account allows you to provision for a new table that you are creating in this Region, including the read capacity units provisioned for its global secondary indexes (GSIs).
    tableMaxReadCapacityUnits :: Core.Maybe Core.Natural,
    -- | The maximum write capacity units that your account allows you to provision for a new table that you are creating in this Region, including the write capacity units provisioned for its global secondary indexes (GSIs).
    tableMaxWriteCapacityUnits :: Core.Maybe Core.Natural,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLimitsResponse' value with any optional fields omitted.
mkDescribeLimitsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeLimitsResponse
mkDescribeLimitsResponse responseStatus =
  DescribeLimitsResponse'
    { accountMaxReadCapacityUnits =
        Core.Nothing,
      accountMaxWriteCapacityUnits = Core.Nothing,
      tableMaxReadCapacityUnits = Core.Nothing,
      tableMaxWriteCapacityUnits = Core.Nothing,
      responseStatus
    }

-- | The maximum total read capacity units that your account allows you to provision across all of your tables in this Region.
--
-- /Note:/ Consider using 'accountMaxReadCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrrsAccountMaxReadCapacityUnits :: Lens.Lens' DescribeLimitsResponse (Core.Maybe Core.Natural)
dlrrsAccountMaxReadCapacityUnits = Lens.field @"accountMaxReadCapacityUnits"
{-# DEPRECATED dlrrsAccountMaxReadCapacityUnits "Use generic-lens or generic-optics with 'accountMaxReadCapacityUnits' instead." #-}

-- | The maximum total write capacity units that your account allows you to provision across all of your tables in this Region.
--
-- /Note:/ Consider using 'accountMaxWriteCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrrsAccountMaxWriteCapacityUnits :: Lens.Lens' DescribeLimitsResponse (Core.Maybe Core.Natural)
dlrrsAccountMaxWriteCapacityUnits = Lens.field @"accountMaxWriteCapacityUnits"
{-# DEPRECATED dlrrsAccountMaxWriteCapacityUnits "Use generic-lens or generic-optics with 'accountMaxWriteCapacityUnits' instead." #-}

-- | The maximum read capacity units that your account allows you to provision for a new table that you are creating in this Region, including the read capacity units provisioned for its global secondary indexes (GSIs).
--
-- /Note:/ Consider using 'tableMaxReadCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrrsTableMaxReadCapacityUnits :: Lens.Lens' DescribeLimitsResponse (Core.Maybe Core.Natural)
dlrrsTableMaxReadCapacityUnits = Lens.field @"tableMaxReadCapacityUnits"
{-# DEPRECATED dlrrsTableMaxReadCapacityUnits "Use generic-lens or generic-optics with 'tableMaxReadCapacityUnits' instead." #-}

-- | The maximum write capacity units that your account allows you to provision for a new table that you are creating in this Region, including the write capacity units provisioned for its global secondary indexes (GSIs).
--
-- /Note:/ Consider using 'tableMaxWriteCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrrsTableMaxWriteCapacityUnits :: Lens.Lens' DescribeLimitsResponse (Core.Maybe Core.Natural)
dlrrsTableMaxWriteCapacityUnits = Lens.field @"tableMaxWriteCapacityUnits"
{-# DEPRECATED dlrrsTableMaxWriteCapacityUnits "Use generic-lens or generic-optics with 'tableMaxWriteCapacityUnits' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrrsResponseStatus :: Lens.Lens' DescribeLimitsResponse Core.Int
dlrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dlrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
