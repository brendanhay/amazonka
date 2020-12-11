{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dlrsTableMaxWriteCapacityUnits,
    dlrsTableMaxReadCapacityUnits,
    dlrsAccountMaxWriteCapacityUnits,
    dlrsAccountMaxReadCapacityUnits,
    dlrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @DescribeLimits@ operation. Has no content.
--
-- /See:/ 'mkDescribeLimits' smart constructor.
data DescribeLimits = DescribeLimits'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLimits' with the minimum fields required to make a request.
mkDescribeLimits ::
  DescribeLimits
mkDescribeLimits = DescribeLimits'

instance Lude.AWSRequest DescribeLimits where
  type Rs DescribeLimits = DescribeLimitsResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeLimitsResponse'
            Lude.<$> (x Lude..?> "TableMaxWriteCapacityUnits")
            Lude.<*> (x Lude..?> "TableMaxReadCapacityUnits")
            Lude.<*> (x Lude..?> "AccountMaxWriteCapacityUnits")
            Lude.<*> (x Lude..?> "AccountMaxReadCapacityUnits")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeLimits where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.DescribeLimits" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeLimits where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DescribeLimits where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeLimits where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a @DescribeLimits@ operation.
--
-- /See:/ 'mkDescribeLimitsResponse' smart constructor.
data DescribeLimitsResponse = DescribeLimitsResponse'
  { tableMaxWriteCapacityUnits ::
      Lude.Maybe Lude.Natural,
    tableMaxReadCapacityUnits ::
      Lude.Maybe Lude.Natural,
    accountMaxWriteCapacityUnits ::
      Lude.Maybe Lude.Natural,
    accountMaxReadCapacityUnits ::
      Lude.Maybe Lude.Natural,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLimitsResponse' with the minimum fields required to make a request.
--
-- * 'accountMaxReadCapacityUnits' - The maximum total read capacity units that your account allows you to provision across all of your tables in this Region.
-- * 'accountMaxWriteCapacityUnits' - The maximum total write capacity units that your account allows you to provision across all of your tables in this Region.
-- * 'responseStatus' - The response status code.
-- * 'tableMaxReadCapacityUnits' - The maximum read capacity units that your account allows you to provision for a new table that you are creating in this Region, including the read capacity units provisioned for its global secondary indexes (GSIs).
-- * 'tableMaxWriteCapacityUnits' - The maximum write capacity units that your account allows you to provision for a new table that you are creating in this Region, including the write capacity units provisioned for its global secondary indexes (GSIs).
mkDescribeLimitsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeLimitsResponse
mkDescribeLimitsResponse pResponseStatus_ =
  DescribeLimitsResponse'
    { tableMaxWriteCapacityUnits =
        Lude.Nothing,
      tableMaxReadCapacityUnits = Lude.Nothing,
      accountMaxWriteCapacityUnits = Lude.Nothing,
      accountMaxReadCapacityUnits = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The maximum write capacity units that your account allows you to provision for a new table that you are creating in this Region, including the write capacity units provisioned for its global secondary indexes (GSIs).
--
-- /Note:/ Consider using 'tableMaxWriteCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrsTableMaxWriteCapacityUnits :: Lens.Lens' DescribeLimitsResponse (Lude.Maybe Lude.Natural)
dlrsTableMaxWriteCapacityUnits = Lens.lens (tableMaxWriteCapacityUnits :: DescribeLimitsResponse -> Lude.Maybe Lude.Natural) (\s a -> s {tableMaxWriteCapacityUnits = a} :: DescribeLimitsResponse)
{-# DEPRECATED dlrsTableMaxWriteCapacityUnits "Use generic-lens or generic-optics with 'tableMaxWriteCapacityUnits' instead." #-}

-- | The maximum read capacity units that your account allows you to provision for a new table that you are creating in this Region, including the read capacity units provisioned for its global secondary indexes (GSIs).
--
-- /Note:/ Consider using 'tableMaxReadCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrsTableMaxReadCapacityUnits :: Lens.Lens' DescribeLimitsResponse (Lude.Maybe Lude.Natural)
dlrsTableMaxReadCapacityUnits = Lens.lens (tableMaxReadCapacityUnits :: DescribeLimitsResponse -> Lude.Maybe Lude.Natural) (\s a -> s {tableMaxReadCapacityUnits = a} :: DescribeLimitsResponse)
{-# DEPRECATED dlrsTableMaxReadCapacityUnits "Use generic-lens or generic-optics with 'tableMaxReadCapacityUnits' instead." #-}

-- | The maximum total write capacity units that your account allows you to provision across all of your tables in this Region.
--
-- /Note:/ Consider using 'accountMaxWriteCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrsAccountMaxWriteCapacityUnits :: Lens.Lens' DescribeLimitsResponse (Lude.Maybe Lude.Natural)
dlrsAccountMaxWriteCapacityUnits = Lens.lens (accountMaxWriteCapacityUnits :: DescribeLimitsResponse -> Lude.Maybe Lude.Natural) (\s a -> s {accountMaxWriteCapacityUnits = a} :: DescribeLimitsResponse)
{-# DEPRECATED dlrsAccountMaxWriteCapacityUnits "Use generic-lens or generic-optics with 'accountMaxWriteCapacityUnits' instead." #-}

-- | The maximum total read capacity units that your account allows you to provision across all of your tables in this Region.
--
-- /Note:/ Consider using 'accountMaxReadCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrsAccountMaxReadCapacityUnits :: Lens.Lens' DescribeLimitsResponse (Lude.Maybe Lude.Natural)
dlrsAccountMaxReadCapacityUnits = Lens.lens (accountMaxReadCapacityUnits :: DescribeLimitsResponse -> Lude.Maybe Lude.Natural) (\s a -> s {accountMaxReadCapacityUnits = a} :: DescribeLimitsResponse)
{-# DEPRECATED dlrsAccountMaxReadCapacityUnits "Use generic-lens or generic-optics with 'accountMaxReadCapacityUnits' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrsResponseStatus :: Lens.Lens' DescribeLimitsResponse Lude.Int
dlrsResponseStatus = Lens.lens (responseStatus :: DescribeLimitsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeLimitsResponse)
{-# DEPRECATED dlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
