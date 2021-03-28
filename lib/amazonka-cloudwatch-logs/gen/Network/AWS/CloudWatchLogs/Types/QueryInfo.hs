{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.QueryInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchLogs.Types.QueryInfo
  ( QueryInfo (..)
  -- * Smart constructor
  , mkQueryInfo
  -- * Lenses
  , qiCreateTime
  , qiLogGroupName
  , qiQueryId
  , qiQueryString
  , qiStatus
  ) where

import qualified Network.AWS.CloudWatchLogs.Types.LogGroupName as Types
import qualified Network.AWS.CloudWatchLogs.Types.QueryId as Types
import qualified Network.AWS.CloudWatchLogs.Types.QueryStatus as Types
import qualified Network.AWS.CloudWatchLogs.Types.QueryString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about one CloudWatch Logs Insights query that matches the request in a @DescribeQueries@ operation. 
--
-- /See:/ 'mkQueryInfo' smart constructor.
data QueryInfo = QueryInfo'
  { createTime :: Core.Maybe Core.Natural
    -- ^ The date and time that this query was created.
  , logGroupName :: Core.Maybe Types.LogGroupName
    -- ^ The name of the log group scanned by this query.
  , queryId :: Core.Maybe Types.QueryId
    -- ^ The unique ID number of this query.
  , queryString :: Core.Maybe Types.QueryString
    -- ^ The query string used in this query.
  , status :: Core.Maybe Types.QueryStatus
    -- ^ The status of this query. Possible values are @Cancelled@ , @Complete@ , @Failed@ , @Running@ , @Scheduled@ , and @Unknown@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'QueryInfo' value with any optional fields omitted.
mkQueryInfo
    :: QueryInfo
mkQueryInfo
  = QueryInfo'{createTime = Core.Nothing,
               logGroupName = Core.Nothing, queryId = Core.Nothing,
               queryString = Core.Nothing, status = Core.Nothing}

-- | The date and time that this query was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qiCreateTime :: Lens.Lens' QueryInfo (Core.Maybe Core.Natural)
qiCreateTime = Lens.field @"createTime"
{-# INLINEABLE qiCreateTime #-}
{-# DEPRECATED createTime "Use generic-lens or generic-optics with 'createTime' instead"  #-}

-- | The name of the log group scanned by this query.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qiLogGroupName :: Lens.Lens' QueryInfo (Core.Maybe Types.LogGroupName)
qiLogGroupName = Lens.field @"logGroupName"
{-# INLINEABLE qiLogGroupName #-}
{-# DEPRECATED logGroupName "Use generic-lens or generic-optics with 'logGroupName' instead"  #-}

-- | The unique ID number of this query.
--
-- /Note:/ Consider using 'queryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qiQueryId :: Lens.Lens' QueryInfo (Core.Maybe Types.QueryId)
qiQueryId = Lens.field @"queryId"
{-# INLINEABLE qiQueryId #-}
{-# DEPRECATED queryId "Use generic-lens or generic-optics with 'queryId' instead"  #-}

-- | The query string used in this query.
--
-- /Note:/ Consider using 'queryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qiQueryString :: Lens.Lens' QueryInfo (Core.Maybe Types.QueryString)
qiQueryString = Lens.field @"queryString"
{-# INLINEABLE qiQueryString #-}
{-# DEPRECATED queryString "Use generic-lens or generic-optics with 'queryString' instead"  #-}

-- | The status of this query. Possible values are @Cancelled@ , @Complete@ , @Failed@ , @Running@ , @Scheduled@ , and @Unknown@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qiStatus :: Lens.Lens' QueryInfo (Core.Maybe Types.QueryStatus)
qiStatus = Lens.field @"status"
{-# INLINEABLE qiStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON QueryInfo where
        parseJSON
          = Core.withObject "QueryInfo" Core.$
              \ x ->
                QueryInfo' Core.<$>
                  (x Core..:? "createTime") Core.<*> x Core..:? "logGroupName"
                    Core.<*> x Core..:? "queryId"
                    Core.<*> x Core..:? "queryString"
                    Core.<*> x Core..:? "status"
