{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.QueryInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.QueryInfo
  ( QueryInfo (..),

    -- * Smart constructor
    mkQueryInfo,

    -- * Lenses
    qiStatus,
    qiQueryId,
    qiLogGroupName,
    qiQueryString,
    qiCreateTime,
  )
where

import Network.AWS.CloudWatchLogs.Types.QueryStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about one CloudWatch Logs Insights query that matches the request in a @DescribeQueries@ operation.
--
-- /See:/ 'mkQueryInfo' smart constructor.
data QueryInfo = QueryInfo'
  { -- | The status of this query. Possible values are @Cancelled@ , @Complete@ , @Failed@ , @Running@ , @Scheduled@ , and @Unknown@ .
    status :: Lude.Maybe QueryStatus,
    -- | The unique ID number of this query.
    queryId :: Lude.Maybe Lude.Text,
    -- | The name of the log group scanned by this query.
    logGroupName :: Lude.Maybe Lude.Text,
    -- | The query string used in this query.
    queryString :: Lude.Maybe Lude.Text,
    -- | The date and time that this query was created.
    createTime :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'QueryInfo' with the minimum fields required to make a request.
--
-- * 'status' - The status of this query. Possible values are @Cancelled@ , @Complete@ , @Failed@ , @Running@ , @Scheduled@ , and @Unknown@ .
-- * 'queryId' - The unique ID number of this query.
-- * 'logGroupName' - The name of the log group scanned by this query.
-- * 'queryString' - The query string used in this query.
-- * 'createTime' - The date and time that this query was created.
mkQueryInfo ::
  QueryInfo
mkQueryInfo =
  QueryInfo'
    { status = Lude.Nothing,
      queryId = Lude.Nothing,
      logGroupName = Lude.Nothing,
      queryString = Lude.Nothing,
      createTime = Lude.Nothing
    }

-- | The status of this query. Possible values are @Cancelled@ , @Complete@ , @Failed@ , @Running@ , @Scheduled@ , and @Unknown@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qiStatus :: Lens.Lens' QueryInfo (Lude.Maybe QueryStatus)
qiStatus = Lens.lens (status :: QueryInfo -> Lude.Maybe QueryStatus) (\s a -> s {status = a} :: QueryInfo)
{-# DEPRECATED qiStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The unique ID number of this query.
--
-- /Note:/ Consider using 'queryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qiQueryId :: Lens.Lens' QueryInfo (Lude.Maybe Lude.Text)
qiQueryId = Lens.lens (queryId :: QueryInfo -> Lude.Maybe Lude.Text) (\s a -> s {queryId = a} :: QueryInfo)
{-# DEPRECATED qiQueryId "Use generic-lens or generic-optics with 'queryId' instead." #-}

-- | The name of the log group scanned by this query.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qiLogGroupName :: Lens.Lens' QueryInfo (Lude.Maybe Lude.Text)
qiLogGroupName = Lens.lens (logGroupName :: QueryInfo -> Lude.Maybe Lude.Text) (\s a -> s {logGroupName = a} :: QueryInfo)
{-# DEPRECATED qiLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The query string used in this query.
--
-- /Note:/ Consider using 'queryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qiQueryString :: Lens.Lens' QueryInfo (Lude.Maybe Lude.Text)
qiQueryString = Lens.lens (queryString :: QueryInfo -> Lude.Maybe Lude.Text) (\s a -> s {queryString = a} :: QueryInfo)
{-# DEPRECATED qiQueryString "Use generic-lens or generic-optics with 'queryString' instead." #-}

-- | The date and time that this query was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qiCreateTime :: Lens.Lens' QueryInfo (Lude.Maybe Lude.Natural)
qiCreateTime = Lens.lens (createTime :: QueryInfo -> Lude.Maybe Lude.Natural) (\s a -> s {createTime = a} :: QueryInfo)
{-# DEPRECATED qiCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

instance Lude.FromJSON QueryInfo where
  parseJSON =
    Lude.withObject
      "QueryInfo"
      ( \x ->
          QueryInfo'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "queryId")
            Lude.<*> (x Lude..:? "logGroupName")
            Lude.<*> (x Lude..:? "queryString")
            Lude.<*> (x Lude..:? "createTime")
      )
