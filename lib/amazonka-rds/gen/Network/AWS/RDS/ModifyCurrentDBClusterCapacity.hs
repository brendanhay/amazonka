{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ModifyCurrentDBClusterCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Set the capacity of an Aurora Serverless DB cluster to a specific value.
--
-- Aurora Serverless scales seamlessly based on the workload on the DB cluster. In some cases, the capacity might not scale fast enough to meet a sudden change in workload, such as a large number of new transactions. Call @ModifyCurrentDBClusterCapacity@ to set the capacity explicitly.
-- After this call sets the DB cluster capacity, Aurora Serverless can automatically scale the DB cluster based on the cooldown period for scaling up and the cooldown period for scaling down.
-- For more information about Aurora Serverless, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html Using Amazon Aurora Serverless> in the /Amazon Aurora User Guide/ .
-- /Important:/ If you call @ModifyCurrentDBClusterCapacity@ with the default @TimeoutAction@ , connections that prevent Aurora Serverless from finding a scaling point might be dropped. For more information about scaling points, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.how-it-works.html#aurora-serverless.how-it-works.auto-scaling Autoscaling for Aurora Serverless> in the /Amazon Aurora User Guide/ .
module Network.AWS.RDS.ModifyCurrentDBClusterCapacity
  ( -- * Creating a request
    ModifyCurrentDBClusterCapacity (..),
    mkModifyCurrentDBClusterCapacity,

    -- ** Request lenses
    mcdccTimeoutAction,
    mcdccCapacity,
    mcdccSecondsBeforeTimeout,
    mcdccDBClusterIdentifier,

    -- * Destructuring the response
    ModifyCurrentDBClusterCapacityResponse (..),
    mkModifyCurrentDBClusterCapacityResponse,

    -- ** Response lenses
    mcdccrsDBClusterIdentifier,
    mcdccrsTimeoutAction,
    mcdccrsCurrentCapacity,
    mcdccrsPendingCapacity,
    mcdccrsSecondsBeforeTimeout,
    mcdccrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyCurrentDBClusterCapacity' smart constructor.
data ModifyCurrentDBClusterCapacity = ModifyCurrentDBClusterCapacity'
  { timeoutAction ::
      Lude.Maybe Lude.Text,
    capacity ::
      Lude.Maybe Lude.Int,
    secondsBeforeTimeout ::
      Lude.Maybe Lude.Int,
    dbClusterIdentifier ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyCurrentDBClusterCapacity' with the minimum fields required to make a request.
--
-- * 'capacity' - The DB cluster capacity.
--
-- When you change the capacity of a paused Aurora Serverless DB cluster, it automatically resumes.
-- Constraints:
--
--     * For Aurora MySQL, valid capacity values are @1@ , @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @128@ , and @256@ .
--
--
--     * For Aurora PostgreSQL, valid capacity values are @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @192@ , and @384@ .
--
--
-- * 'dbClusterIdentifier' - The DB cluster identifier for the cluster being modified. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must match the identifier of an existing DB cluster.
--
--
-- * 'secondsBeforeTimeout' - The amount of time, in seconds, that Aurora Serverless tries to find a scaling point to perform seamless scaling before enforcing the timeout action. The default is 300.
--
--
--     * Value must be from 10 through 600.
--
--
-- * 'timeoutAction' - The action to take when the timeout is reached, either @ForceApplyCapacityChange@ or @RollbackCapacityChange@ .
--
-- @ForceApplyCapacityChange@ , the default, sets the capacity to the specified value as soon as possible.
-- @RollbackCapacityChange@ ignores the capacity change if a scaling point isn't found in the timeout period.
mkModifyCurrentDBClusterCapacity ::
  -- | 'dbClusterIdentifier'
  Lude.Text ->
  ModifyCurrentDBClusterCapacity
mkModifyCurrentDBClusterCapacity pDBClusterIdentifier_ =
  ModifyCurrentDBClusterCapacity'
    { timeoutAction = Lude.Nothing,
      capacity = Lude.Nothing,
      secondsBeforeTimeout = Lude.Nothing,
      dbClusterIdentifier = pDBClusterIdentifier_
    }

-- | The action to take when the timeout is reached, either @ForceApplyCapacityChange@ or @RollbackCapacityChange@ .
--
-- @ForceApplyCapacityChange@ , the default, sets the capacity to the specified value as soon as possible.
-- @RollbackCapacityChange@ ignores the capacity change if a scaling point isn't found in the timeout period.
--
-- /Note:/ Consider using 'timeoutAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdccTimeoutAction :: Lens.Lens' ModifyCurrentDBClusterCapacity (Lude.Maybe Lude.Text)
mcdccTimeoutAction = Lens.lens (timeoutAction :: ModifyCurrentDBClusterCapacity -> Lude.Maybe Lude.Text) (\s a -> s {timeoutAction = a} :: ModifyCurrentDBClusterCapacity)
{-# DEPRECATED mcdccTimeoutAction "Use generic-lens or generic-optics with 'timeoutAction' instead." #-}

-- | The DB cluster capacity.
--
-- When you change the capacity of a paused Aurora Serverless DB cluster, it automatically resumes.
-- Constraints:
--
--     * For Aurora MySQL, valid capacity values are @1@ , @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @128@ , and @256@ .
--
--
--     * For Aurora PostgreSQL, valid capacity values are @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @192@ , and @384@ .
--
--
--
-- /Note:/ Consider using 'capacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdccCapacity :: Lens.Lens' ModifyCurrentDBClusterCapacity (Lude.Maybe Lude.Int)
mcdccCapacity = Lens.lens (capacity :: ModifyCurrentDBClusterCapacity -> Lude.Maybe Lude.Int) (\s a -> s {capacity = a} :: ModifyCurrentDBClusterCapacity)
{-# DEPRECATED mcdccCapacity "Use generic-lens or generic-optics with 'capacity' instead." #-}

-- | The amount of time, in seconds, that Aurora Serverless tries to find a scaling point to perform seamless scaling before enforcing the timeout action. The default is 300.
--
--
--     * Value must be from 10 through 600.
--
--
--
-- /Note:/ Consider using 'secondsBeforeTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdccSecondsBeforeTimeout :: Lens.Lens' ModifyCurrentDBClusterCapacity (Lude.Maybe Lude.Int)
mcdccSecondsBeforeTimeout = Lens.lens (secondsBeforeTimeout :: ModifyCurrentDBClusterCapacity -> Lude.Maybe Lude.Int) (\s a -> s {secondsBeforeTimeout = a} :: ModifyCurrentDBClusterCapacity)
{-# DEPRECATED mcdccSecondsBeforeTimeout "Use generic-lens or generic-optics with 'secondsBeforeTimeout' instead." #-}

-- | The DB cluster identifier for the cluster being modified. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must match the identifier of an existing DB cluster.
--
--
--
-- /Note:/ Consider using 'dbClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdccDBClusterIdentifier :: Lens.Lens' ModifyCurrentDBClusterCapacity Lude.Text
mcdccDBClusterIdentifier = Lens.lens (dbClusterIdentifier :: ModifyCurrentDBClusterCapacity -> Lude.Text) (\s a -> s {dbClusterIdentifier = a} :: ModifyCurrentDBClusterCapacity)
{-# DEPRECATED mcdccDBClusterIdentifier "Use generic-lens or generic-optics with 'dbClusterIdentifier' instead." #-}

instance Lude.AWSRequest ModifyCurrentDBClusterCapacity where
  type
    Rs ModifyCurrentDBClusterCapacity =
      ModifyCurrentDBClusterCapacityResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "ModifyCurrentDBClusterCapacityResult"
      ( \s h x ->
          ModifyCurrentDBClusterCapacityResponse'
            Lude.<$> (x Lude..@? "DBClusterIdentifier")
            Lude.<*> (x Lude..@? "TimeoutAction")
            Lude.<*> (x Lude..@? "CurrentCapacity")
            Lude.<*> (x Lude..@? "PendingCapacity")
            Lude.<*> (x Lude..@? "SecondsBeforeTimeout")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyCurrentDBClusterCapacity where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyCurrentDBClusterCapacity where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyCurrentDBClusterCapacity where
  toQuery ModifyCurrentDBClusterCapacity' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ModifyCurrentDBClusterCapacity" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "TimeoutAction" Lude.=: timeoutAction,
        "Capacity" Lude.=: capacity,
        "SecondsBeforeTimeout" Lude.=: secondsBeforeTimeout,
        "DBClusterIdentifier" Lude.=: dbClusterIdentifier
      ]

-- | /See:/ 'mkModifyCurrentDBClusterCapacityResponse' smart constructor.
data ModifyCurrentDBClusterCapacityResponse = ModifyCurrentDBClusterCapacityResponse'
  { dbClusterIdentifier ::
      Lude.Maybe
        Lude.Text,
    timeoutAction ::
      Lude.Maybe
        Lude.Text,
    currentCapacity ::
      Lude.Maybe
        Lude.Int,
    pendingCapacity ::
      Lude.Maybe
        Lude.Int,
    secondsBeforeTimeout ::
      Lude.Maybe
        Lude.Int,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyCurrentDBClusterCapacityResponse' with the minimum fields required to make a request.
--
-- * 'currentCapacity' - The current capacity of the DB cluster.
-- * 'dbClusterIdentifier' - A user-supplied DB cluster identifier. This identifier is the unique key that identifies a DB cluster.
-- * 'pendingCapacity' - A value that specifies the capacity that the DB cluster scales to next.
-- * 'responseStatus' - The response status code.
-- * 'secondsBeforeTimeout' - The number of seconds before a call to @ModifyCurrentDBClusterCapacity@ times out.
-- * 'timeoutAction' - The timeout action of a call to @ModifyCurrentDBClusterCapacity@ , either @ForceApplyCapacityChange@ or @RollbackCapacityChange@ .
mkModifyCurrentDBClusterCapacityResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyCurrentDBClusterCapacityResponse
mkModifyCurrentDBClusterCapacityResponse pResponseStatus_ =
  ModifyCurrentDBClusterCapacityResponse'
    { dbClusterIdentifier =
        Lude.Nothing,
      timeoutAction = Lude.Nothing,
      currentCapacity = Lude.Nothing,
      pendingCapacity = Lude.Nothing,
      secondsBeforeTimeout = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A user-supplied DB cluster identifier. This identifier is the unique key that identifies a DB cluster.
--
-- /Note:/ Consider using 'dbClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdccrsDBClusterIdentifier :: Lens.Lens' ModifyCurrentDBClusterCapacityResponse (Lude.Maybe Lude.Text)
mcdccrsDBClusterIdentifier = Lens.lens (dbClusterIdentifier :: ModifyCurrentDBClusterCapacityResponse -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterIdentifier = a} :: ModifyCurrentDBClusterCapacityResponse)
{-# DEPRECATED mcdccrsDBClusterIdentifier "Use generic-lens or generic-optics with 'dbClusterIdentifier' instead." #-}

-- | The timeout action of a call to @ModifyCurrentDBClusterCapacity@ , either @ForceApplyCapacityChange@ or @RollbackCapacityChange@ .
--
-- /Note:/ Consider using 'timeoutAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdccrsTimeoutAction :: Lens.Lens' ModifyCurrentDBClusterCapacityResponse (Lude.Maybe Lude.Text)
mcdccrsTimeoutAction = Lens.lens (timeoutAction :: ModifyCurrentDBClusterCapacityResponse -> Lude.Maybe Lude.Text) (\s a -> s {timeoutAction = a} :: ModifyCurrentDBClusterCapacityResponse)
{-# DEPRECATED mcdccrsTimeoutAction "Use generic-lens or generic-optics with 'timeoutAction' instead." #-}

-- | The current capacity of the DB cluster.
--
-- /Note:/ Consider using 'currentCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdccrsCurrentCapacity :: Lens.Lens' ModifyCurrentDBClusterCapacityResponse (Lude.Maybe Lude.Int)
mcdccrsCurrentCapacity = Lens.lens (currentCapacity :: ModifyCurrentDBClusterCapacityResponse -> Lude.Maybe Lude.Int) (\s a -> s {currentCapacity = a} :: ModifyCurrentDBClusterCapacityResponse)
{-# DEPRECATED mcdccrsCurrentCapacity "Use generic-lens or generic-optics with 'currentCapacity' instead." #-}

-- | A value that specifies the capacity that the DB cluster scales to next.
--
-- /Note:/ Consider using 'pendingCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdccrsPendingCapacity :: Lens.Lens' ModifyCurrentDBClusterCapacityResponse (Lude.Maybe Lude.Int)
mcdccrsPendingCapacity = Lens.lens (pendingCapacity :: ModifyCurrentDBClusterCapacityResponse -> Lude.Maybe Lude.Int) (\s a -> s {pendingCapacity = a} :: ModifyCurrentDBClusterCapacityResponse)
{-# DEPRECATED mcdccrsPendingCapacity "Use generic-lens or generic-optics with 'pendingCapacity' instead." #-}

-- | The number of seconds before a call to @ModifyCurrentDBClusterCapacity@ times out.
--
-- /Note:/ Consider using 'secondsBeforeTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdccrsSecondsBeforeTimeout :: Lens.Lens' ModifyCurrentDBClusterCapacityResponse (Lude.Maybe Lude.Int)
mcdccrsSecondsBeforeTimeout = Lens.lens (secondsBeforeTimeout :: ModifyCurrentDBClusterCapacityResponse -> Lude.Maybe Lude.Int) (\s a -> s {secondsBeforeTimeout = a} :: ModifyCurrentDBClusterCapacityResponse)
{-# DEPRECATED mcdccrsSecondsBeforeTimeout "Use generic-lens or generic-optics with 'secondsBeforeTimeout' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcdccrsResponseStatus :: Lens.Lens' ModifyCurrentDBClusterCapacityResponse Lude.Int
mcdccrsResponseStatus = Lens.lens (responseStatus :: ModifyCurrentDBClusterCapacityResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyCurrentDBClusterCapacityResponse)
{-# DEPRECATED mcdccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
