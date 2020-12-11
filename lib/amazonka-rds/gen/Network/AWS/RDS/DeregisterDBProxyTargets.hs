{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeregisterDBProxyTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove the association between one or more @DBProxyTarget@ data structures and a @DBProxyTargetGroup@ .
module Network.AWS.RDS.DeregisterDBProxyTargets
  ( -- * Creating a request
    DeregisterDBProxyTargets (..),
    mkDeregisterDBProxyTargets,

    -- ** Request lenses
    ddbptDBClusterIdentifiers,
    ddbptDBInstanceIdentifiers,
    ddbptTargetGroupName,
    ddbptDBProxyName,

    -- * Destructuring the response
    DeregisterDBProxyTargetsResponse (..),
    mkDeregisterDBProxyTargetsResponse,

    -- ** Response lenses
    ddptrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeregisterDBProxyTargets' smart constructor.
data DeregisterDBProxyTargets = DeregisterDBProxyTargets'
  { dbClusterIdentifiers ::
      Lude.Maybe [Lude.Text],
    dbInstanceIdentifiers ::
      Lude.Maybe [Lude.Text],
    targetGroupName :: Lude.Maybe Lude.Text,
    dbProxyName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterDBProxyTargets' with the minimum fields required to make a request.
--
-- * 'dbClusterIdentifiers' - One or more DB cluster identifiers.
-- * 'dbInstanceIdentifiers' - One or more DB instance identifiers.
-- * 'dbProxyName' - The identifier of the @DBProxy@ that is associated with the @DBProxyTargetGroup@ .
-- * 'targetGroupName' - The identifier of the @DBProxyTargetGroup@ .
mkDeregisterDBProxyTargets ::
  -- | 'dbProxyName'
  Lude.Text ->
  DeregisterDBProxyTargets
mkDeregisterDBProxyTargets pDBProxyName_ =
  DeregisterDBProxyTargets'
    { dbClusterIdentifiers = Lude.Nothing,
      dbInstanceIdentifiers = Lude.Nothing,
      targetGroupName = Lude.Nothing,
      dbProxyName = pDBProxyName_
    }

-- | One or more DB cluster identifiers.
--
-- /Note:/ Consider using 'dbClusterIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptDBClusterIdentifiers :: Lens.Lens' DeregisterDBProxyTargets (Lude.Maybe [Lude.Text])
ddbptDBClusterIdentifiers = Lens.lens (dbClusterIdentifiers :: DeregisterDBProxyTargets -> Lude.Maybe [Lude.Text]) (\s a -> s {dbClusterIdentifiers = a} :: DeregisterDBProxyTargets)
{-# DEPRECATED ddbptDBClusterIdentifiers "Use generic-lens or generic-optics with 'dbClusterIdentifiers' instead." #-}

-- | One or more DB instance identifiers.
--
-- /Note:/ Consider using 'dbInstanceIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptDBInstanceIdentifiers :: Lens.Lens' DeregisterDBProxyTargets (Lude.Maybe [Lude.Text])
ddbptDBInstanceIdentifiers = Lens.lens (dbInstanceIdentifiers :: DeregisterDBProxyTargets -> Lude.Maybe [Lude.Text]) (\s a -> s {dbInstanceIdentifiers = a} :: DeregisterDBProxyTargets)
{-# DEPRECATED ddbptDBInstanceIdentifiers "Use generic-lens or generic-optics with 'dbInstanceIdentifiers' instead." #-}

-- | The identifier of the @DBProxyTargetGroup@ .
--
-- /Note:/ Consider using 'targetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptTargetGroupName :: Lens.Lens' DeregisterDBProxyTargets (Lude.Maybe Lude.Text)
ddbptTargetGroupName = Lens.lens (targetGroupName :: DeregisterDBProxyTargets -> Lude.Maybe Lude.Text) (\s a -> s {targetGroupName = a} :: DeregisterDBProxyTargets)
{-# DEPRECATED ddbptTargetGroupName "Use generic-lens or generic-optics with 'targetGroupName' instead." #-}

-- | The identifier of the @DBProxy@ that is associated with the @DBProxyTargetGroup@ .
--
-- /Note:/ Consider using 'dbProxyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbptDBProxyName :: Lens.Lens' DeregisterDBProxyTargets Lude.Text
ddbptDBProxyName = Lens.lens (dbProxyName :: DeregisterDBProxyTargets -> Lude.Text) (\s a -> s {dbProxyName = a} :: DeregisterDBProxyTargets)
{-# DEPRECATED ddbptDBProxyName "Use generic-lens or generic-optics with 'dbProxyName' instead." #-}

instance Lude.AWSRequest DeregisterDBProxyTargets where
  type Rs DeregisterDBProxyTargets = DeregisterDBProxyTargetsResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DeregisterDBProxyTargetsResult"
      ( \s h x ->
          DeregisterDBProxyTargetsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeregisterDBProxyTargets where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeregisterDBProxyTargets where
  toPath = Lude.const "/"

instance Lude.ToQuery DeregisterDBProxyTargets where
  toQuery DeregisterDBProxyTargets' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeregisterDBProxyTargets" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBClusterIdentifiers"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> dbClusterIdentifiers),
        "DBInstanceIdentifiers"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> dbInstanceIdentifiers),
        "TargetGroupName" Lude.=: targetGroupName,
        "DBProxyName" Lude.=: dbProxyName
      ]

-- | /See:/ 'mkDeregisterDBProxyTargetsResponse' smart constructor.
newtype DeregisterDBProxyTargetsResponse = DeregisterDBProxyTargetsResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterDBProxyTargetsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeregisterDBProxyTargetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeregisterDBProxyTargetsResponse
mkDeregisterDBProxyTargetsResponse pResponseStatus_ =
  DeregisterDBProxyTargetsResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddptrsResponseStatus :: Lens.Lens' DeregisterDBProxyTargetsResponse Lude.Int
ddptrsResponseStatus = Lens.lens (responseStatus :: DeregisterDBProxyTargetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeregisterDBProxyTargetsResponse)
{-# DEPRECATED ddptrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
