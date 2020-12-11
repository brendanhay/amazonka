{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.RegisterDBProxyTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associate one or more @DBProxyTarget@ data structures with a @DBProxyTargetGroup@ .
module Network.AWS.RDS.RegisterDBProxyTargets
  ( -- * Creating a request
    RegisterDBProxyTargets (..),
    mkRegisterDBProxyTargets,

    -- ** Request lenses
    rdptDBClusterIdentifiers,
    rdptDBInstanceIdentifiers,
    rdptTargetGroupName,
    rdptDBProxyName,

    -- * Destructuring the response
    RegisterDBProxyTargetsResponse (..),
    mkRegisterDBProxyTargetsResponse,

    -- ** Response lenses
    rdptrsDBProxyTargets,
    rdptrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRegisterDBProxyTargets' smart constructor.
data RegisterDBProxyTargets = RegisterDBProxyTargets'
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

-- | Creates a value of 'RegisterDBProxyTargets' with the minimum fields required to make a request.
--
-- * 'dbClusterIdentifiers' - One or more DB cluster identifiers.
-- * 'dbInstanceIdentifiers' - One or more DB instance identifiers.
-- * 'dbProxyName' - The identifier of the @DBProxy@ that is associated with the @DBProxyTargetGroup@ .
-- * 'targetGroupName' - The identifier of the @DBProxyTargetGroup@ .
mkRegisterDBProxyTargets ::
  -- | 'dbProxyName'
  Lude.Text ->
  RegisterDBProxyTargets
mkRegisterDBProxyTargets pDBProxyName_ =
  RegisterDBProxyTargets'
    { dbClusterIdentifiers = Lude.Nothing,
      dbInstanceIdentifiers = Lude.Nothing,
      targetGroupName = Lude.Nothing,
      dbProxyName = pDBProxyName_
    }

-- | One or more DB cluster identifiers.
--
-- /Note:/ Consider using 'dbClusterIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdptDBClusterIdentifiers :: Lens.Lens' RegisterDBProxyTargets (Lude.Maybe [Lude.Text])
rdptDBClusterIdentifiers = Lens.lens (dbClusterIdentifiers :: RegisterDBProxyTargets -> Lude.Maybe [Lude.Text]) (\s a -> s {dbClusterIdentifiers = a} :: RegisterDBProxyTargets)
{-# DEPRECATED rdptDBClusterIdentifiers "Use generic-lens or generic-optics with 'dbClusterIdentifiers' instead." #-}

-- | One or more DB instance identifiers.
--
-- /Note:/ Consider using 'dbInstanceIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdptDBInstanceIdentifiers :: Lens.Lens' RegisterDBProxyTargets (Lude.Maybe [Lude.Text])
rdptDBInstanceIdentifiers = Lens.lens (dbInstanceIdentifiers :: RegisterDBProxyTargets -> Lude.Maybe [Lude.Text]) (\s a -> s {dbInstanceIdentifiers = a} :: RegisterDBProxyTargets)
{-# DEPRECATED rdptDBInstanceIdentifiers "Use generic-lens or generic-optics with 'dbInstanceIdentifiers' instead." #-}

-- | The identifier of the @DBProxyTargetGroup@ .
--
-- /Note:/ Consider using 'targetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdptTargetGroupName :: Lens.Lens' RegisterDBProxyTargets (Lude.Maybe Lude.Text)
rdptTargetGroupName = Lens.lens (targetGroupName :: RegisterDBProxyTargets -> Lude.Maybe Lude.Text) (\s a -> s {targetGroupName = a} :: RegisterDBProxyTargets)
{-# DEPRECATED rdptTargetGroupName "Use generic-lens or generic-optics with 'targetGroupName' instead." #-}

-- | The identifier of the @DBProxy@ that is associated with the @DBProxyTargetGroup@ .
--
-- /Note:/ Consider using 'dbProxyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdptDBProxyName :: Lens.Lens' RegisterDBProxyTargets Lude.Text
rdptDBProxyName = Lens.lens (dbProxyName :: RegisterDBProxyTargets -> Lude.Text) (\s a -> s {dbProxyName = a} :: RegisterDBProxyTargets)
{-# DEPRECATED rdptDBProxyName "Use generic-lens or generic-optics with 'dbProxyName' instead." #-}

instance Lude.AWSRequest RegisterDBProxyTargets where
  type Rs RegisterDBProxyTargets = RegisterDBProxyTargetsResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "RegisterDBProxyTargetsResult"
      ( \s h x ->
          RegisterDBProxyTargetsResponse'
            Lude.<$> ( x Lude..@? "DBProxyTargets" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RegisterDBProxyTargets where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RegisterDBProxyTargets where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterDBProxyTargets where
  toQuery RegisterDBProxyTargets' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("RegisterDBProxyTargets" :: Lude.ByteString),
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

-- | /See:/ 'mkRegisterDBProxyTargetsResponse' smart constructor.
data RegisterDBProxyTargetsResponse = RegisterDBProxyTargetsResponse'
  { dbProxyTargets ::
      Lude.Maybe [DBProxyTarget],
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

-- | Creates a value of 'RegisterDBProxyTargetsResponse' with the minimum fields required to make a request.
--
-- * 'dbProxyTargets' - One or more @DBProxyTarget@ objects that are created when you register targets with a target group.
-- * 'responseStatus' - The response status code.
mkRegisterDBProxyTargetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RegisterDBProxyTargetsResponse
mkRegisterDBProxyTargetsResponse pResponseStatus_ =
  RegisterDBProxyTargetsResponse'
    { dbProxyTargets = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | One or more @DBProxyTarget@ objects that are created when you register targets with a target group.
--
-- /Note:/ Consider using 'dbProxyTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdptrsDBProxyTargets :: Lens.Lens' RegisterDBProxyTargetsResponse (Lude.Maybe [DBProxyTarget])
rdptrsDBProxyTargets = Lens.lens (dbProxyTargets :: RegisterDBProxyTargetsResponse -> Lude.Maybe [DBProxyTarget]) (\s a -> s {dbProxyTargets = a} :: RegisterDBProxyTargetsResponse)
{-# DEPRECATED rdptrsDBProxyTargets "Use generic-lens or generic-optics with 'dbProxyTargets' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdptrsResponseStatus :: Lens.Lens' RegisterDBProxyTargetsResponse Lude.Int
rdptrsResponseStatus = Lens.lens (responseStatus :: RegisterDBProxyTargetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterDBProxyTargetsResponse)
{-# DEPRECATED rdptrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
