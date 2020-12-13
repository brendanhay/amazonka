{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ModifyDBProxyTargetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the properties of a @DBProxyTargetGroup@ .
module Network.AWS.RDS.ModifyDBProxyTargetGroup
  ( -- * Creating a request
    ModifyDBProxyTargetGroup (..),
    mkModifyDBProxyTargetGroup,

    -- ** Request lenses
    mdptgConnectionPoolConfig,
    mdptgNewName,
    mdptgDBProxyName,
    mdptgTargetGroupName,

    -- * Destructuring the response
    ModifyDBProxyTargetGroupResponse (..),
    mkModifyDBProxyTargetGroupResponse,

    -- ** Response lenses
    mdptgrsDBProxyTargetGroup,
    mdptgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyDBProxyTargetGroup' smart constructor.
data ModifyDBProxyTargetGroup = ModifyDBProxyTargetGroup'
  { -- | The settings that determine the size and behavior of the connection pool for the target group.
    connectionPoolConfig :: Lude.Maybe ConnectionPoolConfiguration,
    -- | The new name for the modified @DBProxyTarget@ . An identifier must begin with a letter and must contain only ASCII letters, digits, and hyphens; it can't end with a hyphen or contain two consecutive hyphens.
    newName :: Lude.Maybe Lude.Text,
    -- | The name of the new proxy to which to assign the target group.
    dbProxyName :: Lude.Text,
    -- | The name of the new target group to assign to the proxy.
    targetGroupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyDBProxyTargetGroup' with the minimum fields required to make a request.
--
-- * 'connectionPoolConfig' - The settings that determine the size and behavior of the connection pool for the target group.
-- * 'newName' - The new name for the modified @DBProxyTarget@ . An identifier must begin with a letter and must contain only ASCII letters, digits, and hyphens; it can't end with a hyphen or contain two consecutive hyphens.
-- * 'dbProxyName' - The name of the new proxy to which to assign the target group.
-- * 'targetGroupName' - The name of the new target group to assign to the proxy.
mkModifyDBProxyTargetGroup ::
  -- | 'dbProxyName'
  Lude.Text ->
  -- | 'targetGroupName'
  Lude.Text ->
  ModifyDBProxyTargetGroup
mkModifyDBProxyTargetGroup pDBProxyName_ pTargetGroupName_ =
  ModifyDBProxyTargetGroup'
    { connectionPoolConfig = Lude.Nothing,
      newName = Lude.Nothing,
      dbProxyName = pDBProxyName_,
      targetGroupName = pTargetGroupName_
    }

-- | The settings that determine the size and behavior of the connection pool for the target group.
--
-- /Note:/ Consider using 'connectionPoolConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdptgConnectionPoolConfig :: Lens.Lens' ModifyDBProxyTargetGroup (Lude.Maybe ConnectionPoolConfiguration)
mdptgConnectionPoolConfig = Lens.lens (connectionPoolConfig :: ModifyDBProxyTargetGroup -> Lude.Maybe ConnectionPoolConfiguration) (\s a -> s {connectionPoolConfig = a} :: ModifyDBProxyTargetGroup)
{-# DEPRECATED mdptgConnectionPoolConfig "Use generic-lens or generic-optics with 'connectionPoolConfig' instead." #-}

-- | The new name for the modified @DBProxyTarget@ . An identifier must begin with a letter and must contain only ASCII letters, digits, and hyphens; it can't end with a hyphen or contain two consecutive hyphens.
--
-- /Note:/ Consider using 'newName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdptgNewName :: Lens.Lens' ModifyDBProxyTargetGroup (Lude.Maybe Lude.Text)
mdptgNewName = Lens.lens (newName :: ModifyDBProxyTargetGroup -> Lude.Maybe Lude.Text) (\s a -> s {newName = a} :: ModifyDBProxyTargetGroup)
{-# DEPRECATED mdptgNewName "Use generic-lens or generic-optics with 'newName' instead." #-}

-- | The name of the new proxy to which to assign the target group.
--
-- /Note:/ Consider using 'dbProxyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdptgDBProxyName :: Lens.Lens' ModifyDBProxyTargetGroup Lude.Text
mdptgDBProxyName = Lens.lens (dbProxyName :: ModifyDBProxyTargetGroup -> Lude.Text) (\s a -> s {dbProxyName = a} :: ModifyDBProxyTargetGroup)
{-# DEPRECATED mdptgDBProxyName "Use generic-lens or generic-optics with 'dbProxyName' instead." #-}

-- | The name of the new target group to assign to the proxy.
--
-- /Note:/ Consider using 'targetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdptgTargetGroupName :: Lens.Lens' ModifyDBProxyTargetGroup Lude.Text
mdptgTargetGroupName = Lens.lens (targetGroupName :: ModifyDBProxyTargetGroup -> Lude.Text) (\s a -> s {targetGroupName = a} :: ModifyDBProxyTargetGroup)
{-# DEPRECATED mdptgTargetGroupName "Use generic-lens or generic-optics with 'targetGroupName' instead." #-}

instance Lude.AWSRequest ModifyDBProxyTargetGroup where
  type Rs ModifyDBProxyTargetGroup = ModifyDBProxyTargetGroupResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "ModifyDBProxyTargetGroupResult"
      ( \s h x ->
          ModifyDBProxyTargetGroupResponse'
            Lude.<$> (x Lude..@? "DBProxyTargetGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyDBProxyTargetGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyDBProxyTargetGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyDBProxyTargetGroup where
  toQuery ModifyDBProxyTargetGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyDBProxyTargetGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "ConnectionPoolConfig" Lude.=: connectionPoolConfig,
        "NewName" Lude.=: newName,
        "DBProxyName" Lude.=: dbProxyName,
        "TargetGroupName" Lude.=: targetGroupName
      ]

-- | /See:/ 'mkModifyDBProxyTargetGroupResponse' smart constructor.
data ModifyDBProxyTargetGroupResponse = ModifyDBProxyTargetGroupResponse'
  { -- | The settings of the modified @DBProxyTarget@ .
    dbProxyTargetGroup :: Lude.Maybe DBProxyTargetGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyDBProxyTargetGroupResponse' with the minimum fields required to make a request.
--
-- * 'dbProxyTargetGroup' - The settings of the modified @DBProxyTarget@ .
-- * 'responseStatus' - The response status code.
mkModifyDBProxyTargetGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyDBProxyTargetGroupResponse
mkModifyDBProxyTargetGroupResponse pResponseStatus_ =
  ModifyDBProxyTargetGroupResponse'
    { dbProxyTargetGroup =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The settings of the modified @DBProxyTarget@ .
--
-- /Note:/ Consider using 'dbProxyTargetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdptgrsDBProxyTargetGroup :: Lens.Lens' ModifyDBProxyTargetGroupResponse (Lude.Maybe DBProxyTargetGroup)
mdptgrsDBProxyTargetGroup = Lens.lens (dbProxyTargetGroup :: ModifyDBProxyTargetGroupResponse -> Lude.Maybe DBProxyTargetGroup) (\s a -> s {dbProxyTargetGroup = a} :: ModifyDBProxyTargetGroupResponse)
{-# DEPRECATED mdptgrsDBProxyTargetGroup "Use generic-lens or generic-optics with 'dbProxyTargetGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdptgrsResponseStatus :: Lens.Lens' ModifyDBProxyTargetGroupResponse Lude.Int
mdptgrsResponseStatus = Lens.lens (responseStatus :: ModifyDBProxyTargetGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyDBProxyTargetGroupResponse)
{-# DEPRECATED mdptgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
