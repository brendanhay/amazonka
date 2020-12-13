{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteDBProxy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing proxy.
module Network.AWS.RDS.DeleteDBProxy
  ( -- * Creating a request
    DeleteDBProxy (..),
    mkDeleteDBProxy,

    -- ** Request lenses
    ddpDBProxyName,

    -- * Destructuring the response
    DeleteDBProxyResponse (..),
    mkDeleteDBProxyResponse,

    -- ** Response lenses
    ddprsDBProxy,
    ddprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteDBProxy' smart constructor.
newtype DeleteDBProxy = DeleteDBProxy'
  { -- | The name of the DB proxy to delete.
    dbProxyName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDBProxy' with the minimum fields required to make a request.
--
-- * 'dbProxyName' - The name of the DB proxy to delete.
mkDeleteDBProxy ::
  -- | 'dbProxyName'
  Lude.Text ->
  DeleteDBProxy
mkDeleteDBProxy pDBProxyName_ =
  DeleteDBProxy' {dbProxyName = pDBProxyName_}

-- | The name of the DB proxy to delete.
--
-- /Note:/ Consider using 'dbProxyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddpDBProxyName :: Lens.Lens' DeleteDBProxy Lude.Text
ddpDBProxyName = Lens.lens (dbProxyName :: DeleteDBProxy -> Lude.Text) (\s a -> s {dbProxyName = a} :: DeleteDBProxy)
{-# DEPRECATED ddpDBProxyName "Use generic-lens or generic-optics with 'dbProxyName' instead." #-}

instance Lude.AWSRequest DeleteDBProxy where
  type Rs DeleteDBProxy = DeleteDBProxyResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DeleteDBProxyResult"
      ( \s h x ->
          DeleteDBProxyResponse'
            Lude.<$> (x Lude..@? "DBProxy") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDBProxy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteDBProxy where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDBProxy where
  toQuery DeleteDBProxy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteDBProxy" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBProxyName" Lude.=: dbProxyName
      ]

-- | /See:/ 'mkDeleteDBProxyResponse' smart constructor.
data DeleteDBProxyResponse = DeleteDBProxyResponse'
  { -- | The data structure representing the details of the DB proxy that you delete.
    dbProxy :: Lude.Maybe DBProxy,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDBProxyResponse' with the minimum fields required to make a request.
--
-- * 'dbProxy' - The data structure representing the details of the DB proxy that you delete.
-- * 'responseStatus' - The response status code.
mkDeleteDBProxyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDBProxyResponse
mkDeleteDBProxyResponse pResponseStatus_ =
  DeleteDBProxyResponse'
    { dbProxy = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The data structure representing the details of the DB proxy that you delete.
--
-- /Note:/ Consider using 'dbProxy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddprsDBProxy :: Lens.Lens' DeleteDBProxyResponse (Lude.Maybe DBProxy)
ddprsDBProxy = Lens.lens (dbProxy :: DeleteDBProxyResponse -> Lude.Maybe DBProxy) (\s a -> s {dbProxy = a} :: DeleteDBProxyResponse)
{-# DEPRECATED ddprsDBProxy "Use generic-lens or generic-optics with 'dbProxy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddprsResponseStatus :: Lens.Lens' DeleteDBProxyResponse Lude.Int
ddprsResponseStatus = Lens.lens (responseStatus :: DeleteDBProxyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDBProxyResponse)
{-# DEPRECATED ddprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
