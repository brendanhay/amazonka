{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.RemovePermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes the permission of another AWS account to be able to put events to the specified event bus. Specify the account to revoke by the @StatementId@ value that you associated with the account when you granted it permission with @PutPermission@ . You can find the @StatementId@ by using 'DescribeEventBus' .
module Network.AWS.CloudWatchEvents.RemovePermission
  ( -- * Creating a request
    RemovePermission (..),
    mkRemovePermission,

    -- ** Request lenses
    rpEventBusName,
    rpRemoveAllPermissions,
    rpStatementId,

    -- * Destructuring the response
    RemovePermissionResponse (..),
    mkRemovePermissionResponse,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRemovePermission' smart constructor.
data RemovePermission = RemovePermission'
  { -- | The name of the event bus to revoke permissions for. If you omit this, the default event bus is used.
    eventBusName :: Lude.Maybe Lude.Text,
    -- | Specifies whether to remove all permissions.
    removeAllPermissions :: Lude.Maybe Lude.Bool,
    -- | The statement ID corresponding to the account that is no longer allowed to put events to the default event bus.
    statementId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemovePermission' with the minimum fields required to make a request.
--
-- * 'eventBusName' - The name of the event bus to revoke permissions for. If you omit this, the default event bus is used.
-- * 'removeAllPermissions' - Specifies whether to remove all permissions.
-- * 'statementId' - The statement ID corresponding to the account that is no longer allowed to put events to the default event bus.
mkRemovePermission ::
  RemovePermission
mkRemovePermission =
  RemovePermission'
    { eventBusName = Lude.Nothing,
      removeAllPermissions = Lude.Nothing,
      statementId = Lude.Nothing
    }

-- | The name of the event bus to revoke permissions for. If you omit this, the default event bus is used.
--
-- /Note:/ Consider using 'eventBusName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpEventBusName :: Lens.Lens' RemovePermission (Lude.Maybe Lude.Text)
rpEventBusName = Lens.lens (eventBusName :: RemovePermission -> Lude.Maybe Lude.Text) (\s a -> s {eventBusName = a} :: RemovePermission)
{-# DEPRECATED rpEventBusName "Use generic-lens or generic-optics with 'eventBusName' instead." #-}

-- | Specifies whether to remove all permissions.
--
-- /Note:/ Consider using 'removeAllPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpRemoveAllPermissions :: Lens.Lens' RemovePermission (Lude.Maybe Lude.Bool)
rpRemoveAllPermissions = Lens.lens (removeAllPermissions :: RemovePermission -> Lude.Maybe Lude.Bool) (\s a -> s {removeAllPermissions = a} :: RemovePermission)
{-# DEPRECATED rpRemoveAllPermissions "Use generic-lens or generic-optics with 'removeAllPermissions' instead." #-}

-- | The statement ID corresponding to the account that is no longer allowed to put events to the default event bus.
--
-- /Note:/ Consider using 'statementId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpStatementId :: Lens.Lens' RemovePermission (Lude.Maybe Lude.Text)
rpStatementId = Lens.lens (statementId :: RemovePermission -> Lude.Maybe Lude.Text) (\s a -> s {statementId = a} :: RemovePermission)
{-# DEPRECATED rpStatementId "Use generic-lens or generic-optics with 'statementId' instead." #-}

instance Lude.AWSRequest RemovePermission where
  type Rs RemovePermission = RemovePermissionResponse
  request = Req.postJSON cloudWatchEventsService
  response = Res.receiveNull RemovePermissionResponse'

instance Lude.ToHeaders RemovePermission where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSEvents.RemovePermission" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RemovePermission where
  toJSON RemovePermission' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EventBusName" Lude..=) Lude.<$> eventBusName,
            ("RemoveAllPermissions" Lude..=) Lude.<$> removeAllPermissions,
            ("StatementId" Lude..=) Lude.<$> statementId
          ]
      )

instance Lude.ToPath RemovePermission where
  toPath = Lude.const "/"

instance Lude.ToQuery RemovePermission where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRemovePermissionResponse' smart constructor.
data RemovePermissionResponse = RemovePermissionResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemovePermissionResponse' with the minimum fields required to make a request.
mkRemovePermissionResponse ::
  RemovePermissionResponse
mkRemovePermissionResponse = RemovePermissionResponse'
