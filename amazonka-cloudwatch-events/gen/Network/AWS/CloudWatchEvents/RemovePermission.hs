{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.RemovePermission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes the permission of another AWS account to be able to put events
-- to the specified event bus. Specify the account to revoke by the
-- @StatementId@ value that you associated with the account when you
-- granted it permission with @PutPermission@. You can find the
-- @StatementId@ by using DescribeEventBus.
module Network.AWS.CloudWatchEvents.RemovePermission
  ( -- * Creating a Request
    RemovePermission (..),
    newRemovePermission,

    -- * Request Lenses
    removePermission_statementId,
    removePermission_eventBusName,
    removePermission_removeAllPermissions,

    -- * Destructuring the Response
    RemovePermissionResponse (..),
    newRemovePermissionResponse,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRemovePermission' smart constructor.
data RemovePermission = RemovePermission'
  { -- | The statement ID corresponding to the account that is no longer allowed
    -- to put events to the default event bus.
    statementId :: Prelude.Maybe Prelude.Text,
    -- | The name of the event bus to revoke permissions for. If you omit this,
    -- the default event bus is used.
    eventBusName :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether to remove all permissions.
    removeAllPermissions :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RemovePermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statementId', 'removePermission_statementId' - The statement ID corresponding to the account that is no longer allowed
-- to put events to the default event bus.
--
-- 'eventBusName', 'removePermission_eventBusName' - The name of the event bus to revoke permissions for. If you omit this,
-- the default event bus is used.
--
-- 'removeAllPermissions', 'removePermission_removeAllPermissions' - Specifies whether to remove all permissions.
newRemovePermission ::
  RemovePermission
newRemovePermission =
  RemovePermission'
    { statementId = Prelude.Nothing,
      eventBusName = Prelude.Nothing,
      removeAllPermissions = Prelude.Nothing
    }

-- | The statement ID corresponding to the account that is no longer allowed
-- to put events to the default event bus.
removePermission_statementId :: Lens.Lens' RemovePermission (Prelude.Maybe Prelude.Text)
removePermission_statementId = Lens.lens (\RemovePermission' {statementId} -> statementId) (\s@RemovePermission' {} a -> s {statementId = a} :: RemovePermission)

-- | The name of the event bus to revoke permissions for. If you omit this,
-- the default event bus is used.
removePermission_eventBusName :: Lens.Lens' RemovePermission (Prelude.Maybe Prelude.Text)
removePermission_eventBusName = Lens.lens (\RemovePermission' {eventBusName} -> eventBusName) (\s@RemovePermission' {} a -> s {eventBusName = a} :: RemovePermission)

-- | Specifies whether to remove all permissions.
removePermission_removeAllPermissions :: Lens.Lens' RemovePermission (Prelude.Maybe Prelude.Bool)
removePermission_removeAllPermissions = Lens.lens (\RemovePermission' {removeAllPermissions} -> removeAllPermissions) (\s@RemovePermission' {} a -> s {removeAllPermissions = a} :: RemovePermission)

instance Prelude.AWSRequest RemovePermission where
  type Rs RemovePermission = RemovePermissionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull RemovePermissionResponse'

instance Prelude.Hashable RemovePermission

instance Prelude.NFData RemovePermission

instance Prelude.ToHeaders RemovePermission where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSEvents.RemovePermission" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON RemovePermission where
  toJSON RemovePermission' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("StatementId" Prelude..=) Prelude.<$> statementId,
            ("EventBusName" Prelude..=) Prelude.<$> eventBusName,
            ("RemoveAllPermissions" Prelude..=)
              Prelude.<$> removeAllPermissions
          ]
      )

instance Prelude.ToPath RemovePermission where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RemovePermission where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemovePermissionResponse' smart constructor.
data RemovePermissionResponse = RemovePermissionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RemovePermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRemovePermissionResponse ::
  RemovePermissionResponse
newRemovePermissionResponse =
  RemovePermissionResponse'

instance Prelude.NFData RemovePermissionResponse
