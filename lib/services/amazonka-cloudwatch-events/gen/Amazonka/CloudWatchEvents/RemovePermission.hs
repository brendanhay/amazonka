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
-- Module      : Amazonka.CloudWatchEvents.RemovePermission
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes the permission of another Amazon Web Services account to be able
-- to put events to the specified event bus. Specify the account to revoke
-- by the @StatementId@ value that you associated with the account when you
-- granted it permission with @PutPermission@. You can find the
-- @StatementId@ by using
-- <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_DescribeEventBus.html DescribeEventBus>.
module Amazonka.CloudWatchEvents.RemovePermission
  ( -- * Creating a Request
    RemovePermission (..),
    newRemovePermission,

    -- * Request Lenses
    removePermission_eventBusName,
    removePermission_removeAllPermissions,
    removePermission_statementId,

    -- * Destructuring the Response
    RemovePermissionResponse (..),
    newRemovePermissionResponse,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRemovePermission' smart constructor.
data RemovePermission = RemovePermission'
  { -- | The name of the event bus to revoke permissions for. If you omit this,
    -- the default event bus is used.
    eventBusName :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether to remove all permissions.
    removeAllPermissions :: Prelude.Maybe Prelude.Bool,
    -- | The statement ID corresponding to the account that is no longer allowed
    -- to put events to the default event bus.
    statementId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemovePermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventBusName', 'removePermission_eventBusName' - The name of the event bus to revoke permissions for. If you omit this,
-- the default event bus is used.
--
-- 'removeAllPermissions', 'removePermission_removeAllPermissions' - Specifies whether to remove all permissions.
--
-- 'statementId', 'removePermission_statementId' - The statement ID corresponding to the account that is no longer allowed
-- to put events to the default event bus.
newRemovePermission ::
  RemovePermission
newRemovePermission =
  RemovePermission'
    { eventBusName = Prelude.Nothing,
      removeAllPermissions = Prelude.Nothing,
      statementId = Prelude.Nothing
    }

-- | The name of the event bus to revoke permissions for. If you omit this,
-- the default event bus is used.
removePermission_eventBusName :: Lens.Lens' RemovePermission (Prelude.Maybe Prelude.Text)
removePermission_eventBusName = Lens.lens (\RemovePermission' {eventBusName} -> eventBusName) (\s@RemovePermission' {} a -> s {eventBusName = a} :: RemovePermission)

-- | Specifies whether to remove all permissions.
removePermission_removeAllPermissions :: Lens.Lens' RemovePermission (Prelude.Maybe Prelude.Bool)
removePermission_removeAllPermissions = Lens.lens (\RemovePermission' {removeAllPermissions} -> removeAllPermissions) (\s@RemovePermission' {} a -> s {removeAllPermissions = a} :: RemovePermission)

-- | The statement ID corresponding to the account that is no longer allowed
-- to put events to the default event bus.
removePermission_statementId :: Lens.Lens' RemovePermission (Prelude.Maybe Prelude.Text)
removePermission_statementId = Lens.lens (\RemovePermission' {statementId} -> statementId) (\s@RemovePermission' {} a -> s {statementId = a} :: RemovePermission)

instance Core.AWSRequest RemovePermission where
  type
    AWSResponse RemovePermission =
      RemovePermissionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull RemovePermissionResponse'

instance Prelude.Hashable RemovePermission where
  hashWithSalt _salt RemovePermission' {..} =
    _salt
      `Prelude.hashWithSalt` eventBusName
      `Prelude.hashWithSalt` removeAllPermissions
      `Prelude.hashWithSalt` statementId

instance Prelude.NFData RemovePermission where
  rnf RemovePermission' {..} =
    Prelude.rnf eventBusName
      `Prelude.seq` Prelude.rnf removeAllPermissions
      `Prelude.seq` Prelude.rnf statementId

instance Data.ToHeaders RemovePermission where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSEvents.RemovePermission" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RemovePermission where
  toJSON RemovePermission' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EventBusName" Data..=) Prelude.<$> eventBusName,
            ("RemoveAllPermissions" Data..=)
              Prelude.<$> removeAllPermissions,
            ("StatementId" Data..=) Prelude.<$> statementId
          ]
      )

instance Data.ToPath RemovePermission where
  toPath = Prelude.const "/"

instance Data.ToQuery RemovePermission where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemovePermissionResponse' smart constructor.
data RemovePermissionResponse = RemovePermissionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemovePermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRemovePermissionResponse ::
  RemovePermissionResponse
newRemovePermissionResponse =
  RemovePermissionResponse'

instance Prelude.NFData RemovePermissionResponse where
  rnf _ = ()
