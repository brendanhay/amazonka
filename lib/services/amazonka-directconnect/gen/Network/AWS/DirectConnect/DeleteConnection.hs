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
-- Module      : Network.AWS.DirectConnect.DeleteConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified connection.
--
-- Deleting a connection only stops the Direct Connect port hour and data
-- transfer charges. If you are partnering with any third parties to
-- connect with the Direct Connect location, you must cancel your service
-- with them separately.
module Network.AWS.DirectConnect.DeleteConnection
  ( -- * Creating a Request
    DeleteConnection (..),
    newDeleteConnection,

    -- * Request Lenses
    deleteConnection_connectionId,

    -- * Destructuring the Response
    Connection (..),
    newConnection,

    -- * Response Lenses
    connection_bandwidth,
    connection_awsDeviceV2,
    connection_connectionState,
    connection_connectionName,
    connection_macSecKeys,
    connection_providerName,
    connection_connectionId,
    connection_awsLogicalDeviceId,
    connection_hasLogicalRedundancy,
    connection_awsDevice,
    connection_jumboFrameCapable,
    connection_portEncryptionStatus,
    connection_lagId,
    connection_encryptionMode,
    connection_partnerName,
    connection_tags,
    connection_loaIssueTime,
    connection_ownerAccount,
    connection_region,
    connection_vlan,
    connection_location,
    connection_macSecCapable,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteConnection' smart constructor.
data DeleteConnection = DeleteConnection'
  { -- | The ID of the connection.
    connectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionId', 'deleteConnection_connectionId' - The ID of the connection.
newDeleteConnection ::
  -- | 'connectionId'
  Prelude.Text ->
  DeleteConnection
newDeleteConnection pConnectionId_ =
  DeleteConnection' {connectionId = pConnectionId_}

-- | The ID of the connection.
deleteConnection_connectionId :: Lens.Lens' DeleteConnection Prelude.Text
deleteConnection_connectionId = Lens.lens (\DeleteConnection' {connectionId} -> connectionId) (\s@DeleteConnection' {} a -> s {connectionId = a} :: DeleteConnection)

instance Core.AWSRequest DeleteConnection where
  type AWSResponse DeleteConnection = Connection
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable DeleteConnection

instance Prelude.NFData DeleteConnection

instance Core.ToHeaders DeleteConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.DeleteConnection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteConnection where
  toJSON DeleteConnection' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("connectionId" Core..= connectionId)]
      )

instance Core.ToPath DeleteConnection where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteConnection where
  toQuery = Prelude.const Prelude.mempty
