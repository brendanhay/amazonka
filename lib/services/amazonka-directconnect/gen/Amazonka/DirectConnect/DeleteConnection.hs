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
-- Module      : Amazonka.DirectConnect.DeleteConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.DirectConnect.DeleteConnection
  ( -- * Creating a Request
    DeleteConnection (..),
    newDeleteConnection,

    -- * Request Lenses
    deleteConnection_connectionId,

    -- * Destructuring the Response
    Connection (..),
    newConnection,

    -- * Response Lenses
    connection_awsDevice,
    connection_awsDeviceV2,
    connection_awsLogicalDeviceId,
    connection_bandwidth,
    connection_connectionId,
    connection_connectionName,
    connection_connectionState,
    connection_encryptionMode,
    connection_hasLogicalRedundancy,
    connection_jumboFrameCapable,
    connection_lagId,
    connection_loaIssueTime,
    connection_location,
    connection_macSecCapable,
    connection_macSecKeys,
    connection_ownerAccount,
    connection_partnerName,
    connection_portEncryptionStatus,
    connection_providerName,
    connection_region,
    connection_tags,
    connection_vlan,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable DeleteConnection where
  hashWithSalt _salt DeleteConnection' {..} =
    _salt `Prelude.hashWithSalt` connectionId

instance Prelude.NFData DeleteConnection where
  rnf DeleteConnection' {..} = Prelude.rnf connectionId

instance Data.ToHeaders DeleteConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OvertureService.DeleteConnection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteConnection where
  toJSON DeleteConnection' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("connectionId" Data..= connectionId)]
      )

instance Data.ToPath DeleteConnection where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteConnection where
  toQuery = Prelude.const Prelude.mempty
