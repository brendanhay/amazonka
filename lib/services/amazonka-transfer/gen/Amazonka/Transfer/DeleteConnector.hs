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
-- Module      : Amazonka.Transfer.DeleteConnector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the agreement that\'s specified in the provided @ConnectorId@.
module Amazonka.Transfer.DeleteConnector
  ( -- * Creating a Request
    DeleteConnector (..),
    newDeleteConnector,

    -- * Request Lenses
    deleteConnector_connectorId,

    -- * Destructuring the Response
    DeleteConnectorResponse (..),
    newDeleteConnectorResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newDeleteConnector' smart constructor.
data DeleteConnector = DeleteConnector'
  { -- | The unique identifier for the connector.
    connectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConnector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectorId', 'deleteConnector_connectorId' - The unique identifier for the connector.
newDeleteConnector ::
  -- | 'connectorId'
  Prelude.Text ->
  DeleteConnector
newDeleteConnector pConnectorId_ =
  DeleteConnector' {connectorId = pConnectorId_}

-- | The unique identifier for the connector.
deleteConnector_connectorId :: Lens.Lens' DeleteConnector Prelude.Text
deleteConnector_connectorId = Lens.lens (\DeleteConnector' {connectorId} -> connectorId) (\s@DeleteConnector' {} a -> s {connectorId = a} :: DeleteConnector)

instance Core.AWSRequest DeleteConnector where
  type
    AWSResponse DeleteConnector =
      DeleteConnectorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteConnectorResponse'

instance Prelude.Hashable DeleteConnector where
  hashWithSalt _salt DeleteConnector' {..} =
    _salt `Prelude.hashWithSalt` connectorId

instance Prelude.NFData DeleteConnector where
  rnf DeleteConnector' {..} = Prelude.rnf connectorId

instance Data.ToHeaders DeleteConnector where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TransferService.DeleteConnector" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteConnector where
  toJSON DeleteConnector' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ConnectorId" Data..= connectorId)]
      )

instance Data.ToPath DeleteConnector where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteConnector where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteConnectorResponse' smart constructor.
data DeleteConnectorResponse = DeleteConnectorResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConnectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteConnectorResponse ::
  DeleteConnectorResponse
newDeleteConnectorResponse = DeleteConnectorResponse'

instance Prelude.NFData DeleteConnectorResponse where
  rnf _ = ()
