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
-- Module      : Network.AWS.SMS.DisassociateConnector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified connector from AWS SMS.
--
-- After you disassociate a connector, it is no longer available to support
-- replication jobs.
module Network.AWS.SMS.DisassociateConnector
  ( -- * Creating a Request
    DisassociateConnector (..),
    newDisassociateConnector,

    -- * Request Lenses
    disassociateConnector_connectorId,

    -- * Destructuring the Response
    DisassociateConnectorResponse (..),
    newDisassociateConnectorResponse,

    -- * Response Lenses
    disassociateConnectorResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newDisassociateConnector' smart constructor.
data DisassociateConnector = DisassociateConnector'
  { -- | The ID of the connector.
    connectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisassociateConnector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectorId', 'disassociateConnector_connectorId' - The ID of the connector.
newDisassociateConnector ::
  -- | 'connectorId'
  Prelude.Text ->
  DisassociateConnector
newDisassociateConnector pConnectorId_ =
  DisassociateConnector' {connectorId = pConnectorId_}

-- | The ID of the connector.
disassociateConnector_connectorId :: Lens.Lens' DisassociateConnector Prelude.Text
disassociateConnector_connectorId = Lens.lens (\DisassociateConnector' {connectorId} -> connectorId) (\s@DisassociateConnector' {} a -> s {connectorId = a} :: DisassociateConnector)

instance Prelude.AWSRequest DisassociateConnector where
  type
    Rs DisassociateConnector =
      DisassociateConnectorResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateConnectorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateConnector

instance Prelude.NFData DisassociateConnector

instance Prelude.ToHeaders DisassociateConnector where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSServerMigrationService_V2016_10_24.DisassociateConnector" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DisassociateConnector where
  toJSON DisassociateConnector' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("connectorId" Prelude..= connectorId)
          ]
      )

instance Prelude.ToPath DisassociateConnector where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DisassociateConnector where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateConnectorResponse' smart constructor.
data DisassociateConnectorResponse = DisassociateConnectorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisassociateConnectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateConnectorResponse_httpStatus' - The response's http status code.
newDisassociateConnectorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateConnectorResponse
newDisassociateConnectorResponse pHttpStatus_ =
  DisassociateConnectorResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateConnectorResponse_httpStatus :: Lens.Lens' DisassociateConnectorResponse Prelude.Int
disassociateConnectorResponse_httpStatus = Lens.lens (\DisassociateConnectorResponse' {httpStatus} -> httpStatus) (\s@DisassociateConnectorResponse' {} a -> s {httpStatus = a} :: DisassociateConnectorResponse)

instance Prelude.NFData DisassociateConnectorResponse
