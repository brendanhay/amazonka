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
-- Module      : Amazonka.AppFlow.UnregisterConnector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unregisters the custom connector registered in your account that matches
-- the connector label provided in the request.
module Amazonka.AppFlow.UnregisterConnector
  ( -- * Creating a Request
    UnregisterConnector (..),
    newUnregisterConnector,

    -- * Request Lenses
    unregisterConnector_forceDelete,
    unregisterConnector_connectorLabel,

    -- * Destructuring the Response
    UnregisterConnectorResponse (..),
    newUnregisterConnectorResponse,

    -- * Response Lenses
    unregisterConnectorResponse_httpStatus,
  )
where

import Amazonka.AppFlow.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUnregisterConnector' smart constructor.
data UnregisterConnector = UnregisterConnector'
  { -- | Indicates whether Amazon AppFlow should unregister the connector, even
    -- if it is currently in use in one or more connector profiles. The default
    -- value is false.
    forceDelete :: Prelude.Maybe Prelude.Bool,
    -- | The label of the connector. The label is unique for each
    -- @ConnectorRegistration@ in your Amazon Web Services account.
    connectorLabel :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnregisterConnector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forceDelete', 'unregisterConnector_forceDelete' - Indicates whether Amazon AppFlow should unregister the connector, even
-- if it is currently in use in one or more connector profiles. The default
-- value is false.
--
-- 'connectorLabel', 'unregisterConnector_connectorLabel' - The label of the connector. The label is unique for each
-- @ConnectorRegistration@ in your Amazon Web Services account.
newUnregisterConnector ::
  -- | 'connectorLabel'
  Prelude.Text ->
  UnregisterConnector
newUnregisterConnector pConnectorLabel_ =
  UnregisterConnector'
    { forceDelete = Prelude.Nothing,
      connectorLabel = pConnectorLabel_
    }

-- | Indicates whether Amazon AppFlow should unregister the connector, even
-- if it is currently in use in one or more connector profiles. The default
-- value is false.
unregisterConnector_forceDelete :: Lens.Lens' UnregisterConnector (Prelude.Maybe Prelude.Bool)
unregisterConnector_forceDelete = Lens.lens (\UnregisterConnector' {forceDelete} -> forceDelete) (\s@UnregisterConnector' {} a -> s {forceDelete = a} :: UnregisterConnector)

-- | The label of the connector. The label is unique for each
-- @ConnectorRegistration@ in your Amazon Web Services account.
unregisterConnector_connectorLabel :: Lens.Lens' UnregisterConnector Prelude.Text
unregisterConnector_connectorLabel = Lens.lens (\UnregisterConnector' {connectorLabel} -> connectorLabel) (\s@UnregisterConnector' {} a -> s {connectorLabel = a} :: UnregisterConnector)

instance Core.AWSRequest UnregisterConnector where
  type
    AWSResponse UnregisterConnector =
      UnregisterConnectorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UnregisterConnectorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UnregisterConnector where
  hashWithSalt _salt UnregisterConnector' {..} =
    _salt
      `Prelude.hashWithSalt` forceDelete
      `Prelude.hashWithSalt` connectorLabel

instance Prelude.NFData UnregisterConnector where
  rnf UnregisterConnector' {..} =
    Prelude.rnf forceDelete
      `Prelude.seq` Prelude.rnf connectorLabel

instance Data.ToHeaders UnregisterConnector where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UnregisterConnector where
  toJSON UnregisterConnector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("forceDelete" Data..=) Prelude.<$> forceDelete,
            Prelude.Just
              ("connectorLabel" Data..= connectorLabel)
          ]
      )

instance Data.ToPath UnregisterConnector where
  toPath = Prelude.const "/unregister-connector"

instance Data.ToQuery UnregisterConnector where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUnregisterConnectorResponse' smart constructor.
data UnregisterConnectorResponse = UnregisterConnectorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnregisterConnectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'unregisterConnectorResponse_httpStatus' - The response's http status code.
newUnregisterConnectorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UnregisterConnectorResponse
newUnregisterConnectorResponse pHttpStatus_ =
  UnregisterConnectorResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
unregisterConnectorResponse_httpStatus :: Lens.Lens' UnregisterConnectorResponse Prelude.Int
unregisterConnectorResponse_httpStatus = Lens.lens (\UnregisterConnectorResponse' {httpStatus} -> httpStatus) (\s@UnregisterConnectorResponse' {} a -> s {httpStatus = a} :: UnregisterConnectorResponse)

instance Prelude.NFData UnregisterConnectorResponse where
  rnf UnregisterConnectorResponse' {..} =
    Prelude.rnf httpStatus
