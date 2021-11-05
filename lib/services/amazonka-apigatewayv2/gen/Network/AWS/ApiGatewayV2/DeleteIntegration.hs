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
-- Module      : Network.AWS.ApiGatewayV2.DeleteIntegration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Integration.
module Network.AWS.ApiGatewayV2.DeleteIntegration
  ( -- * Creating a Request
    DeleteIntegration (..),
    newDeleteIntegration,

    -- * Request Lenses
    deleteIntegration_apiId,
    deleteIntegration_integrationId,

    -- * Destructuring the Response
    DeleteIntegrationResponse' (..),
    newDeleteIntegrationResponse',
  )
where

import Network.AWS.ApiGatewayV2.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteIntegration' smart constructor.
data DeleteIntegration = DeleteIntegration'
  { -- | The API identifier.
    apiId :: Prelude.Text,
    -- | The integration ID.
    integrationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIntegration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiId', 'deleteIntegration_apiId' - The API identifier.
--
-- 'integrationId', 'deleteIntegration_integrationId' - The integration ID.
newDeleteIntegration ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'integrationId'
  Prelude.Text ->
  DeleteIntegration
newDeleteIntegration pApiId_ pIntegrationId_ =
  DeleteIntegration'
    { apiId = pApiId_,
      integrationId = pIntegrationId_
    }

-- | The API identifier.
deleteIntegration_apiId :: Lens.Lens' DeleteIntegration Prelude.Text
deleteIntegration_apiId = Lens.lens (\DeleteIntegration' {apiId} -> apiId) (\s@DeleteIntegration' {} a -> s {apiId = a} :: DeleteIntegration)

-- | The integration ID.
deleteIntegration_integrationId :: Lens.Lens' DeleteIntegration Prelude.Text
deleteIntegration_integrationId = Lens.lens (\DeleteIntegration' {integrationId} -> integrationId) (\s@DeleteIntegration' {} a -> s {integrationId = a} :: DeleteIntegration)

instance Core.AWSRequest DeleteIntegration where
  type
    AWSResponse DeleteIntegration =
      DeleteIntegrationResponse'
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteIntegrationResponse''

instance Prelude.Hashable DeleteIntegration

instance Prelude.NFData DeleteIntegration

instance Core.ToHeaders DeleteIntegration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteIntegration where
  toPath DeleteIntegration' {..} =
    Prelude.mconcat
      [ "/v2/apis/",
        Core.toBS apiId,
        "/integrations/",
        Core.toBS integrationId
      ]

instance Core.ToQuery DeleteIntegration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteIntegrationResponse'' smart constructor.
data DeleteIntegrationResponse' = DeleteIntegrationResponse''
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIntegrationResponse'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteIntegrationResponse' ::
  DeleteIntegrationResponse'
newDeleteIntegrationResponse' =
  DeleteIntegrationResponse''

instance Prelude.NFData DeleteIntegrationResponse'
