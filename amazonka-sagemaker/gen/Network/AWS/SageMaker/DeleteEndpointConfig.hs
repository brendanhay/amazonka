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
-- Module      : Network.AWS.SageMaker.DeleteEndpointConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an endpoint configuration. The @DeleteEndpointConfig@ API
-- deletes only the specified configuration. It does not delete endpoints
-- created using the configuration.
--
-- You must not delete an @EndpointConfig@ in use by an endpoint that is
-- live or while the @UpdateEndpoint@ or @CreateEndpoint@ operations are
-- being performed on the endpoint. If you delete the @EndpointConfig@ of
-- an endpoint that is active or being created or updated you may lose
-- visibility into the instance type the endpoint is using. The endpoint
-- must be deleted in order to stop incurring charges.
module Network.AWS.SageMaker.DeleteEndpointConfig
  ( -- * Creating a Request
    DeleteEndpointConfig (..),
    newDeleteEndpointConfig,

    -- * Request Lenses
    deleteEndpointConfig_endpointConfigName,

    -- * Destructuring the Response
    DeleteEndpointConfigResponse (..),
    newDeleteEndpointConfigResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteEndpointConfig' smart constructor.
data DeleteEndpointConfig = DeleteEndpointConfig'
  { -- | The name of the endpoint configuration that you want to delete.
    endpointConfigName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteEndpointConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointConfigName', 'deleteEndpointConfig_endpointConfigName' - The name of the endpoint configuration that you want to delete.
newDeleteEndpointConfig ::
  -- | 'endpointConfigName'
  Prelude.Text ->
  DeleteEndpointConfig
newDeleteEndpointConfig pEndpointConfigName_ =
  DeleteEndpointConfig'
    { endpointConfigName =
        pEndpointConfigName_
    }

-- | The name of the endpoint configuration that you want to delete.
deleteEndpointConfig_endpointConfigName :: Lens.Lens' DeleteEndpointConfig Prelude.Text
deleteEndpointConfig_endpointConfigName = Lens.lens (\DeleteEndpointConfig' {endpointConfigName} -> endpointConfigName) (\s@DeleteEndpointConfig' {} a -> s {endpointConfigName = a} :: DeleteEndpointConfig)

instance Prelude.AWSRequest DeleteEndpointConfig where
  type
    Rs DeleteEndpointConfig =
      DeleteEndpointConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteEndpointConfigResponse'

instance Prelude.Hashable DeleteEndpointConfig

instance Prelude.NFData DeleteEndpointConfig

instance Prelude.ToHeaders DeleteEndpointConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.DeleteEndpointConfig" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteEndpointConfig where
  toJSON DeleteEndpointConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "EndpointConfigName"
                  Prelude..= endpointConfigName
              )
          ]
      )

instance Prelude.ToPath DeleteEndpointConfig where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteEndpointConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteEndpointConfigResponse' smart constructor.
data DeleteEndpointConfigResponse = DeleteEndpointConfigResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteEndpointConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteEndpointConfigResponse ::
  DeleteEndpointConfigResponse
newDeleteEndpointConfigResponse =
  DeleteEndpointConfigResponse'

instance Prelude.NFData DeleteEndpointConfigResponse
