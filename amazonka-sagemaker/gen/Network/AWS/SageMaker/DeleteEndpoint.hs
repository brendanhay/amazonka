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
-- Module      : Network.AWS.SageMaker.DeleteEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an endpoint. Amazon SageMaker frees up all of the resources that
-- were deployed when the endpoint was created.
--
-- Amazon SageMaker retires any custom KMS key grants associated with the
-- endpoint, meaning you don\'t need to use the
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_RevokeGrant.html RevokeGrant>
-- API call.
module Network.AWS.SageMaker.DeleteEndpoint
  ( -- * Creating a Request
    DeleteEndpoint (..),
    newDeleteEndpoint,

    -- * Request Lenses
    deleteEndpoint_endpointName,

    -- * Destructuring the Response
    DeleteEndpointResponse (..),
    newDeleteEndpointResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteEndpoint' smart constructor.
data DeleteEndpoint = DeleteEndpoint'
  { -- | The name of the endpoint that you want to delete.
    endpointName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointName', 'deleteEndpoint_endpointName' - The name of the endpoint that you want to delete.
newDeleteEndpoint ::
  -- | 'endpointName'
  Prelude.Text ->
  DeleteEndpoint
newDeleteEndpoint pEndpointName_ =
  DeleteEndpoint' {endpointName = pEndpointName_}

-- | The name of the endpoint that you want to delete.
deleteEndpoint_endpointName :: Lens.Lens' DeleteEndpoint Prelude.Text
deleteEndpoint_endpointName = Lens.lens (\DeleteEndpoint' {endpointName} -> endpointName) (\s@DeleteEndpoint' {} a -> s {endpointName = a} :: DeleteEndpoint)

instance Prelude.AWSRequest DeleteEndpoint where
  type Rs DeleteEndpoint = DeleteEndpointResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteEndpointResponse'

instance Prelude.Hashable DeleteEndpoint

instance Prelude.NFData DeleteEndpoint

instance Prelude.ToHeaders DeleteEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.DeleteEndpoint" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteEndpoint where
  toJSON DeleteEndpoint' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("EndpointName" Prelude..= endpointName)
          ]
      )

instance Prelude.ToPath DeleteEndpoint where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteEndpointResponse' smart constructor.
data DeleteEndpointResponse = DeleteEndpointResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteEndpointResponse ::
  DeleteEndpointResponse
newDeleteEndpointResponse = DeleteEndpointResponse'

instance Prelude.NFData DeleteEndpointResponse
