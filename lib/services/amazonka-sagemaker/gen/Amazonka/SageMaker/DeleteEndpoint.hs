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
-- Module      : Amazonka.SageMaker.DeleteEndpoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an endpoint. SageMaker frees up all of the resources that were
-- deployed when the endpoint was created.
--
-- SageMaker retires any custom KMS key grants associated with the
-- endpoint, meaning you don\'t need to use the
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_RevokeGrant.html RevokeGrant>
-- API call.
--
-- When you delete your endpoint, SageMaker asynchronously deletes
-- associated endpoint resources such as KMS key grants. You might still
-- see these resources in your account for a few minutes after deleting
-- your endpoint. Do not delete or revoke the permissions for your
-- @ ExecutionRoleArn @, otherwise SageMaker cannot delete these resources.
module Amazonka.SageMaker.DeleteEndpoint
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeleteEndpoint' smart constructor.
data DeleteEndpoint = DeleteEndpoint'
  { -- | The name of the endpoint that you want to delete.
    endpointName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteEndpoint where
  type
    AWSResponse DeleteEndpoint =
      DeleteEndpointResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteEndpointResponse'

instance Prelude.Hashable DeleteEndpoint where
  hashWithSalt _salt DeleteEndpoint' {..} =
    _salt `Prelude.hashWithSalt` endpointName

instance Prelude.NFData DeleteEndpoint where
  rnf DeleteEndpoint' {..} = Prelude.rnf endpointName

instance Data.ToHeaders DeleteEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.DeleteEndpoint" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteEndpoint where
  toJSON DeleteEndpoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("EndpointName" Data..= endpointName)]
      )

instance Data.ToPath DeleteEndpoint where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteEndpointResponse' smart constructor.
data DeleteEndpointResponse = DeleteEndpointResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteEndpointResponse ::
  DeleteEndpointResponse
newDeleteEndpointResponse = DeleteEndpointResponse'

instance Prelude.NFData DeleteEndpointResponse where
  rnf _ = ()
