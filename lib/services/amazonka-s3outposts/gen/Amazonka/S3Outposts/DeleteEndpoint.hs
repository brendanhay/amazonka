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
-- Module      : Amazonka.S3Outposts.DeleteEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an endpoint.
--
-- It can take up to 5 minutes for this action to finish.
--
-- Related actions include:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_s3outposts_CreateEndpoint.html CreateEndpoint>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_s3outposts_ListEndpoints.html ListEndpoints>
module Amazonka.S3Outposts.DeleteEndpoint
  ( -- * Creating a Request
    DeleteEndpoint (..),
    newDeleteEndpoint,

    -- * Request Lenses
    deleteEndpoint_endpointId,
    deleteEndpoint_outpostId,

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
import Amazonka.S3Outposts.Types

-- | /See:/ 'newDeleteEndpoint' smart constructor.
data DeleteEndpoint = DeleteEndpoint'
  { -- | The ID of the endpoint.
    endpointId :: Prelude.Text,
    -- | The ID of the Outposts.
    outpostId :: Prelude.Text
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
-- 'endpointId', 'deleteEndpoint_endpointId' - The ID of the endpoint.
--
-- 'outpostId', 'deleteEndpoint_outpostId' - The ID of the Outposts.
newDeleteEndpoint ::
  -- | 'endpointId'
  Prelude.Text ->
  -- | 'outpostId'
  Prelude.Text ->
  DeleteEndpoint
newDeleteEndpoint pEndpointId_ pOutpostId_ =
  DeleteEndpoint'
    { endpointId = pEndpointId_,
      outpostId = pOutpostId_
    }

-- | The ID of the endpoint.
deleteEndpoint_endpointId :: Lens.Lens' DeleteEndpoint Prelude.Text
deleteEndpoint_endpointId = Lens.lens (\DeleteEndpoint' {endpointId} -> endpointId) (\s@DeleteEndpoint' {} a -> s {endpointId = a} :: DeleteEndpoint)

-- | The ID of the Outposts.
deleteEndpoint_outpostId :: Lens.Lens' DeleteEndpoint Prelude.Text
deleteEndpoint_outpostId = Lens.lens (\DeleteEndpoint' {outpostId} -> outpostId) (\s@DeleteEndpoint' {} a -> s {outpostId = a} :: DeleteEndpoint)

instance Core.AWSRequest DeleteEndpoint where
  type
    AWSResponse DeleteEndpoint =
      DeleteEndpointResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteEndpointResponse'

instance Prelude.Hashable DeleteEndpoint where
  hashWithSalt _salt DeleteEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` endpointId
      `Prelude.hashWithSalt` outpostId

instance Prelude.NFData DeleteEndpoint where
  rnf DeleteEndpoint' {..} =
    Prelude.rnf endpointId `Prelude.seq`
      Prelude.rnf outpostId

instance Data.ToHeaders DeleteEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteEndpoint where
  toPath = Prelude.const "/S3Outposts/DeleteEndpoint"

instance Data.ToQuery DeleteEndpoint where
  toQuery DeleteEndpoint' {..} =
    Prelude.mconcat
      [ "endpointId" Data.=: endpointId,
        "outpostId" Data.=: outpostId
      ]

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
