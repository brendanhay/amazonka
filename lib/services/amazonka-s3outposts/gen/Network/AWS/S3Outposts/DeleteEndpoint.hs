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
-- Module      : Network.AWS.S3Outposts.DeleteEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon S3 on Outposts Access Points simplify managing data access at
-- scale for shared datasets in S3 on Outposts. S3 on Outposts uses
-- endpoints to connect to Outposts buckets so that you can perform actions
-- within your virtual private cloud (VPC). For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/AccessingS3Outposts.html Accessing S3 on Outposts using VPC only access points>.
--
-- This action deletes an endpoint.
--
-- It can take up to 5 minutes for this action to complete.
--
-- Related actions include:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_s3outposts_CreateEndpoint.html CreateEndpoint>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_s3outposts_ListEndpoints.html ListEndpoints>
module Network.AWS.S3Outposts.DeleteEndpoint
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3Outposts.Types

-- | /See:/ 'newDeleteEndpoint' smart constructor.
data DeleteEndpoint = DeleteEndpoint'
  { -- | The ID of the endpoint.
    endpointId :: Prelude.Text,
    -- | The ID of the AWS Outposts.
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
-- 'outpostId', 'deleteEndpoint_outpostId' - The ID of the AWS Outposts.
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

-- | The ID of the AWS Outposts.
deleteEndpoint_outpostId :: Lens.Lens' DeleteEndpoint Prelude.Text
deleteEndpoint_outpostId = Lens.lens (\DeleteEndpoint' {outpostId} -> outpostId) (\s@DeleteEndpoint' {} a -> s {outpostId = a} :: DeleteEndpoint)

instance Core.AWSRequest DeleteEndpoint where
  type
    AWSResponse DeleteEndpoint =
      DeleteEndpointResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteEndpointResponse'

instance Prelude.Hashable DeleteEndpoint

instance Prelude.NFData DeleteEndpoint

instance Core.ToHeaders DeleteEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteEndpoint where
  toPath = Prelude.const "/S3Outposts/DeleteEndpoint"

instance Core.ToQuery DeleteEndpoint where
  toQuery DeleteEndpoint' {..} =
    Prelude.mconcat
      [ "endpointId" Core.=: endpointId,
        "outpostId" Core.=: outpostId
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

instance Prelude.NFData DeleteEndpointResponse
