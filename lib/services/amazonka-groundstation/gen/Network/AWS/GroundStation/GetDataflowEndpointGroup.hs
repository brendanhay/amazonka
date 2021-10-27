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
-- Module      : Network.AWS.GroundStation.GetDataflowEndpointGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the dataflow endpoint group.
module Network.AWS.GroundStation.GetDataflowEndpointGroup
  ( -- * Creating a Request
    GetDataflowEndpointGroup (..),
    newGetDataflowEndpointGroup,

    -- * Request Lenses
    getDataflowEndpointGroup_dataflowEndpointGroupId,

    -- * Destructuring the Response
    GetDataflowEndpointGroupResponse (..),
    newGetDataflowEndpointGroupResponse,

    -- * Response Lenses
    getDataflowEndpointGroupResponse_endpointsDetails,
    getDataflowEndpointGroupResponse_dataflowEndpointGroupArn,
    getDataflowEndpointGroupResponse_dataflowEndpointGroupId,
    getDataflowEndpointGroupResponse_tags,
    getDataflowEndpointGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GroundStation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newGetDataflowEndpointGroup' smart constructor.
data GetDataflowEndpointGroup = GetDataflowEndpointGroup'
  { -- | UUID of a dataflow endpoint group.
    dataflowEndpointGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataflowEndpointGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataflowEndpointGroupId', 'getDataflowEndpointGroup_dataflowEndpointGroupId' - UUID of a dataflow endpoint group.
newGetDataflowEndpointGroup ::
  -- | 'dataflowEndpointGroupId'
  Prelude.Text ->
  GetDataflowEndpointGroup
newGetDataflowEndpointGroup pDataflowEndpointGroupId_ =
  GetDataflowEndpointGroup'
    { dataflowEndpointGroupId =
        pDataflowEndpointGroupId_
    }

-- | UUID of a dataflow endpoint group.
getDataflowEndpointGroup_dataflowEndpointGroupId :: Lens.Lens' GetDataflowEndpointGroup Prelude.Text
getDataflowEndpointGroup_dataflowEndpointGroupId = Lens.lens (\GetDataflowEndpointGroup' {dataflowEndpointGroupId} -> dataflowEndpointGroupId) (\s@GetDataflowEndpointGroup' {} a -> s {dataflowEndpointGroupId = a} :: GetDataflowEndpointGroup)

instance Core.AWSRequest GetDataflowEndpointGroup where
  type
    AWSResponse GetDataflowEndpointGroup =
      GetDataflowEndpointGroupResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDataflowEndpointGroupResponse'
            Prelude.<$> ( x Core..?> "endpointsDetails"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "dataflowEndpointGroupArn")
            Prelude.<*> (x Core..?> "dataflowEndpointGroupId")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDataflowEndpointGroup

instance Prelude.NFData GetDataflowEndpointGroup

instance Core.ToHeaders GetDataflowEndpointGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetDataflowEndpointGroup where
  toPath GetDataflowEndpointGroup' {..} =
    Prelude.mconcat
      [ "/dataflowEndpointGroup/",
        Core.toBS dataflowEndpointGroupId
      ]

instance Core.ToQuery GetDataflowEndpointGroup where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newGetDataflowEndpointGroupResponse' smart constructor.
data GetDataflowEndpointGroupResponse = GetDataflowEndpointGroupResponse'
  { -- | Details of a dataflow endpoint.
    endpointsDetails :: Prelude.Maybe [EndpointDetails],
    -- | ARN of a dataflow endpoint group.
    dataflowEndpointGroupArn :: Prelude.Maybe Prelude.Text,
    -- | UUID of a dataflow endpoint group.
    dataflowEndpointGroupId :: Prelude.Maybe Prelude.Text,
    -- | Tags assigned to a dataflow endpoint group.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataflowEndpointGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointsDetails', 'getDataflowEndpointGroupResponse_endpointsDetails' - Details of a dataflow endpoint.
--
-- 'dataflowEndpointGroupArn', 'getDataflowEndpointGroupResponse_dataflowEndpointGroupArn' - ARN of a dataflow endpoint group.
--
-- 'dataflowEndpointGroupId', 'getDataflowEndpointGroupResponse_dataflowEndpointGroupId' - UUID of a dataflow endpoint group.
--
-- 'tags', 'getDataflowEndpointGroupResponse_tags' - Tags assigned to a dataflow endpoint group.
--
-- 'httpStatus', 'getDataflowEndpointGroupResponse_httpStatus' - The response's http status code.
newGetDataflowEndpointGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDataflowEndpointGroupResponse
newGetDataflowEndpointGroupResponse pHttpStatus_ =
  GetDataflowEndpointGroupResponse'
    { endpointsDetails =
        Prelude.Nothing,
      dataflowEndpointGroupArn =
        Prelude.Nothing,
      dataflowEndpointGroupId = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details of a dataflow endpoint.
getDataflowEndpointGroupResponse_endpointsDetails :: Lens.Lens' GetDataflowEndpointGroupResponse (Prelude.Maybe [EndpointDetails])
getDataflowEndpointGroupResponse_endpointsDetails = Lens.lens (\GetDataflowEndpointGroupResponse' {endpointsDetails} -> endpointsDetails) (\s@GetDataflowEndpointGroupResponse' {} a -> s {endpointsDetails = a} :: GetDataflowEndpointGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | ARN of a dataflow endpoint group.
getDataflowEndpointGroupResponse_dataflowEndpointGroupArn :: Lens.Lens' GetDataflowEndpointGroupResponse (Prelude.Maybe Prelude.Text)
getDataflowEndpointGroupResponse_dataflowEndpointGroupArn = Lens.lens (\GetDataflowEndpointGroupResponse' {dataflowEndpointGroupArn} -> dataflowEndpointGroupArn) (\s@GetDataflowEndpointGroupResponse' {} a -> s {dataflowEndpointGroupArn = a} :: GetDataflowEndpointGroupResponse)

-- | UUID of a dataflow endpoint group.
getDataflowEndpointGroupResponse_dataflowEndpointGroupId :: Lens.Lens' GetDataflowEndpointGroupResponse (Prelude.Maybe Prelude.Text)
getDataflowEndpointGroupResponse_dataflowEndpointGroupId = Lens.lens (\GetDataflowEndpointGroupResponse' {dataflowEndpointGroupId} -> dataflowEndpointGroupId) (\s@GetDataflowEndpointGroupResponse' {} a -> s {dataflowEndpointGroupId = a} :: GetDataflowEndpointGroupResponse)

-- | Tags assigned to a dataflow endpoint group.
getDataflowEndpointGroupResponse_tags :: Lens.Lens' GetDataflowEndpointGroupResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getDataflowEndpointGroupResponse_tags = Lens.lens (\GetDataflowEndpointGroupResponse' {tags} -> tags) (\s@GetDataflowEndpointGroupResponse' {} a -> s {tags = a} :: GetDataflowEndpointGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getDataflowEndpointGroupResponse_httpStatus :: Lens.Lens' GetDataflowEndpointGroupResponse Prelude.Int
getDataflowEndpointGroupResponse_httpStatus = Lens.lens (\GetDataflowEndpointGroupResponse' {httpStatus} -> httpStatus) (\s@GetDataflowEndpointGroupResponse' {} a -> s {httpStatus = a} :: GetDataflowEndpointGroupResponse)

instance
  Prelude.NFData
    GetDataflowEndpointGroupResponse
