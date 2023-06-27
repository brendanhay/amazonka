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
-- Module      : Amazonka.GroundStation.GetDataflowEndpointGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the dataflow endpoint group.
module Amazonka.GroundStation.GetDataflowEndpointGroup
  ( -- * Creating a Request
    GetDataflowEndpointGroup (..),
    newGetDataflowEndpointGroup,

    -- * Request Lenses
    getDataflowEndpointGroup_dataflowEndpointGroupId,

    -- * Destructuring the Response
    GetDataflowEndpointGroupResponse (..),
    newGetDataflowEndpointGroupResponse,

    -- * Response Lenses
    getDataflowEndpointGroupResponse_contactPostPassDurationSeconds,
    getDataflowEndpointGroupResponse_contactPrePassDurationSeconds,
    getDataflowEndpointGroupResponse_dataflowEndpointGroupArn,
    getDataflowEndpointGroupResponse_dataflowEndpointGroupId,
    getDataflowEndpointGroupResponse_endpointsDetails,
    getDataflowEndpointGroupResponse_tags,
    getDataflowEndpointGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDataflowEndpointGroupResponse'
            Prelude.<$> (x Data..?> "contactPostPassDurationSeconds")
            Prelude.<*> (x Data..?> "contactPrePassDurationSeconds")
            Prelude.<*> (x Data..?> "dataflowEndpointGroupArn")
            Prelude.<*> (x Data..?> "dataflowEndpointGroupId")
            Prelude.<*> ( x
                            Data..?> "endpointsDetails"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDataflowEndpointGroup where
  hashWithSalt _salt GetDataflowEndpointGroup' {..} =
    _salt
      `Prelude.hashWithSalt` dataflowEndpointGroupId

instance Prelude.NFData GetDataflowEndpointGroup where
  rnf GetDataflowEndpointGroup' {..} =
    Prelude.rnf dataflowEndpointGroupId

instance Data.ToHeaders GetDataflowEndpointGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetDataflowEndpointGroup where
  toPath GetDataflowEndpointGroup' {..} =
    Prelude.mconcat
      [ "/dataflowEndpointGroup/",
        Data.toBS dataflowEndpointGroupId
      ]

instance Data.ToQuery GetDataflowEndpointGroup where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newGetDataflowEndpointGroupResponse' smart constructor.
data GetDataflowEndpointGroupResponse = GetDataflowEndpointGroupResponse'
  { -- | Amount of time, in seconds, after a contact ends that the Ground Station
    -- Dataflow Endpoint Group will be in a @POSTPASS@ state. A Ground Station
    -- Dataflow Endpoint Group State Change event will be emitted when the
    -- Dataflow Endpoint Group enters and exits the @POSTPASS@ state.
    contactPostPassDurationSeconds :: Prelude.Maybe Prelude.Natural,
    -- | Amount of time, in seconds, before a contact starts that the Ground
    -- Station Dataflow Endpoint Group will be in a @PREPASS@ state. A Ground
    -- Station Dataflow Endpoint Group State Change event will be emitted when
    -- the Dataflow Endpoint Group enters and exits the @PREPASS@ state.
    contactPrePassDurationSeconds :: Prelude.Maybe Prelude.Natural,
    -- | ARN of a dataflow endpoint group.
    dataflowEndpointGroupArn :: Prelude.Maybe Prelude.Text,
    -- | UUID of a dataflow endpoint group.
    dataflowEndpointGroupId :: Prelude.Maybe Prelude.Text,
    -- | Details of a dataflow endpoint.
    endpointsDetails :: Prelude.Maybe [EndpointDetails],
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
-- 'contactPostPassDurationSeconds', 'getDataflowEndpointGroupResponse_contactPostPassDurationSeconds' - Amount of time, in seconds, after a contact ends that the Ground Station
-- Dataflow Endpoint Group will be in a @POSTPASS@ state. A Ground Station
-- Dataflow Endpoint Group State Change event will be emitted when the
-- Dataflow Endpoint Group enters and exits the @POSTPASS@ state.
--
-- 'contactPrePassDurationSeconds', 'getDataflowEndpointGroupResponse_contactPrePassDurationSeconds' - Amount of time, in seconds, before a contact starts that the Ground
-- Station Dataflow Endpoint Group will be in a @PREPASS@ state. A Ground
-- Station Dataflow Endpoint Group State Change event will be emitted when
-- the Dataflow Endpoint Group enters and exits the @PREPASS@ state.
--
-- 'dataflowEndpointGroupArn', 'getDataflowEndpointGroupResponse_dataflowEndpointGroupArn' - ARN of a dataflow endpoint group.
--
-- 'dataflowEndpointGroupId', 'getDataflowEndpointGroupResponse_dataflowEndpointGroupId' - UUID of a dataflow endpoint group.
--
-- 'endpointsDetails', 'getDataflowEndpointGroupResponse_endpointsDetails' - Details of a dataflow endpoint.
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
    { contactPostPassDurationSeconds =
        Prelude.Nothing,
      contactPrePassDurationSeconds =
        Prelude.Nothing,
      dataflowEndpointGroupArn =
        Prelude.Nothing,
      dataflowEndpointGroupId = Prelude.Nothing,
      endpointsDetails = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Amount of time, in seconds, after a contact ends that the Ground Station
-- Dataflow Endpoint Group will be in a @POSTPASS@ state. A Ground Station
-- Dataflow Endpoint Group State Change event will be emitted when the
-- Dataflow Endpoint Group enters and exits the @POSTPASS@ state.
getDataflowEndpointGroupResponse_contactPostPassDurationSeconds :: Lens.Lens' GetDataflowEndpointGroupResponse (Prelude.Maybe Prelude.Natural)
getDataflowEndpointGroupResponse_contactPostPassDurationSeconds = Lens.lens (\GetDataflowEndpointGroupResponse' {contactPostPassDurationSeconds} -> contactPostPassDurationSeconds) (\s@GetDataflowEndpointGroupResponse' {} a -> s {contactPostPassDurationSeconds = a} :: GetDataflowEndpointGroupResponse)

-- | Amount of time, in seconds, before a contact starts that the Ground
-- Station Dataflow Endpoint Group will be in a @PREPASS@ state. A Ground
-- Station Dataflow Endpoint Group State Change event will be emitted when
-- the Dataflow Endpoint Group enters and exits the @PREPASS@ state.
getDataflowEndpointGroupResponse_contactPrePassDurationSeconds :: Lens.Lens' GetDataflowEndpointGroupResponse (Prelude.Maybe Prelude.Natural)
getDataflowEndpointGroupResponse_contactPrePassDurationSeconds = Lens.lens (\GetDataflowEndpointGroupResponse' {contactPrePassDurationSeconds} -> contactPrePassDurationSeconds) (\s@GetDataflowEndpointGroupResponse' {} a -> s {contactPrePassDurationSeconds = a} :: GetDataflowEndpointGroupResponse)

-- | ARN of a dataflow endpoint group.
getDataflowEndpointGroupResponse_dataflowEndpointGroupArn :: Lens.Lens' GetDataflowEndpointGroupResponse (Prelude.Maybe Prelude.Text)
getDataflowEndpointGroupResponse_dataflowEndpointGroupArn = Lens.lens (\GetDataflowEndpointGroupResponse' {dataflowEndpointGroupArn} -> dataflowEndpointGroupArn) (\s@GetDataflowEndpointGroupResponse' {} a -> s {dataflowEndpointGroupArn = a} :: GetDataflowEndpointGroupResponse)

-- | UUID of a dataflow endpoint group.
getDataflowEndpointGroupResponse_dataflowEndpointGroupId :: Lens.Lens' GetDataflowEndpointGroupResponse (Prelude.Maybe Prelude.Text)
getDataflowEndpointGroupResponse_dataflowEndpointGroupId = Lens.lens (\GetDataflowEndpointGroupResponse' {dataflowEndpointGroupId} -> dataflowEndpointGroupId) (\s@GetDataflowEndpointGroupResponse' {} a -> s {dataflowEndpointGroupId = a} :: GetDataflowEndpointGroupResponse)

-- | Details of a dataflow endpoint.
getDataflowEndpointGroupResponse_endpointsDetails :: Lens.Lens' GetDataflowEndpointGroupResponse (Prelude.Maybe [EndpointDetails])
getDataflowEndpointGroupResponse_endpointsDetails = Lens.lens (\GetDataflowEndpointGroupResponse' {endpointsDetails} -> endpointsDetails) (\s@GetDataflowEndpointGroupResponse' {} a -> s {endpointsDetails = a} :: GetDataflowEndpointGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | Tags assigned to a dataflow endpoint group.
getDataflowEndpointGroupResponse_tags :: Lens.Lens' GetDataflowEndpointGroupResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getDataflowEndpointGroupResponse_tags = Lens.lens (\GetDataflowEndpointGroupResponse' {tags} -> tags) (\s@GetDataflowEndpointGroupResponse' {} a -> s {tags = a} :: GetDataflowEndpointGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getDataflowEndpointGroupResponse_httpStatus :: Lens.Lens' GetDataflowEndpointGroupResponse Prelude.Int
getDataflowEndpointGroupResponse_httpStatus = Lens.lens (\GetDataflowEndpointGroupResponse' {httpStatus} -> httpStatus) (\s@GetDataflowEndpointGroupResponse' {} a -> s {httpStatus = a} :: GetDataflowEndpointGroupResponse)

instance
  Prelude.NFData
    GetDataflowEndpointGroupResponse
  where
  rnf GetDataflowEndpointGroupResponse' {..} =
    Prelude.rnf contactPostPassDurationSeconds
      `Prelude.seq` Prelude.rnf contactPrePassDurationSeconds
      `Prelude.seq` Prelude.rnf dataflowEndpointGroupArn
      `Prelude.seq` Prelude.rnf dataflowEndpointGroupId
      `Prelude.seq` Prelude.rnf endpointsDetails
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
