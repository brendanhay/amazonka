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
-- Module      : Amazonka.Connect.DescribeTrafficDistributionGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details and status of a traffic distribution group.
module Amazonka.Connect.DescribeTrafficDistributionGroup
  ( -- * Creating a Request
    DescribeTrafficDistributionGroup (..),
    newDescribeTrafficDistributionGroup,

    -- * Request Lenses
    describeTrafficDistributionGroup_trafficDistributionGroupId,

    -- * Destructuring the Response
    DescribeTrafficDistributionGroupResponse (..),
    newDescribeTrafficDistributionGroupResponse,

    -- * Response Lenses
    describeTrafficDistributionGroupResponse_trafficDistributionGroup,
    describeTrafficDistributionGroupResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTrafficDistributionGroup' smart constructor.
data DescribeTrafficDistributionGroup = DescribeTrafficDistributionGroup'
  { -- | The identifier of the traffic distribution group. This can be the ID or
    -- the ARN if the API is being called in the Region where the traffic
    -- distribution group was created. The ARN must be provided if the call is
    -- from the replicated Region.
    trafficDistributionGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTrafficDistributionGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trafficDistributionGroupId', 'describeTrafficDistributionGroup_trafficDistributionGroupId' - The identifier of the traffic distribution group. This can be the ID or
-- the ARN if the API is being called in the Region where the traffic
-- distribution group was created. The ARN must be provided if the call is
-- from the replicated Region.
newDescribeTrafficDistributionGroup ::
  -- | 'trafficDistributionGroupId'
  Prelude.Text ->
  DescribeTrafficDistributionGroup
newDescribeTrafficDistributionGroup
  pTrafficDistributionGroupId_ =
    DescribeTrafficDistributionGroup'
      { trafficDistributionGroupId =
          pTrafficDistributionGroupId_
      }

-- | The identifier of the traffic distribution group. This can be the ID or
-- the ARN if the API is being called in the Region where the traffic
-- distribution group was created. The ARN must be provided if the call is
-- from the replicated Region.
describeTrafficDistributionGroup_trafficDistributionGroupId :: Lens.Lens' DescribeTrafficDistributionGroup Prelude.Text
describeTrafficDistributionGroup_trafficDistributionGroupId = Lens.lens (\DescribeTrafficDistributionGroup' {trafficDistributionGroupId} -> trafficDistributionGroupId) (\s@DescribeTrafficDistributionGroup' {} a -> s {trafficDistributionGroupId = a} :: DescribeTrafficDistributionGroup)

instance
  Core.AWSRequest
    DescribeTrafficDistributionGroup
  where
  type
    AWSResponse DescribeTrafficDistributionGroup =
      DescribeTrafficDistributionGroupResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTrafficDistributionGroupResponse'
            Prelude.<$> (x Data..?> "TrafficDistributionGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeTrafficDistributionGroup
  where
  hashWithSalt
    _salt
    DescribeTrafficDistributionGroup' {..} =
      _salt
        `Prelude.hashWithSalt` trafficDistributionGroupId

instance
  Prelude.NFData
    DescribeTrafficDistributionGroup
  where
  rnf DescribeTrafficDistributionGroup' {..} =
    Prelude.rnf trafficDistributionGroupId

instance
  Data.ToHeaders
    DescribeTrafficDistributionGroup
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeTrafficDistributionGroup where
  toPath DescribeTrafficDistributionGroup' {..} =
    Prelude.mconcat
      [ "/traffic-distribution-group/",
        Data.toBS trafficDistributionGroupId
      ]

instance
  Data.ToQuery
    DescribeTrafficDistributionGroup
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTrafficDistributionGroupResponse' smart constructor.
data DescribeTrafficDistributionGroupResponse = DescribeTrafficDistributionGroupResponse'
  { -- | Information about the traffic distribution group.
    trafficDistributionGroup :: Prelude.Maybe TrafficDistributionGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTrafficDistributionGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trafficDistributionGroup', 'describeTrafficDistributionGroupResponse_trafficDistributionGroup' - Information about the traffic distribution group.
--
-- 'httpStatus', 'describeTrafficDistributionGroupResponse_httpStatus' - The response's http status code.
newDescribeTrafficDistributionGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTrafficDistributionGroupResponse
newDescribeTrafficDistributionGroupResponse
  pHttpStatus_ =
    DescribeTrafficDistributionGroupResponse'
      { trafficDistributionGroup =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the traffic distribution group.
describeTrafficDistributionGroupResponse_trafficDistributionGroup :: Lens.Lens' DescribeTrafficDistributionGroupResponse (Prelude.Maybe TrafficDistributionGroup)
describeTrafficDistributionGroupResponse_trafficDistributionGroup = Lens.lens (\DescribeTrafficDistributionGroupResponse' {trafficDistributionGroup} -> trafficDistributionGroup) (\s@DescribeTrafficDistributionGroupResponse' {} a -> s {trafficDistributionGroup = a} :: DescribeTrafficDistributionGroupResponse)

-- | The response's http status code.
describeTrafficDistributionGroupResponse_httpStatus :: Lens.Lens' DescribeTrafficDistributionGroupResponse Prelude.Int
describeTrafficDistributionGroupResponse_httpStatus = Lens.lens (\DescribeTrafficDistributionGroupResponse' {httpStatus} -> httpStatus) (\s@DescribeTrafficDistributionGroupResponse' {} a -> s {httpStatus = a} :: DescribeTrafficDistributionGroupResponse)

instance
  Prelude.NFData
    DescribeTrafficDistributionGroupResponse
  where
  rnf DescribeTrafficDistributionGroupResponse' {..} =
    Prelude.rnf trafficDistributionGroup `Prelude.seq`
      Prelude.rnf httpStatus
