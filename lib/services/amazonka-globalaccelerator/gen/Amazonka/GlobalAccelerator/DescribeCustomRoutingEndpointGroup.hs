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
-- Module      : Amazonka.GlobalAccelerator.DescribeCustomRoutingEndpointGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe an endpoint group for a custom routing accelerator.
module Amazonka.GlobalAccelerator.DescribeCustomRoutingEndpointGroup
  ( -- * Creating a Request
    DescribeCustomRoutingEndpointGroup (..),
    newDescribeCustomRoutingEndpointGroup,

    -- * Request Lenses
    describeCustomRoutingEndpointGroup_endpointGroupArn,

    -- * Destructuring the Response
    DescribeCustomRoutingEndpointGroupResponse (..),
    newDescribeCustomRoutingEndpointGroupResponse,

    -- * Response Lenses
    describeCustomRoutingEndpointGroupResponse_endpointGroup,
    describeCustomRoutingEndpointGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeCustomRoutingEndpointGroup' smart constructor.
data DescribeCustomRoutingEndpointGroup = DescribeCustomRoutingEndpointGroup'
  { -- | The Amazon Resource Name (ARN) of the endpoint group to describe.
    endpointGroupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCustomRoutingEndpointGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointGroupArn', 'describeCustomRoutingEndpointGroup_endpointGroupArn' - The Amazon Resource Name (ARN) of the endpoint group to describe.
newDescribeCustomRoutingEndpointGroup ::
  -- | 'endpointGroupArn'
  Prelude.Text ->
  DescribeCustomRoutingEndpointGroup
newDescribeCustomRoutingEndpointGroup
  pEndpointGroupArn_ =
    DescribeCustomRoutingEndpointGroup'
      { endpointGroupArn =
          pEndpointGroupArn_
      }

-- | The Amazon Resource Name (ARN) of the endpoint group to describe.
describeCustomRoutingEndpointGroup_endpointGroupArn :: Lens.Lens' DescribeCustomRoutingEndpointGroup Prelude.Text
describeCustomRoutingEndpointGroup_endpointGroupArn = Lens.lens (\DescribeCustomRoutingEndpointGroup' {endpointGroupArn} -> endpointGroupArn) (\s@DescribeCustomRoutingEndpointGroup' {} a -> s {endpointGroupArn = a} :: DescribeCustomRoutingEndpointGroup)

instance
  Core.AWSRequest
    DescribeCustomRoutingEndpointGroup
  where
  type
    AWSResponse DescribeCustomRoutingEndpointGroup =
      DescribeCustomRoutingEndpointGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCustomRoutingEndpointGroupResponse'
            Prelude.<$> (x Data..?> "EndpointGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeCustomRoutingEndpointGroup
  where
  hashWithSalt
    _salt
    DescribeCustomRoutingEndpointGroup' {..} =
      _salt `Prelude.hashWithSalt` endpointGroupArn

instance
  Prelude.NFData
    DescribeCustomRoutingEndpointGroup
  where
  rnf DescribeCustomRoutingEndpointGroup' {..} =
    Prelude.rnf endpointGroupArn

instance
  Data.ToHeaders
    DescribeCustomRoutingEndpointGroup
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GlobalAccelerator_V20180706.DescribeCustomRoutingEndpointGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeCustomRoutingEndpointGroup
  where
  toJSON DescribeCustomRoutingEndpointGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("EndpointGroupArn" Data..= endpointGroupArn)
          ]
      )

instance
  Data.ToPath
    DescribeCustomRoutingEndpointGroup
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeCustomRoutingEndpointGroup
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeCustomRoutingEndpointGroupResponse' smart constructor.
data DescribeCustomRoutingEndpointGroupResponse = DescribeCustomRoutingEndpointGroupResponse'
  { -- | The description of an endpoint group for a custom routing accelerator.
    endpointGroup :: Prelude.Maybe CustomRoutingEndpointGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCustomRoutingEndpointGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointGroup', 'describeCustomRoutingEndpointGroupResponse_endpointGroup' - The description of an endpoint group for a custom routing accelerator.
--
-- 'httpStatus', 'describeCustomRoutingEndpointGroupResponse_httpStatus' - The response's http status code.
newDescribeCustomRoutingEndpointGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCustomRoutingEndpointGroupResponse
newDescribeCustomRoutingEndpointGroupResponse
  pHttpStatus_ =
    DescribeCustomRoutingEndpointGroupResponse'
      { endpointGroup =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The description of an endpoint group for a custom routing accelerator.
describeCustomRoutingEndpointGroupResponse_endpointGroup :: Lens.Lens' DescribeCustomRoutingEndpointGroupResponse (Prelude.Maybe CustomRoutingEndpointGroup)
describeCustomRoutingEndpointGroupResponse_endpointGroup = Lens.lens (\DescribeCustomRoutingEndpointGroupResponse' {endpointGroup} -> endpointGroup) (\s@DescribeCustomRoutingEndpointGroupResponse' {} a -> s {endpointGroup = a} :: DescribeCustomRoutingEndpointGroupResponse)

-- | The response's http status code.
describeCustomRoutingEndpointGroupResponse_httpStatus :: Lens.Lens' DescribeCustomRoutingEndpointGroupResponse Prelude.Int
describeCustomRoutingEndpointGroupResponse_httpStatus = Lens.lens (\DescribeCustomRoutingEndpointGroupResponse' {httpStatus} -> httpStatus) (\s@DescribeCustomRoutingEndpointGroupResponse' {} a -> s {httpStatus = a} :: DescribeCustomRoutingEndpointGroupResponse)

instance
  Prelude.NFData
    DescribeCustomRoutingEndpointGroupResponse
  where
  rnf DescribeCustomRoutingEndpointGroupResponse' {..} =
    Prelude.rnf endpointGroup `Prelude.seq`
      Prelude.rnf httpStatus
