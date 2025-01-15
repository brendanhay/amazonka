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
-- Module      : Amazonka.GlobalAccelerator.DescribeEndpointGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe an endpoint group.
module Amazonka.GlobalAccelerator.DescribeEndpointGroup
  ( -- * Creating a Request
    DescribeEndpointGroup (..),
    newDescribeEndpointGroup,

    -- * Request Lenses
    describeEndpointGroup_endpointGroupArn,

    -- * Destructuring the Response
    DescribeEndpointGroupResponse (..),
    newDescribeEndpointGroupResponse,

    -- * Response Lenses
    describeEndpointGroupResponse_endpointGroup,
    describeEndpointGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeEndpointGroup' smart constructor.
data DescribeEndpointGroup = DescribeEndpointGroup'
  { -- | The Amazon Resource Name (ARN) of the endpoint group to describe.
    endpointGroupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEndpointGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointGroupArn', 'describeEndpointGroup_endpointGroupArn' - The Amazon Resource Name (ARN) of the endpoint group to describe.
newDescribeEndpointGroup ::
  -- | 'endpointGroupArn'
  Prelude.Text ->
  DescribeEndpointGroup
newDescribeEndpointGroup pEndpointGroupArn_ =
  DescribeEndpointGroup'
    { endpointGroupArn =
        pEndpointGroupArn_
    }

-- | The Amazon Resource Name (ARN) of the endpoint group to describe.
describeEndpointGroup_endpointGroupArn :: Lens.Lens' DescribeEndpointGroup Prelude.Text
describeEndpointGroup_endpointGroupArn = Lens.lens (\DescribeEndpointGroup' {endpointGroupArn} -> endpointGroupArn) (\s@DescribeEndpointGroup' {} a -> s {endpointGroupArn = a} :: DescribeEndpointGroup)

instance Core.AWSRequest DescribeEndpointGroup where
  type
    AWSResponse DescribeEndpointGroup =
      DescribeEndpointGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEndpointGroupResponse'
            Prelude.<$> (x Data..?> "EndpointGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEndpointGroup where
  hashWithSalt _salt DescribeEndpointGroup' {..} =
    _salt `Prelude.hashWithSalt` endpointGroupArn

instance Prelude.NFData DescribeEndpointGroup where
  rnf DescribeEndpointGroup' {..} =
    Prelude.rnf endpointGroupArn

instance Data.ToHeaders DescribeEndpointGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GlobalAccelerator_V20180706.DescribeEndpointGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeEndpointGroup where
  toJSON DescribeEndpointGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("EndpointGroupArn" Data..= endpointGroupArn)
          ]
      )

instance Data.ToPath DescribeEndpointGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeEndpointGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEndpointGroupResponse' smart constructor.
data DescribeEndpointGroupResponse = DescribeEndpointGroupResponse'
  { -- | The description of an endpoint group.
    endpointGroup :: Prelude.Maybe EndpointGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEndpointGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointGroup', 'describeEndpointGroupResponse_endpointGroup' - The description of an endpoint group.
--
-- 'httpStatus', 'describeEndpointGroupResponse_httpStatus' - The response's http status code.
newDescribeEndpointGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEndpointGroupResponse
newDescribeEndpointGroupResponse pHttpStatus_ =
  DescribeEndpointGroupResponse'
    { endpointGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The description of an endpoint group.
describeEndpointGroupResponse_endpointGroup :: Lens.Lens' DescribeEndpointGroupResponse (Prelude.Maybe EndpointGroup)
describeEndpointGroupResponse_endpointGroup = Lens.lens (\DescribeEndpointGroupResponse' {endpointGroup} -> endpointGroup) (\s@DescribeEndpointGroupResponse' {} a -> s {endpointGroup = a} :: DescribeEndpointGroupResponse)

-- | The response's http status code.
describeEndpointGroupResponse_httpStatus :: Lens.Lens' DescribeEndpointGroupResponse Prelude.Int
describeEndpointGroupResponse_httpStatus = Lens.lens (\DescribeEndpointGroupResponse' {httpStatus} -> httpStatus) (\s@DescribeEndpointGroupResponse' {} a -> s {httpStatus = a} :: DescribeEndpointGroupResponse)

instance Prelude.NFData DescribeEndpointGroupResponse where
  rnf DescribeEndpointGroupResponse' {..} =
    Prelude.rnf endpointGroup `Prelude.seq`
      Prelude.rnf httpStatus
