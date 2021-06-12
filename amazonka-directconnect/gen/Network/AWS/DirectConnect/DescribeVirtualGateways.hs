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
-- Module      : Network.AWS.DirectConnect.DescribeVirtualGateways
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the virtual private gateways owned by the AWS account.
--
-- You can create one or more AWS Direct Connect private virtual interfaces
-- linked to a virtual private gateway.
module Network.AWS.DirectConnect.DescribeVirtualGateways
  ( -- * Creating a Request
    DescribeVirtualGateways (..),
    newDescribeVirtualGateways,

    -- * Destructuring the Response
    DescribeVirtualGatewaysResponse (..),
    newDescribeVirtualGatewaysResponse,

    -- * Response Lenses
    describeVirtualGatewaysResponse_virtualGateways,
    describeVirtualGatewaysResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeVirtualGateways' smart constructor.
data DescribeVirtualGateways = DescribeVirtualGateways'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeVirtualGateways' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeVirtualGateways ::
  DescribeVirtualGateways
newDescribeVirtualGateways = DescribeVirtualGateways'

instance Core.AWSRequest DescribeVirtualGateways where
  type
    AWSResponse DescribeVirtualGateways =
      DescribeVirtualGatewaysResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeVirtualGatewaysResponse'
            Core.<$> (x Core..?> "virtualGateways" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeVirtualGateways

instance Core.NFData DescribeVirtualGateways

instance Core.ToHeaders DescribeVirtualGateways where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.DescribeVirtualGateways" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeVirtualGateways where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath DescribeVirtualGateways where
  toPath = Core.const "/"

instance Core.ToQuery DescribeVirtualGateways where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeVirtualGatewaysResponse' smart constructor.
data DescribeVirtualGatewaysResponse = DescribeVirtualGatewaysResponse'
  { -- | The virtual private gateways.
    virtualGateways :: Core.Maybe [VirtualGateway],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeVirtualGatewaysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualGateways', 'describeVirtualGatewaysResponse_virtualGateways' - The virtual private gateways.
--
-- 'httpStatus', 'describeVirtualGatewaysResponse_httpStatus' - The response's http status code.
newDescribeVirtualGatewaysResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeVirtualGatewaysResponse
newDescribeVirtualGatewaysResponse pHttpStatus_ =
  DescribeVirtualGatewaysResponse'
    { virtualGateways =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The virtual private gateways.
describeVirtualGatewaysResponse_virtualGateways :: Lens.Lens' DescribeVirtualGatewaysResponse (Core.Maybe [VirtualGateway])
describeVirtualGatewaysResponse_virtualGateways = Lens.lens (\DescribeVirtualGatewaysResponse' {virtualGateways} -> virtualGateways) (\s@DescribeVirtualGatewaysResponse' {} a -> s {virtualGateways = a} :: DescribeVirtualGatewaysResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeVirtualGatewaysResponse_httpStatus :: Lens.Lens' DescribeVirtualGatewaysResponse Core.Int
describeVirtualGatewaysResponse_httpStatus = Lens.lens (\DescribeVirtualGatewaysResponse' {httpStatus} -> httpStatus) (\s@DescribeVirtualGatewaysResponse' {} a -> s {httpStatus = a} :: DescribeVirtualGatewaysResponse)

instance Core.NFData DescribeVirtualGatewaysResponse
