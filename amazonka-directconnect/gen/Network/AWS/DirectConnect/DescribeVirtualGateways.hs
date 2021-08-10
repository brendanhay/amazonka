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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeVirtualGateways' smart constructor.
data DescribeVirtualGateways = DescribeVirtualGateways'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
            Prelude.<$> ( x Core..?> "virtualGateways"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeVirtualGateways

instance Prelude.NFData DescribeVirtualGateways

instance Core.ToHeaders DescribeVirtualGateways where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.DescribeVirtualGateways" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeVirtualGateways where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath DescribeVirtualGateways where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeVirtualGateways where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeVirtualGatewaysResponse' smart constructor.
data DescribeVirtualGatewaysResponse = DescribeVirtualGatewaysResponse'
  { -- | The virtual private gateways.
    virtualGateways :: Prelude.Maybe [VirtualGateway],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeVirtualGatewaysResponse
newDescribeVirtualGatewaysResponse pHttpStatus_ =
  DescribeVirtualGatewaysResponse'
    { virtualGateways =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The virtual private gateways.
describeVirtualGatewaysResponse_virtualGateways :: Lens.Lens' DescribeVirtualGatewaysResponse (Prelude.Maybe [VirtualGateway])
describeVirtualGatewaysResponse_virtualGateways = Lens.lens (\DescribeVirtualGatewaysResponse' {virtualGateways} -> virtualGateways) (\s@DescribeVirtualGatewaysResponse' {} a -> s {virtualGateways = a} :: DescribeVirtualGatewaysResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeVirtualGatewaysResponse_httpStatus :: Lens.Lens' DescribeVirtualGatewaysResponse Prelude.Int
describeVirtualGatewaysResponse_httpStatus = Lens.lens (\DescribeVirtualGatewaysResponse' {httpStatus} -> httpStatus) (\s@DescribeVirtualGatewaysResponse' {} a -> s {httpStatus = a} :: DescribeVirtualGatewaysResponse)

instance
  Prelude.NFData
    DescribeVirtualGatewaysResponse
