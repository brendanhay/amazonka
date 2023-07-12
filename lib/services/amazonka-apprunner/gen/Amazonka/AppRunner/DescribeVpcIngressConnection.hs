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
-- Module      : Amazonka.AppRunner.DescribeVpcIngressConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Return a full description of an App Runner VPC Ingress Connection
-- resource.
module Amazonka.AppRunner.DescribeVpcIngressConnection
  ( -- * Creating a Request
    DescribeVpcIngressConnection (..),
    newDescribeVpcIngressConnection,

    -- * Request Lenses
    describeVpcIngressConnection_vpcIngressConnectionArn,

    -- * Destructuring the Response
    DescribeVpcIngressConnectionResponse (..),
    newDescribeVpcIngressConnectionResponse,

    -- * Response Lenses
    describeVpcIngressConnectionResponse_httpStatus,
    describeVpcIngressConnectionResponse_vpcIngressConnection,
  )
where

import Amazonka.AppRunner.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeVpcIngressConnection' smart constructor.
data DescribeVpcIngressConnection = DescribeVpcIngressConnection'
  { -- | The Amazon Resource Name (ARN) of the App Runner VPC Ingress Connection
    -- that you want a description for.
    vpcIngressConnectionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVpcIngressConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcIngressConnectionArn', 'describeVpcIngressConnection_vpcIngressConnectionArn' - The Amazon Resource Name (ARN) of the App Runner VPC Ingress Connection
-- that you want a description for.
newDescribeVpcIngressConnection ::
  -- | 'vpcIngressConnectionArn'
  Prelude.Text ->
  DescribeVpcIngressConnection
newDescribeVpcIngressConnection
  pVpcIngressConnectionArn_ =
    DescribeVpcIngressConnection'
      { vpcIngressConnectionArn =
          pVpcIngressConnectionArn_
      }

-- | The Amazon Resource Name (ARN) of the App Runner VPC Ingress Connection
-- that you want a description for.
describeVpcIngressConnection_vpcIngressConnectionArn :: Lens.Lens' DescribeVpcIngressConnection Prelude.Text
describeVpcIngressConnection_vpcIngressConnectionArn = Lens.lens (\DescribeVpcIngressConnection' {vpcIngressConnectionArn} -> vpcIngressConnectionArn) (\s@DescribeVpcIngressConnection' {} a -> s {vpcIngressConnectionArn = a} :: DescribeVpcIngressConnection)

instance Core.AWSRequest DescribeVpcIngressConnection where
  type
    AWSResponse DescribeVpcIngressConnection =
      DescribeVpcIngressConnectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeVpcIngressConnectionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "VpcIngressConnection")
      )

instance
  Prelude.Hashable
    DescribeVpcIngressConnection
  where
  hashWithSalt _salt DescribeVpcIngressConnection' {..} =
    _salt
      `Prelude.hashWithSalt` vpcIngressConnectionArn

instance Prelude.NFData DescribeVpcIngressConnection where
  rnf DescribeVpcIngressConnection' {..} =
    Prelude.rnf vpcIngressConnectionArn

instance Data.ToHeaders DescribeVpcIngressConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AppRunner.DescribeVpcIngressConnection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeVpcIngressConnection where
  toJSON DescribeVpcIngressConnection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "VpcIngressConnectionArn"
                  Data..= vpcIngressConnectionArn
              )
          ]
      )

instance Data.ToPath DescribeVpcIngressConnection where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeVpcIngressConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeVpcIngressConnectionResponse' smart constructor.
data DescribeVpcIngressConnectionResponse = DescribeVpcIngressConnectionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A description of the App Runner VPC Ingress Connection that you
    -- specified in this request.
    vpcIngressConnection :: VpcIngressConnection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVpcIngressConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeVpcIngressConnectionResponse_httpStatus' - The response's http status code.
--
-- 'vpcIngressConnection', 'describeVpcIngressConnectionResponse_vpcIngressConnection' - A description of the App Runner VPC Ingress Connection that you
-- specified in this request.
newDescribeVpcIngressConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'vpcIngressConnection'
  VpcIngressConnection ->
  DescribeVpcIngressConnectionResponse
newDescribeVpcIngressConnectionResponse
  pHttpStatus_
  pVpcIngressConnection_ =
    DescribeVpcIngressConnectionResponse'
      { httpStatus =
          pHttpStatus_,
        vpcIngressConnection =
          pVpcIngressConnection_
      }

-- | The response's http status code.
describeVpcIngressConnectionResponse_httpStatus :: Lens.Lens' DescribeVpcIngressConnectionResponse Prelude.Int
describeVpcIngressConnectionResponse_httpStatus = Lens.lens (\DescribeVpcIngressConnectionResponse' {httpStatus} -> httpStatus) (\s@DescribeVpcIngressConnectionResponse' {} a -> s {httpStatus = a} :: DescribeVpcIngressConnectionResponse)

-- | A description of the App Runner VPC Ingress Connection that you
-- specified in this request.
describeVpcIngressConnectionResponse_vpcIngressConnection :: Lens.Lens' DescribeVpcIngressConnectionResponse VpcIngressConnection
describeVpcIngressConnectionResponse_vpcIngressConnection = Lens.lens (\DescribeVpcIngressConnectionResponse' {vpcIngressConnection} -> vpcIngressConnection) (\s@DescribeVpcIngressConnectionResponse' {} a -> s {vpcIngressConnection = a} :: DescribeVpcIngressConnectionResponse)

instance
  Prelude.NFData
    DescribeVpcIngressConnectionResponse
  where
  rnf DescribeVpcIngressConnectionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf vpcIngressConnection
