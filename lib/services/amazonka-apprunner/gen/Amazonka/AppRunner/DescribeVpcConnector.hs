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
-- Module      : Amazonka.AppRunner.DescribeVpcConnector
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Return a description of an App Runner VPC connector resource.
module Amazonka.AppRunner.DescribeVpcConnector
  ( -- * Creating a Request
    DescribeVpcConnector (..),
    newDescribeVpcConnector,

    -- * Request Lenses
    describeVpcConnector_vpcConnectorArn,

    -- * Destructuring the Response
    DescribeVpcConnectorResponse (..),
    newDescribeVpcConnectorResponse,

    -- * Response Lenses
    describeVpcConnectorResponse_httpStatus,
    describeVpcConnectorResponse_vpcConnector,
  )
where

import Amazonka.AppRunner.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeVpcConnector' smart constructor.
data DescribeVpcConnector = DescribeVpcConnector'
  { -- | The Amazon Resource Name (ARN) of the App Runner VPC connector that you
    -- want a description for.
    --
    -- The ARN must be a full VPC connector ARN.
    vpcConnectorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVpcConnector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConnectorArn', 'describeVpcConnector_vpcConnectorArn' - The Amazon Resource Name (ARN) of the App Runner VPC connector that you
-- want a description for.
--
-- The ARN must be a full VPC connector ARN.
newDescribeVpcConnector ::
  -- | 'vpcConnectorArn'
  Prelude.Text ->
  DescribeVpcConnector
newDescribeVpcConnector pVpcConnectorArn_ =
  DescribeVpcConnector'
    { vpcConnectorArn =
        pVpcConnectorArn_
    }

-- | The Amazon Resource Name (ARN) of the App Runner VPC connector that you
-- want a description for.
--
-- The ARN must be a full VPC connector ARN.
describeVpcConnector_vpcConnectorArn :: Lens.Lens' DescribeVpcConnector Prelude.Text
describeVpcConnector_vpcConnectorArn = Lens.lens (\DescribeVpcConnector' {vpcConnectorArn} -> vpcConnectorArn) (\s@DescribeVpcConnector' {} a -> s {vpcConnectorArn = a} :: DescribeVpcConnector)

instance Core.AWSRequest DescribeVpcConnector where
  type
    AWSResponse DescribeVpcConnector =
      DescribeVpcConnectorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeVpcConnectorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "VpcConnector")
      )

instance Prelude.Hashable DescribeVpcConnector where
  hashWithSalt _salt DescribeVpcConnector' {..} =
    _salt `Prelude.hashWithSalt` vpcConnectorArn

instance Prelude.NFData DescribeVpcConnector where
  rnf DescribeVpcConnector' {..} =
    Prelude.rnf vpcConnectorArn

instance Data.ToHeaders DescribeVpcConnector where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AppRunner.DescribeVpcConnector" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeVpcConnector where
  toJSON DescribeVpcConnector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("VpcConnectorArn" Data..= vpcConnectorArn)
          ]
      )

instance Data.ToPath DescribeVpcConnector where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeVpcConnector where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeVpcConnectorResponse' smart constructor.
data DescribeVpcConnectorResponse = DescribeVpcConnectorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A description of the App Runner VPC connector that you specified in this
    -- request.
    vpcConnector :: VpcConnector
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVpcConnectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeVpcConnectorResponse_httpStatus' - The response's http status code.
--
-- 'vpcConnector', 'describeVpcConnectorResponse_vpcConnector' - A description of the App Runner VPC connector that you specified in this
-- request.
newDescribeVpcConnectorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'vpcConnector'
  VpcConnector ->
  DescribeVpcConnectorResponse
newDescribeVpcConnectorResponse
  pHttpStatus_
  pVpcConnector_ =
    DescribeVpcConnectorResponse'
      { httpStatus =
          pHttpStatus_,
        vpcConnector = pVpcConnector_
      }

-- | The response's http status code.
describeVpcConnectorResponse_httpStatus :: Lens.Lens' DescribeVpcConnectorResponse Prelude.Int
describeVpcConnectorResponse_httpStatus = Lens.lens (\DescribeVpcConnectorResponse' {httpStatus} -> httpStatus) (\s@DescribeVpcConnectorResponse' {} a -> s {httpStatus = a} :: DescribeVpcConnectorResponse)

-- | A description of the App Runner VPC connector that you specified in this
-- request.
describeVpcConnectorResponse_vpcConnector :: Lens.Lens' DescribeVpcConnectorResponse VpcConnector
describeVpcConnectorResponse_vpcConnector = Lens.lens (\DescribeVpcConnectorResponse' {vpcConnector} -> vpcConnector) (\s@DescribeVpcConnectorResponse' {} a -> s {vpcConnector = a} :: DescribeVpcConnectorResponse)

instance Prelude.NFData DescribeVpcConnectorResponse where
  rnf DescribeVpcConnectorResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf vpcConnector
