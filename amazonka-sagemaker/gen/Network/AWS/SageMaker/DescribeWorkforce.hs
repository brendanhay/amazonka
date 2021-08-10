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
-- Module      : Network.AWS.SageMaker.DescribeWorkforce
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists private workforce information, including workforce name, Amazon
-- Resource Name (ARN), and, if applicable, allowed IP address ranges
-- (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs>).
-- Allowable IP address ranges are the IP addresses that workers can use to
-- access tasks.
--
-- This operation applies only to private workforces.
module Network.AWS.SageMaker.DescribeWorkforce
  ( -- * Creating a Request
    DescribeWorkforce (..),
    newDescribeWorkforce,

    -- * Request Lenses
    describeWorkforce_workforceName,

    -- * Destructuring the Response
    DescribeWorkforceResponse (..),
    newDescribeWorkforceResponse,

    -- * Response Lenses
    describeWorkforceResponse_httpStatus,
    describeWorkforceResponse_workforce,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeWorkforce' smart constructor.
data DescribeWorkforce = DescribeWorkforce'
  { -- | The name of the private workforce whose access you want to restrict.
    -- @WorkforceName@ is automatically set to @default@ when a workforce is
    -- created and cannot be modified.
    workforceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWorkforce' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workforceName', 'describeWorkforce_workforceName' - The name of the private workforce whose access you want to restrict.
-- @WorkforceName@ is automatically set to @default@ when a workforce is
-- created and cannot be modified.
newDescribeWorkforce ::
  -- | 'workforceName'
  Prelude.Text ->
  DescribeWorkforce
newDescribeWorkforce pWorkforceName_ =
  DescribeWorkforce' {workforceName = pWorkforceName_}

-- | The name of the private workforce whose access you want to restrict.
-- @WorkforceName@ is automatically set to @default@ when a workforce is
-- created and cannot be modified.
describeWorkforce_workforceName :: Lens.Lens' DescribeWorkforce Prelude.Text
describeWorkforce_workforceName = Lens.lens (\DescribeWorkforce' {workforceName} -> workforceName) (\s@DescribeWorkforce' {} a -> s {workforceName = a} :: DescribeWorkforce)

instance Core.AWSRequest DescribeWorkforce where
  type
    AWSResponse DescribeWorkforce =
      DescribeWorkforceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWorkforceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "Workforce")
      )

instance Prelude.Hashable DescribeWorkforce

instance Prelude.NFData DescribeWorkforce

instance Core.ToHeaders DescribeWorkforce where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribeWorkforce" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeWorkforce where
  toJSON DescribeWorkforce' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("WorkforceName" Core..= workforceName)
          ]
      )

instance Core.ToPath DescribeWorkforce where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeWorkforce where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeWorkforceResponse' smart constructor.
data DescribeWorkforceResponse = DescribeWorkforceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A single private workforce, which is automatically created when you
    -- create your first private work team. You can create one private work
    -- force in each AWS Region. By default, any workforce-related API
    -- operation used in a specific region will apply to the workforce created
    -- in that region. To learn how to create a private workforce, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-create-private.html Create a Private Workforce>.
    workforce :: Workforce
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWorkforceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeWorkforceResponse_httpStatus' - The response's http status code.
--
-- 'workforce', 'describeWorkforceResponse_workforce' - A single private workforce, which is automatically created when you
-- create your first private work team. You can create one private work
-- force in each AWS Region. By default, any workforce-related API
-- operation used in a specific region will apply to the workforce created
-- in that region. To learn how to create a private workforce, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-create-private.html Create a Private Workforce>.
newDescribeWorkforceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'workforce'
  Workforce ->
  DescribeWorkforceResponse
newDescribeWorkforceResponse pHttpStatus_ pWorkforce_ =
  DescribeWorkforceResponse'
    { httpStatus =
        pHttpStatus_,
      workforce = pWorkforce_
    }

-- | The response's http status code.
describeWorkforceResponse_httpStatus :: Lens.Lens' DescribeWorkforceResponse Prelude.Int
describeWorkforceResponse_httpStatus = Lens.lens (\DescribeWorkforceResponse' {httpStatus} -> httpStatus) (\s@DescribeWorkforceResponse' {} a -> s {httpStatus = a} :: DescribeWorkforceResponse)

-- | A single private workforce, which is automatically created when you
-- create your first private work team. You can create one private work
-- force in each AWS Region. By default, any workforce-related API
-- operation used in a specific region will apply to the workforce created
-- in that region. To learn how to create a private workforce, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-create-private.html Create a Private Workforce>.
describeWorkforceResponse_workforce :: Lens.Lens' DescribeWorkforceResponse Workforce
describeWorkforceResponse_workforce = Lens.lens (\DescribeWorkforceResponse' {workforce} -> workforce) (\s@DescribeWorkforceResponse' {} a -> s {workforce = a} :: DescribeWorkforceResponse)

instance Prelude.NFData DescribeWorkforceResponse
