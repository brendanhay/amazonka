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
-- Module      : Amazonka.AutoScaling.DescribeTerminationPolicyTypes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the termination policies supported by Amazon EC2 Auto Scaling.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-termination-policies.html Work with Amazon EC2 Auto Scaling termination policies>
-- in the /Amazon EC2 Auto Scaling User Guide/.
module Amazonka.AutoScaling.DescribeTerminationPolicyTypes
  ( -- * Creating a Request
    DescribeTerminationPolicyTypes (..),
    newDescribeTerminationPolicyTypes,

    -- * Destructuring the Response
    DescribeTerminationPolicyTypesResponse (..),
    newDescribeTerminationPolicyTypesResponse,

    -- * Response Lenses
    describeTerminationPolicyTypesResponse_terminationPolicyTypes,
    describeTerminationPolicyTypesResponse_httpStatus,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTerminationPolicyTypes' smart constructor.
data DescribeTerminationPolicyTypes = DescribeTerminationPolicyTypes'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTerminationPolicyTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeTerminationPolicyTypes ::
  DescribeTerminationPolicyTypes
newDescribeTerminationPolicyTypes =
  DescribeTerminationPolicyTypes'

instance
  Core.AWSRequest
    DescribeTerminationPolicyTypes
  where
  type
    AWSResponse DescribeTerminationPolicyTypes =
      DescribeTerminationPolicyTypesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeTerminationPolicyTypesResult"
      ( \s h x ->
          DescribeTerminationPolicyTypesResponse'
            Prelude.<$> ( x
                            Data..@? "TerminationPolicyTypes"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeTerminationPolicyTypes
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    DescribeTerminationPolicyTypes
  where
  rnf _ = ()

instance
  Data.ToHeaders
    DescribeTerminationPolicyTypes
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeTerminationPolicyTypes where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeTerminationPolicyTypes where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          [ "Action"
              Data.=: ( "DescribeTerminationPolicyTypes" ::
                          Prelude.ByteString
                      ),
            "Version"
              Data.=: ("2011-01-01" :: Prelude.ByteString)
          ]
      )

-- | /See:/ 'newDescribeTerminationPolicyTypesResponse' smart constructor.
data DescribeTerminationPolicyTypesResponse = DescribeTerminationPolicyTypesResponse'
  { -- | The termination policies supported by Amazon EC2 Auto Scaling:
    -- @OldestInstance@, @OldestLaunchConfiguration@, @NewestInstance@,
    -- @ClosestToNextInstanceHour@, @Default@, @OldestLaunchTemplate@, and
    -- @AllocationStrategy@.
    terminationPolicyTypes :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTerminationPolicyTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'terminationPolicyTypes', 'describeTerminationPolicyTypesResponse_terminationPolicyTypes' - The termination policies supported by Amazon EC2 Auto Scaling:
-- @OldestInstance@, @OldestLaunchConfiguration@, @NewestInstance@,
-- @ClosestToNextInstanceHour@, @Default@, @OldestLaunchTemplate@, and
-- @AllocationStrategy@.
--
-- 'httpStatus', 'describeTerminationPolicyTypesResponse_httpStatus' - The response's http status code.
newDescribeTerminationPolicyTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTerminationPolicyTypesResponse
newDescribeTerminationPolicyTypesResponse
  pHttpStatus_ =
    DescribeTerminationPolicyTypesResponse'
      { terminationPolicyTypes =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The termination policies supported by Amazon EC2 Auto Scaling:
-- @OldestInstance@, @OldestLaunchConfiguration@, @NewestInstance@,
-- @ClosestToNextInstanceHour@, @Default@, @OldestLaunchTemplate@, and
-- @AllocationStrategy@.
describeTerminationPolicyTypesResponse_terminationPolicyTypes :: Lens.Lens' DescribeTerminationPolicyTypesResponse (Prelude.Maybe [Prelude.Text])
describeTerminationPolicyTypesResponse_terminationPolicyTypes = Lens.lens (\DescribeTerminationPolicyTypesResponse' {terminationPolicyTypes} -> terminationPolicyTypes) (\s@DescribeTerminationPolicyTypesResponse' {} a -> s {terminationPolicyTypes = a} :: DescribeTerminationPolicyTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeTerminationPolicyTypesResponse_httpStatus :: Lens.Lens' DescribeTerminationPolicyTypesResponse Prelude.Int
describeTerminationPolicyTypesResponse_httpStatus = Lens.lens (\DescribeTerminationPolicyTypesResponse' {httpStatus} -> httpStatus) (\s@DescribeTerminationPolicyTypesResponse' {} a -> s {httpStatus = a} :: DescribeTerminationPolicyTypesResponse)

instance
  Prelude.NFData
    DescribeTerminationPolicyTypesResponse
  where
  rnf DescribeTerminationPolicyTypesResponse' {..} =
    Prelude.rnf terminationPolicyTypes `Prelude.seq`
      Prelude.rnf httpStatus
