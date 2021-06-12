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
-- Module      : Network.AWS.AutoScaling.DescribeTerminationPolicyTypes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the termination policies supported by Amazon EC2 Auto Scaling.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html Controlling which Auto Scaling instances terminate during scale in>
-- in the /Amazon EC2 Auto Scaling User Guide/.
module Network.AWS.AutoScaling.DescribeTerminationPolicyTypes
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

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeTerminationPolicyTypes' smart constructor.
data DescribeTerminationPolicyTypes = DescribeTerminationPolicyTypes'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeTerminationPolicyTypesResult"
      ( \s h x ->
          DescribeTerminationPolicyTypesResponse'
            Core.<$> ( x Core..@? "TerminationPolicyTypes"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeTerminationPolicyTypes

instance Core.NFData DescribeTerminationPolicyTypes

instance
  Core.ToHeaders
    DescribeTerminationPolicyTypes
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeTerminationPolicyTypes where
  toPath = Core.const "/"

instance Core.ToQuery DescribeTerminationPolicyTypes where
  toQuery =
    Core.const
      ( Core.mconcat
          [ "Action"
              Core.=: ( "DescribeTerminationPolicyTypes" ::
                          Core.ByteString
                      ),
            "Version" Core.=: ("2011-01-01" :: Core.ByteString)
          ]
      )

-- | /See:/ 'newDescribeTerminationPolicyTypesResponse' smart constructor.
data DescribeTerminationPolicyTypesResponse = DescribeTerminationPolicyTypesResponse'
  { -- | The termination policies supported by Amazon EC2 Auto Scaling:
    -- @OldestInstance@, @OldestLaunchConfiguration@, @NewestInstance@,
    -- @ClosestToNextInstanceHour@, @Default@, @OldestLaunchTemplate@, and
    -- @AllocationStrategy@.
    terminationPolicyTypes :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeTerminationPolicyTypesResponse
newDescribeTerminationPolicyTypesResponse
  pHttpStatus_ =
    DescribeTerminationPolicyTypesResponse'
      { terminationPolicyTypes =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The termination policies supported by Amazon EC2 Auto Scaling:
-- @OldestInstance@, @OldestLaunchConfiguration@, @NewestInstance@,
-- @ClosestToNextInstanceHour@, @Default@, @OldestLaunchTemplate@, and
-- @AllocationStrategy@.
describeTerminationPolicyTypesResponse_terminationPolicyTypes :: Lens.Lens' DescribeTerminationPolicyTypesResponse (Core.Maybe [Core.Text])
describeTerminationPolicyTypesResponse_terminationPolicyTypes = Lens.lens (\DescribeTerminationPolicyTypesResponse' {terminationPolicyTypes} -> terminationPolicyTypes) (\s@DescribeTerminationPolicyTypesResponse' {} a -> s {terminationPolicyTypes = a} :: DescribeTerminationPolicyTypesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeTerminationPolicyTypesResponse_httpStatus :: Lens.Lens' DescribeTerminationPolicyTypesResponse Core.Int
describeTerminationPolicyTypesResponse_httpStatus = Lens.lens (\DescribeTerminationPolicyTypesResponse' {httpStatus} -> httpStatus) (\s@DescribeTerminationPolicyTypesResponse' {} a -> s {httpStatus = a} :: DescribeTerminationPolicyTypesResponse)

instance
  Core.NFData
    DescribeTerminationPolicyTypesResponse
