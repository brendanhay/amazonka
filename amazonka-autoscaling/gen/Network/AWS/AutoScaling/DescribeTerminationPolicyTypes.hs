{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeTerminationPolicyTypes' smart constructor.
data DescribeTerminationPolicyTypes = DescribeTerminationPolicyTypes'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeTerminationPolicyTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeTerminationPolicyTypes ::
  DescribeTerminationPolicyTypes
newDescribeTerminationPolicyTypes =
  DescribeTerminationPolicyTypes'

instance
  Prelude.AWSRequest
    DescribeTerminationPolicyTypes
  where
  type
    Rs DescribeTerminationPolicyTypes =
      DescribeTerminationPolicyTypesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeTerminationPolicyTypesResult"
      ( \s h x ->
          DescribeTerminationPolicyTypesResponse'
            Prelude.<$> ( x Prelude..@? "TerminationPolicyTypes"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeTerminationPolicyTypes

instance
  Prelude.NFData
    DescribeTerminationPolicyTypes

instance
  Prelude.ToHeaders
    DescribeTerminationPolicyTypes
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    DescribeTerminationPolicyTypes
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DescribeTerminationPolicyTypes
  where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          [ "Action"
              Prelude.=: ( "DescribeTerminationPolicyTypes" ::
                             Prelude.ByteString
                         ),
            "Version"
              Prelude.=: ("2011-01-01" :: Prelude.ByteString)
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
describeTerminationPolicyTypesResponse_terminationPolicyTypes = Lens.lens (\DescribeTerminationPolicyTypesResponse' {terminationPolicyTypes} -> terminationPolicyTypes) (\s@DescribeTerminationPolicyTypesResponse' {} a -> s {terminationPolicyTypes = a} :: DescribeTerminationPolicyTypesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeTerminationPolicyTypesResponse_httpStatus :: Lens.Lens' DescribeTerminationPolicyTypesResponse Prelude.Int
describeTerminationPolicyTypesResponse_httpStatus = Lens.lens (\DescribeTerminationPolicyTypesResponse' {httpStatus} -> httpStatus) (\s@DescribeTerminationPolicyTypesResponse' {} a -> s {httpStatus = a} :: DescribeTerminationPolicyTypesResponse)

instance
  Prelude.NFData
    DescribeTerminationPolicyTypesResponse
