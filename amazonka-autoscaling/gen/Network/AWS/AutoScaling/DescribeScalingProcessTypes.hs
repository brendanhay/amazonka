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
-- Module      : Network.AWS.AutoScaling.DescribeScalingProcessTypes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the scaling process types for use with the ResumeProcesses and
-- SuspendProcesses APIs.
module Network.AWS.AutoScaling.DescribeScalingProcessTypes
  ( -- * Creating a Request
    DescribeScalingProcessTypes (..),
    newDescribeScalingProcessTypes,

    -- * Destructuring the Response
    DescribeScalingProcessTypesResponse (..),
    newDescribeScalingProcessTypesResponse,

    -- * Response Lenses
    describeScalingProcessTypesResponse_processes,
    describeScalingProcessTypesResponse_httpStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeScalingProcessTypes' smart constructor.
data DescribeScalingProcessTypes = DescribeScalingProcessTypes'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeScalingProcessTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeScalingProcessTypes ::
  DescribeScalingProcessTypes
newDescribeScalingProcessTypes =
  DescribeScalingProcessTypes'

instance Core.AWSRequest DescribeScalingProcessTypes where
  type
    AWSResponse DescribeScalingProcessTypes =
      DescribeScalingProcessTypesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeScalingProcessTypesResult"
      ( \s h x ->
          DescribeScalingProcessTypesResponse'
            Core.<$> ( x Core..@? "Processes" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeScalingProcessTypes

instance Core.NFData DescribeScalingProcessTypes

instance Core.ToHeaders DescribeScalingProcessTypes where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeScalingProcessTypes where
  toPath = Core.const "/"

instance Core.ToQuery DescribeScalingProcessTypes where
  toQuery =
    Core.const
      ( Core.mconcat
          [ "Action"
              Core.=: ("DescribeScalingProcessTypes" :: Core.ByteString),
            "Version" Core.=: ("2011-01-01" :: Core.ByteString)
          ]
      )

-- | /See:/ 'newDescribeScalingProcessTypesResponse' smart constructor.
data DescribeScalingProcessTypesResponse = DescribeScalingProcessTypesResponse'
  { -- | The names of the process types.
    processes :: Core.Maybe [ProcessType],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeScalingProcessTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'processes', 'describeScalingProcessTypesResponse_processes' - The names of the process types.
--
-- 'httpStatus', 'describeScalingProcessTypesResponse_httpStatus' - The response's http status code.
newDescribeScalingProcessTypesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeScalingProcessTypesResponse
newDescribeScalingProcessTypesResponse pHttpStatus_ =
  DescribeScalingProcessTypesResponse'
    { processes =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The names of the process types.
describeScalingProcessTypesResponse_processes :: Lens.Lens' DescribeScalingProcessTypesResponse (Core.Maybe [ProcessType])
describeScalingProcessTypesResponse_processes = Lens.lens (\DescribeScalingProcessTypesResponse' {processes} -> processes) (\s@DescribeScalingProcessTypesResponse' {} a -> s {processes = a} :: DescribeScalingProcessTypesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeScalingProcessTypesResponse_httpStatus :: Lens.Lens' DescribeScalingProcessTypesResponse Core.Int
describeScalingProcessTypesResponse_httpStatus = Lens.lens (\DescribeScalingProcessTypesResponse' {httpStatus} -> httpStatus) (\s@DescribeScalingProcessTypesResponse' {} a -> s {httpStatus = a} :: DescribeScalingProcessTypesResponse)

instance
  Core.NFData
    DescribeScalingProcessTypesResponse
