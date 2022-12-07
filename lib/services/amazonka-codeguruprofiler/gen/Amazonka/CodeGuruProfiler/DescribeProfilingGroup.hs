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
-- Module      : Amazonka.CodeGuruProfiler.DescribeProfilingGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ProfilingGroupDescription.html ProfilingGroupDescription>
-- object that contains information about the requested profiling group.
module Amazonka.CodeGuruProfiler.DescribeProfilingGroup
  ( -- * Creating a Request
    DescribeProfilingGroup (..),
    newDescribeProfilingGroup,

    -- * Request Lenses
    describeProfilingGroup_profilingGroupName,

    -- * Destructuring the Response
    DescribeProfilingGroupResponse (..),
    newDescribeProfilingGroupResponse,

    -- * Response Lenses
    describeProfilingGroupResponse_httpStatus,
    describeProfilingGroupResponse_profilingGroup,
  )
where

import Amazonka.CodeGuruProfiler.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The structure representing the describeProfilingGroupRequest.
--
-- /See:/ 'newDescribeProfilingGroup' smart constructor.
data DescribeProfilingGroup = DescribeProfilingGroup'
  { -- | The name of the profiling group to get information about.
    profilingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProfilingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profilingGroupName', 'describeProfilingGroup_profilingGroupName' - The name of the profiling group to get information about.
newDescribeProfilingGroup ::
  -- | 'profilingGroupName'
  Prelude.Text ->
  DescribeProfilingGroup
newDescribeProfilingGroup pProfilingGroupName_ =
  DescribeProfilingGroup'
    { profilingGroupName =
        pProfilingGroupName_
    }

-- | The name of the profiling group to get information about.
describeProfilingGroup_profilingGroupName :: Lens.Lens' DescribeProfilingGroup Prelude.Text
describeProfilingGroup_profilingGroupName = Lens.lens (\DescribeProfilingGroup' {profilingGroupName} -> profilingGroupName) (\s@DescribeProfilingGroup' {} a -> s {profilingGroupName = a} :: DescribeProfilingGroup)

instance Core.AWSRequest DescribeProfilingGroup where
  type
    AWSResponse DescribeProfilingGroup =
      DescribeProfilingGroupResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProfilingGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable DescribeProfilingGroup where
  hashWithSalt _salt DescribeProfilingGroup' {..} =
    _salt `Prelude.hashWithSalt` profilingGroupName

instance Prelude.NFData DescribeProfilingGroup where
  rnf DescribeProfilingGroup' {..} =
    Prelude.rnf profilingGroupName

instance Data.ToHeaders DescribeProfilingGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeProfilingGroup where
  toPath DescribeProfilingGroup' {..} =
    Prelude.mconcat
      ["/profilingGroups/", Data.toBS profilingGroupName]

instance Data.ToQuery DescribeProfilingGroup where
  toQuery = Prelude.const Prelude.mempty

-- | The structure representing the describeProfilingGroupResponse.
--
-- /See:/ 'newDescribeProfilingGroupResponse' smart constructor.
data DescribeProfilingGroupResponse = DescribeProfilingGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The returned
    -- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ProfilingGroupDescription.html ProfilingGroupDescription>
    -- object that contains information about the requested profiling group.
    profilingGroup :: ProfilingGroupDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProfilingGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeProfilingGroupResponse_httpStatus' - The response's http status code.
--
-- 'profilingGroup', 'describeProfilingGroupResponse_profilingGroup' - The returned
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ProfilingGroupDescription.html ProfilingGroupDescription>
-- object that contains information about the requested profiling group.
newDescribeProfilingGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'profilingGroup'
  ProfilingGroupDescription ->
  DescribeProfilingGroupResponse
newDescribeProfilingGroupResponse
  pHttpStatus_
  pProfilingGroup_ =
    DescribeProfilingGroupResponse'
      { httpStatus =
          pHttpStatus_,
        profilingGroup = pProfilingGroup_
      }

-- | The response's http status code.
describeProfilingGroupResponse_httpStatus :: Lens.Lens' DescribeProfilingGroupResponse Prelude.Int
describeProfilingGroupResponse_httpStatus = Lens.lens (\DescribeProfilingGroupResponse' {httpStatus} -> httpStatus) (\s@DescribeProfilingGroupResponse' {} a -> s {httpStatus = a} :: DescribeProfilingGroupResponse)

-- | The returned
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ProfilingGroupDescription.html ProfilingGroupDescription>
-- object that contains information about the requested profiling group.
describeProfilingGroupResponse_profilingGroup :: Lens.Lens' DescribeProfilingGroupResponse ProfilingGroupDescription
describeProfilingGroupResponse_profilingGroup = Lens.lens (\DescribeProfilingGroupResponse' {profilingGroup} -> profilingGroup) (\s@DescribeProfilingGroupResponse' {} a -> s {profilingGroup = a} :: DescribeProfilingGroupResponse)

instance
  Prelude.NFData
    DescribeProfilingGroupResponse
  where
  rnf DescribeProfilingGroupResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf profilingGroup
