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
-- Module      : Network.AWS.SageMaker.DescribeWorkteam
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specific work team. You can see information
-- such as the create date, the last updated date, membership information,
-- and the work team\'s Amazon Resource Name (ARN).
module Network.AWS.SageMaker.DescribeWorkteam
  ( -- * Creating a Request
    DescribeWorkteam (..),
    newDescribeWorkteam,

    -- * Request Lenses
    describeWorkteam_workteamName,

    -- * Destructuring the Response
    DescribeWorkteamResponse (..),
    newDescribeWorkteamResponse,

    -- * Response Lenses
    describeWorkteamResponse_httpStatus,
    describeWorkteamResponse_workteam,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeWorkteam' smart constructor.
data DescribeWorkteam = DescribeWorkteam'
  { -- | The name of the work team to return a description of.
    workteamName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeWorkteam' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workteamName', 'describeWorkteam_workteamName' - The name of the work team to return a description of.
newDescribeWorkteam ::
  -- | 'workteamName'
  Core.Text ->
  DescribeWorkteam
newDescribeWorkteam pWorkteamName_ =
  DescribeWorkteam' {workteamName = pWorkteamName_}

-- | The name of the work team to return a description of.
describeWorkteam_workteamName :: Lens.Lens' DescribeWorkteam Core.Text
describeWorkteam_workteamName = Lens.lens (\DescribeWorkteam' {workteamName} -> workteamName) (\s@DescribeWorkteam' {} a -> s {workteamName = a} :: DescribeWorkteam)

instance Core.AWSRequest DescribeWorkteam where
  type
    AWSResponse DescribeWorkteam =
      DescribeWorkteamResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWorkteamResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "Workteam")
      )

instance Core.Hashable DescribeWorkteam

instance Core.NFData DescribeWorkteam

instance Core.ToHeaders DescribeWorkteam where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DescribeWorkteam" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeWorkteam where
  toJSON DescribeWorkteam' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("WorkteamName" Core..= workteamName)]
      )

instance Core.ToPath DescribeWorkteam where
  toPath = Core.const "/"

instance Core.ToQuery DescribeWorkteam where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeWorkteamResponse' smart constructor.
data DescribeWorkteamResponse = DescribeWorkteamResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A @Workteam@ instance that contains information about the work team.
    workteam :: Workteam
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeWorkteamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeWorkteamResponse_httpStatus' - The response's http status code.
--
-- 'workteam', 'describeWorkteamResponse_workteam' - A @Workteam@ instance that contains information about the work team.
newDescribeWorkteamResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'workteam'
  Workteam ->
  DescribeWorkteamResponse
newDescribeWorkteamResponse pHttpStatus_ pWorkteam_ =
  DescribeWorkteamResponse'
    { httpStatus =
        pHttpStatus_,
      workteam = pWorkteam_
    }

-- | The response's http status code.
describeWorkteamResponse_httpStatus :: Lens.Lens' DescribeWorkteamResponse Core.Int
describeWorkteamResponse_httpStatus = Lens.lens (\DescribeWorkteamResponse' {httpStatus} -> httpStatus) (\s@DescribeWorkteamResponse' {} a -> s {httpStatus = a} :: DescribeWorkteamResponse)

-- | A @Workteam@ instance that contains information about the work team.
describeWorkteamResponse_workteam :: Lens.Lens' DescribeWorkteamResponse Workteam
describeWorkteamResponse_workteam = Lens.lens (\DescribeWorkteamResponse' {workteam} -> workteam) (\s@DescribeWorkteamResponse' {} a -> s {workteam = a} :: DescribeWorkteamResponse)

instance Core.NFData DescribeWorkteamResponse
