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
-- Module      : Network.AWS.SageMaker.DescribeSubscribedWorkteam
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a work team provided by a vendor. It returns
-- details about the subscription with a vendor in the AWS Marketplace.
module Network.AWS.SageMaker.DescribeSubscribedWorkteam
  ( -- * Creating a Request
    DescribeSubscribedWorkteam (..),
    newDescribeSubscribedWorkteam,

    -- * Request Lenses
    describeSubscribedWorkteam_workteamArn,

    -- * Destructuring the Response
    DescribeSubscribedWorkteamResponse (..),
    newDescribeSubscribedWorkteamResponse,

    -- * Response Lenses
    describeSubscribedWorkteamResponse_httpStatus,
    describeSubscribedWorkteamResponse_subscribedWorkteam,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeSubscribedWorkteam' smart constructor.
data DescribeSubscribedWorkteam = DescribeSubscribedWorkteam'
  { -- | The Amazon Resource Name (ARN) of the subscribed work team to describe.
    workteamArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSubscribedWorkteam' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workteamArn', 'describeSubscribedWorkteam_workteamArn' - The Amazon Resource Name (ARN) of the subscribed work team to describe.
newDescribeSubscribedWorkteam ::
  -- | 'workteamArn'
  Core.Text ->
  DescribeSubscribedWorkteam
newDescribeSubscribedWorkteam pWorkteamArn_ =
  DescribeSubscribedWorkteam'
    { workteamArn =
        pWorkteamArn_
    }

-- | The Amazon Resource Name (ARN) of the subscribed work team to describe.
describeSubscribedWorkteam_workteamArn :: Lens.Lens' DescribeSubscribedWorkteam Core.Text
describeSubscribedWorkteam_workteamArn = Lens.lens (\DescribeSubscribedWorkteam' {workteamArn} -> workteamArn) (\s@DescribeSubscribedWorkteam' {} a -> s {workteamArn = a} :: DescribeSubscribedWorkteam)

instance Core.AWSRequest DescribeSubscribedWorkteam where
  type
    AWSResponse DescribeSubscribedWorkteam =
      DescribeSubscribedWorkteamResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSubscribedWorkteamResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "SubscribedWorkteam")
      )

instance Core.Hashable DescribeSubscribedWorkteam

instance Core.NFData DescribeSubscribedWorkteam

instance Core.ToHeaders DescribeSubscribedWorkteam where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribeSubscribedWorkteam" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeSubscribedWorkteam where
  toJSON DescribeSubscribedWorkteam' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("WorkteamArn" Core..= workteamArn)]
      )

instance Core.ToPath DescribeSubscribedWorkteam where
  toPath = Core.const "/"

instance Core.ToQuery DescribeSubscribedWorkteam where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeSubscribedWorkteamResponse' smart constructor.
data DescribeSubscribedWorkteamResponse = DescribeSubscribedWorkteamResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A @Workteam@ instance that contains information about the work team.
    subscribedWorkteam :: SubscribedWorkteam
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSubscribedWorkteamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeSubscribedWorkteamResponse_httpStatus' - The response's http status code.
--
-- 'subscribedWorkteam', 'describeSubscribedWorkteamResponse_subscribedWorkteam' - A @Workteam@ instance that contains information about the work team.
newDescribeSubscribedWorkteamResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'subscribedWorkteam'
  SubscribedWorkteam ->
  DescribeSubscribedWorkteamResponse
newDescribeSubscribedWorkteamResponse
  pHttpStatus_
  pSubscribedWorkteam_ =
    DescribeSubscribedWorkteamResponse'
      { httpStatus =
          pHttpStatus_,
        subscribedWorkteam =
          pSubscribedWorkteam_
      }

-- | The response's http status code.
describeSubscribedWorkteamResponse_httpStatus :: Lens.Lens' DescribeSubscribedWorkteamResponse Core.Int
describeSubscribedWorkteamResponse_httpStatus = Lens.lens (\DescribeSubscribedWorkteamResponse' {httpStatus} -> httpStatus) (\s@DescribeSubscribedWorkteamResponse' {} a -> s {httpStatus = a} :: DescribeSubscribedWorkteamResponse)

-- | A @Workteam@ instance that contains information about the work team.
describeSubscribedWorkteamResponse_subscribedWorkteam :: Lens.Lens' DescribeSubscribedWorkteamResponse SubscribedWorkteam
describeSubscribedWorkteamResponse_subscribedWorkteam = Lens.lens (\DescribeSubscribedWorkteamResponse' {subscribedWorkteam} -> subscribedWorkteam) (\s@DescribeSubscribedWorkteamResponse' {} a -> s {subscribedWorkteam = a} :: DescribeSubscribedWorkteamResponse)

instance
  Core.NFData
    DescribeSubscribedWorkteamResponse
