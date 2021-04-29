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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeSubscribedWorkteam' smart constructor.
data DescribeSubscribedWorkteam = DescribeSubscribedWorkteam'
  { -- | The Amazon Resource Name (ARN) of the subscribed work team to describe.
    workteamArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DescribeSubscribedWorkteam
newDescribeSubscribedWorkteam pWorkteamArn_ =
  DescribeSubscribedWorkteam'
    { workteamArn =
        pWorkteamArn_
    }

-- | The Amazon Resource Name (ARN) of the subscribed work team to describe.
describeSubscribedWorkteam_workteamArn :: Lens.Lens' DescribeSubscribedWorkteam Prelude.Text
describeSubscribedWorkteam_workteamArn = Lens.lens (\DescribeSubscribedWorkteam' {workteamArn} -> workteamArn) (\s@DescribeSubscribedWorkteam' {} a -> s {workteamArn = a} :: DescribeSubscribedWorkteam)

instance
  Prelude.AWSRequest
    DescribeSubscribedWorkteam
  where
  type
    Rs DescribeSubscribedWorkteam =
      DescribeSubscribedWorkteamResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSubscribedWorkteamResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "SubscribedWorkteam")
      )

instance Prelude.Hashable DescribeSubscribedWorkteam

instance Prelude.NFData DescribeSubscribedWorkteam

instance Prelude.ToHeaders DescribeSubscribedWorkteam where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.DescribeSubscribedWorkteam" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeSubscribedWorkteam where
  toJSON DescribeSubscribedWorkteam' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("WorkteamArn" Prelude..= workteamArn)
          ]
      )

instance Prelude.ToPath DescribeSubscribedWorkteam where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeSubscribedWorkteam where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSubscribedWorkteamResponse' smart constructor.
data DescribeSubscribedWorkteamResponse = DescribeSubscribedWorkteamResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A @Workteam@ instance that contains information about the work team.
    subscribedWorkteam :: SubscribedWorkteam
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
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
describeSubscribedWorkteamResponse_httpStatus :: Lens.Lens' DescribeSubscribedWorkteamResponse Prelude.Int
describeSubscribedWorkteamResponse_httpStatus = Lens.lens (\DescribeSubscribedWorkteamResponse' {httpStatus} -> httpStatus) (\s@DescribeSubscribedWorkteamResponse' {} a -> s {httpStatus = a} :: DescribeSubscribedWorkteamResponse)

-- | A @Workteam@ instance that contains information about the work team.
describeSubscribedWorkteamResponse_subscribedWorkteam :: Lens.Lens' DescribeSubscribedWorkteamResponse SubscribedWorkteam
describeSubscribedWorkteamResponse_subscribedWorkteam = Lens.lens (\DescribeSubscribedWorkteamResponse' {subscribedWorkteam} -> subscribedWorkteam) (\s@DescribeSubscribedWorkteamResponse' {} a -> s {subscribedWorkteam = a} :: DescribeSubscribedWorkteamResponse)

instance
  Prelude.NFData
    DescribeSubscribedWorkteamResponse
