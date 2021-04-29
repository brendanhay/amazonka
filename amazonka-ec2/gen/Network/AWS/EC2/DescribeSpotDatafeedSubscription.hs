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
-- Module      : Network.AWS.EC2.DescribeSpotDatafeedSubscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the data feed for Spot Instances. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-data-feeds.html Spot Instance data feed>
-- in the /Amazon EC2 User Guide for Linux Instances/.
module Network.AWS.EC2.DescribeSpotDatafeedSubscription
  ( -- * Creating a Request
    DescribeSpotDatafeedSubscription (..),
    newDescribeSpotDatafeedSubscription,

    -- * Request Lenses
    describeSpotDatafeedSubscription_dryRun,

    -- * Destructuring the Response
    DescribeSpotDatafeedSubscriptionResponse (..),
    newDescribeSpotDatafeedSubscriptionResponse,

    -- * Response Lenses
    describeSpotDatafeedSubscriptionResponse_spotDatafeedSubscription,
    describeSpotDatafeedSubscriptionResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeSpotDatafeedSubscription.
--
-- /See:/ 'newDescribeSpotDatafeedSubscription' smart constructor.
data DescribeSpotDatafeedSubscription = DescribeSpotDatafeedSubscription'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeSpotDatafeedSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeSpotDatafeedSubscription_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
newDescribeSpotDatafeedSubscription ::
  DescribeSpotDatafeedSubscription
newDescribeSpotDatafeedSubscription =
  DescribeSpotDatafeedSubscription'
    { dryRun =
        Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeSpotDatafeedSubscription_dryRun :: Lens.Lens' DescribeSpotDatafeedSubscription (Prelude.Maybe Prelude.Bool)
describeSpotDatafeedSubscription_dryRun = Lens.lens (\DescribeSpotDatafeedSubscription' {dryRun} -> dryRun) (\s@DescribeSpotDatafeedSubscription' {} a -> s {dryRun = a} :: DescribeSpotDatafeedSubscription)

instance
  Prelude.AWSRequest
    DescribeSpotDatafeedSubscription
  where
  type
    Rs DescribeSpotDatafeedSubscription =
      DescribeSpotDatafeedSubscriptionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeSpotDatafeedSubscriptionResponse'
            Prelude.<$> (x Prelude..@? "spotDatafeedSubscription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeSpotDatafeedSubscription

instance
  Prelude.NFData
    DescribeSpotDatafeedSubscription

instance
  Prelude.ToHeaders
    DescribeSpotDatafeedSubscription
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    DescribeSpotDatafeedSubscription
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DescribeSpotDatafeedSubscription
  where
  toQuery DescribeSpotDatafeedSubscription' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "DescribeSpotDatafeedSubscription" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun
      ]

-- | Contains the output of DescribeSpotDatafeedSubscription.
--
-- /See:/ 'newDescribeSpotDatafeedSubscriptionResponse' smart constructor.
data DescribeSpotDatafeedSubscriptionResponse = DescribeSpotDatafeedSubscriptionResponse'
  { -- | The Spot Instance data feed subscription.
    spotDatafeedSubscription :: Prelude.Maybe SpotDatafeedSubscription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeSpotDatafeedSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'spotDatafeedSubscription', 'describeSpotDatafeedSubscriptionResponse_spotDatafeedSubscription' - The Spot Instance data feed subscription.
--
-- 'httpStatus', 'describeSpotDatafeedSubscriptionResponse_httpStatus' - The response's http status code.
newDescribeSpotDatafeedSubscriptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSpotDatafeedSubscriptionResponse
newDescribeSpotDatafeedSubscriptionResponse
  pHttpStatus_ =
    DescribeSpotDatafeedSubscriptionResponse'
      { spotDatafeedSubscription =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The Spot Instance data feed subscription.
describeSpotDatafeedSubscriptionResponse_spotDatafeedSubscription :: Lens.Lens' DescribeSpotDatafeedSubscriptionResponse (Prelude.Maybe SpotDatafeedSubscription)
describeSpotDatafeedSubscriptionResponse_spotDatafeedSubscription = Lens.lens (\DescribeSpotDatafeedSubscriptionResponse' {spotDatafeedSubscription} -> spotDatafeedSubscription) (\s@DescribeSpotDatafeedSubscriptionResponse' {} a -> s {spotDatafeedSubscription = a} :: DescribeSpotDatafeedSubscriptionResponse)

-- | The response's http status code.
describeSpotDatafeedSubscriptionResponse_httpStatus :: Lens.Lens' DescribeSpotDatafeedSubscriptionResponse Prelude.Int
describeSpotDatafeedSubscriptionResponse_httpStatus = Lens.lens (\DescribeSpotDatafeedSubscriptionResponse' {httpStatus} -> httpStatus) (\s@DescribeSpotDatafeedSubscriptionResponse' {} a -> s {httpStatus = a} :: DescribeSpotDatafeedSubscriptionResponse)

instance
  Prelude.NFData
    DescribeSpotDatafeedSubscriptionResponse
