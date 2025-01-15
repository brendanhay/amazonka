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
-- Module      : Amazonka.EC2.DescribeSpotDatafeedSubscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the data feed for Spot Instances. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-data-feeds.html Spot Instance data feed>
-- in the /Amazon EC2 User Guide for Linux Instances/.
module Amazonka.EC2.DescribeSpotDatafeedSubscription
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Core.AWSRequest
    DescribeSpotDatafeedSubscription
  where
  type
    AWSResponse DescribeSpotDatafeedSubscription =
      DescribeSpotDatafeedSubscriptionResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeSpotDatafeedSubscriptionResponse'
            Prelude.<$> (x Data..@? "spotDatafeedSubscription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeSpotDatafeedSubscription
  where
  hashWithSalt
    _salt
    DescribeSpotDatafeedSubscription' {..} =
      _salt `Prelude.hashWithSalt` dryRun

instance
  Prelude.NFData
    DescribeSpotDatafeedSubscription
  where
  rnf DescribeSpotDatafeedSubscription' {..} =
    Prelude.rnf dryRun

instance
  Data.ToHeaders
    DescribeSpotDatafeedSubscription
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeSpotDatafeedSubscription where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeSpotDatafeedSubscription
  where
  toQuery DescribeSpotDatafeedSubscription' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeSpotDatafeedSubscription" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf DescribeSpotDatafeedSubscriptionResponse' {..} =
    Prelude.rnf spotDatafeedSubscription `Prelude.seq`
      Prelude.rnf httpStatus
