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
-- Module      : Amazonka.EC2.CreateSpotDatafeedSubscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a data feed for Spot Instances, enabling you to view Spot
-- Instance usage logs. You can create one data feed per Amazon Web
-- Services account. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-data-feeds.html Spot Instance data feed>
-- in the /Amazon EC2 User Guide for Linux Instances/.
module Amazonka.EC2.CreateSpotDatafeedSubscription
  ( -- * Creating a Request
    CreateSpotDatafeedSubscription (..),
    newCreateSpotDatafeedSubscription,

    -- * Request Lenses
    createSpotDatafeedSubscription_dryRun,
    createSpotDatafeedSubscription_prefix,
    createSpotDatafeedSubscription_bucket,

    -- * Destructuring the Response
    CreateSpotDatafeedSubscriptionResponse (..),
    newCreateSpotDatafeedSubscriptionResponse,

    -- * Response Lenses
    createSpotDatafeedSubscriptionResponse_spotDatafeedSubscription,
    createSpotDatafeedSubscriptionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for CreateSpotDatafeedSubscription.
--
-- /See:/ 'newCreateSpotDatafeedSubscription' smart constructor.
data CreateSpotDatafeedSubscription = CreateSpotDatafeedSubscription'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The prefix for the data feed file names.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon S3 bucket in which to store the Spot Instance
    -- data feed. For more information about bucket names, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/BucketRestrictions.html#bucketnamingrules Rules for bucket naming>
    -- in the /Amazon S3 Developer Guide/.
    bucket :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSpotDatafeedSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'createSpotDatafeedSubscription_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'prefix', 'createSpotDatafeedSubscription_prefix' - The prefix for the data feed file names.
--
-- 'bucket', 'createSpotDatafeedSubscription_bucket' - The name of the Amazon S3 bucket in which to store the Spot Instance
-- data feed. For more information about bucket names, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/BucketRestrictions.html#bucketnamingrules Rules for bucket naming>
-- in the /Amazon S3 Developer Guide/.
newCreateSpotDatafeedSubscription ::
  -- | 'bucket'
  Prelude.Text ->
  CreateSpotDatafeedSubscription
newCreateSpotDatafeedSubscription pBucket_ =
  CreateSpotDatafeedSubscription'
    { dryRun =
        Prelude.Nothing,
      prefix = Prelude.Nothing,
      bucket = pBucket_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createSpotDatafeedSubscription_dryRun :: Lens.Lens' CreateSpotDatafeedSubscription (Prelude.Maybe Prelude.Bool)
createSpotDatafeedSubscription_dryRun = Lens.lens (\CreateSpotDatafeedSubscription' {dryRun} -> dryRun) (\s@CreateSpotDatafeedSubscription' {} a -> s {dryRun = a} :: CreateSpotDatafeedSubscription)

-- | The prefix for the data feed file names.
createSpotDatafeedSubscription_prefix :: Lens.Lens' CreateSpotDatafeedSubscription (Prelude.Maybe Prelude.Text)
createSpotDatafeedSubscription_prefix = Lens.lens (\CreateSpotDatafeedSubscription' {prefix} -> prefix) (\s@CreateSpotDatafeedSubscription' {} a -> s {prefix = a} :: CreateSpotDatafeedSubscription)

-- | The name of the Amazon S3 bucket in which to store the Spot Instance
-- data feed. For more information about bucket names, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/BucketRestrictions.html#bucketnamingrules Rules for bucket naming>
-- in the /Amazon S3 Developer Guide/.
createSpotDatafeedSubscription_bucket :: Lens.Lens' CreateSpotDatafeedSubscription Prelude.Text
createSpotDatafeedSubscription_bucket = Lens.lens (\CreateSpotDatafeedSubscription' {bucket} -> bucket) (\s@CreateSpotDatafeedSubscription' {} a -> s {bucket = a} :: CreateSpotDatafeedSubscription)

instance
  Core.AWSRequest
    CreateSpotDatafeedSubscription
  where
  type
    AWSResponse CreateSpotDatafeedSubscription =
      CreateSpotDatafeedSubscriptionResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateSpotDatafeedSubscriptionResponse'
            Prelude.<$> (x Data..@? "spotDatafeedSubscription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateSpotDatafeedSubscription
  where
  hashWithSalt
    _salt
    CreateSpotDatafeedSubscription' {..} =
      _salt `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` prefix
        `Prelude.hashWithSalt` bucket

instance
  Prelude.NFData
    CreateSpotDatafeedSubscription
  where
  rnf CreateSpotDatafeedSubscription' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf bucket

instance
  Data.ToHeaders
    CreateSpotDatafeedSubscription
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateSpotDatafeedSubscription where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateSpotDatafeedSubscription where
  toQuery CreateSpotDatafeedSubscription' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "CreateSpotDatafeedSubscription" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "Prefix" Data.=: prefix,
        "Bucket" Data.=: bucket
      ]

-- | Contains the output of CreateSpotDatafeedSubscription.
--
-- /See:/ 'newCreateSpotDatafeedSubscriptionResponse' smart constructor.
data CreateSpotDatafeedSubscriptionResponse = CreateSpotDatafeedSubscriptionResponse'
  { -- | The Spot Instance data feed subscription.
    spotDatafeedSubscription :: Prelude.Maybe SpotDatafeedSubscription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSpotDatafeedSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'spotDatafeedSubscription', 'createSpotDatafeedSubscriptionResponse_spotDatafeedSubscription' - The Spot Instance data feed subscription.
--
-- 'httpStatus', 'createSpotDatafeedSubscriptionResponse_httpStatus' - The response's http status code.
newCreateSpotDatafeedSubscriptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSpotDatafeedSubscriptionResponse
newCreateSpotDatafeedSubscriptionResponse
  pHttpStatus_ =
    CreateSpotDatafeedSubscriptionResponse'
      { spotDatafeedSubscription =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The Spot Instance data feed subscription.
createSpotDatafeedSubscriptionResponse_spotDatafeedSubscription :: Lens.Lens' CreateSpotDatafeedSubscriptionResponse (Prelude.Maybe SpotDatafeedSubscription)
createSpotDatafeedSubscriptionResponse_spotDatafeedSubscription = Lens.lens (\CreateSpotDatafeedSubscriptionResponse' {spotDatafeedSubscription} -> spotDatafeedSubscription) (\s@CreateSpotDatafeedSubscriptionResponse' {} a -> s {spotDatafeedSubscription = a} :: CreateSpotDatafeedSubscriptionResponse)

-- | The response's http status code.
createSpotDatafeedSubscriptionResponse_httpStatus :: Lens.Lens' CreateSpotDatafeedSubscriptionResponse Prelude.Int
createSpotDatafeedSubscriptionResponse_httpStatus = Lens.lens (\CreateSpotDatafeedSubscriptionResponse' {httpStatus} -> httpStatus) (\s@CreateSpotDatafeedSubscriptionResponse' {} a -> s {httpStatus = a} :: CreateSpotDatafeedSubscriptionResponse)

instance
  Prelude.NFData
    CreateSpotDatafeedSubscriptionResponse
  where
  rnf CreateSpotDatafeedSubscriptionResponse' {..} =
    Prelude.rnf spotDatafeedSubscription
      `Prelude.seq` Prelude.rnf httpStatus
