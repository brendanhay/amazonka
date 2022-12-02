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
-- Module      : Amazonka.Location.DescribeTracker
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the tracker resource details.
module Amazonka.Location.DescribeTracker
  ( -- * Creating a Request
    DescribeTracker (..),
    newDescribeTracker,

    -- * Request Lenses
    describeTracker_trackerName,

    -- * Destructuring the Response
    DescribeTrackerResponse (..),
    newDescribeTrackerResponse,

    -- * Response Lenses
    describeTrackerResponse_tags,
    describeTrackerResponse_pricingPlanDataSource,
    describeTrackerResponse_pricingPlan,
    describeTrackerResponse_kmsKeyId,
    describeTrackerResponse_positionFiltering,
    describeTrackerResponse_httpStatus,
    describeTrackerResponse_createTime,
    describeTrackerResponse_description,
    describeTrackerResponse_trackerArn,
    describeTrackerResponse_trackerName,
    describeTrackerResponse_updateTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTracker' smart constructor.
data DescribeTracker = DescribeTracker'
  { -- | The name of the tracker resource.
    trackerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTracker' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trackerName', 'describeTracker_trackerName' - The name of the tracker resource.
newDescribeTracker ::
  -- | 'trackerName'
  Prelude.Text ->
  DescribeTracker
newDescribeTracker pTrackerName_ =
  DescribeTracker' {trackerName = pTrackerName_}

-- | The name of the tracker resource.
describeTracker_trackerName :: Lens.Lens' DescribeTracker Prelude.Text
describeTracker_trackerName = Lens.lens (\DescribeTracker' {trackerName} -> trackerName) (\s@DescribeTracker' {} a -> s {trackerName = a} :: DescribeTracker)

instance Core.AWSRequest DescribeTracker where
  type
    AWSResponse DescribeTracker =
      DescribeTrackerResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTrackerResponse'
            Prelude.<$> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "PricingPlanDataSource")
            Prelude.<*> (x Data..?> "PricingPlan")
            Prelude.<*> (x Data..?> "KmsKeyId")
            Prelude.<*> (x Data..?> "PositionFiltering")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "CreateTime")
            Prelude.<*> (x Data..:> "Description")
            Prelude.<*> (x Data..:> "TrackerArn")
            Prelude.<*> (x Data..:> "TrackerName")
            Prelude.<*> (x Data..:> "UpdateTime")
      )

instance Prelude.Hashable DescribeTracker where
  hashWithSalt _salt DescribeTracker' {..} =
    _salt `Prelude.hashWithSalt` trackerName

instance Prelude.NFData DescribeTracker where
  rnf DescribeTracker' {..} = Prelude.rnf trackerName

instance Data.ToHeaders DescribeTracker where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeTracker where
  toPath DescribeTracker' {..} =
    Prelude.mconcat
      ["/tracking/v0/trackers/", Data.toBS trackerName]

instance Data.ToQuery DescribeTracker where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTrackerResponse' smart constructor.
data DescribeTrackerResponse = DescribeTrackerResponse'
  { -- | The tags associated with the tracker resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | No longer used. Always returns an empty string.
    pricingPlanDataSource :: Prelude.Maybe Prelude.Text,
    -- | Always returns @RequestBasedUsage@.
    pricingPlan :: Prelude.Maybe PricingPlan,
    -- | A key identifier for an
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/create-keys.html AWS KMS customer managed key>
    -- assigned to the Amazon Location resource.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The position filtering method of the tracker resource.
    positionFiltering :: Prelude.Maybe PositionFiltering,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The timestamp for when the tracker resource was created in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    createTime :: Data.POSIX,
    -- | The optional description for the tracker resource.
    description :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the tracker resource. Used when you
    -- need to specify a resource across all AWS.
    --
    -- -   Format example:
    --     @arn:aws:geo:region:account-id:tracker\/ExampleTracker@
    trackerArn :: Prelude.Text,
    -- | The name of the tracker resource.
    trackerName :: Prelude.Text,
    -- | The timestamp for when the tracker resource was last updated in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    updateTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTrackerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'describeTrackerResponse_tags' - The tags associated with the tracker resource.
--
-- 'pricingPlanDataSource', 'describeTrackerResponse_pricingPlanDataSource' - No longer used. Always returns an empty string.
--
-- 'pricingPlan', 'describeTrackerResponse_pricingPlan' - Always returns @RequestBasedUsage@.
--
-- 'kmsKeyId', 'describeTrackerResponse_kmsKeyId' - A key identifier for an
-- <https://docs.aws.amazon.com/kms/latest/developerguide/create-keys.html AWS KMS customer managed key>
-- assigned to the Amazon Location resource.
--
-- 'positionFiltering', 'describeTrackerResponse_positionFiltering' - The position filtering method of the tracker resource.
--
-- 'httpStatus', 'describeTrackerResponse_httpStatus' - The response's http status code.
--
-- 'createTime', 'describeTrackerResponse_createTime' - The timestamp for when the tracker resource was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
--
-- 'description', 'describeTrackerResponse_description' - The optional description for the tracker resource.
--
-- 'trackerArn', 'describeTrackerResponse_trackerArn' - The Amazon Resource Name (ARN) for the tracker resource. Used when you
-- need to specify a resource across all AWS.
--
-- -   Format example:
--     @arn:aws:geo:region:account-id:tracker\/ExampleTracker@
--
-- 'trackerName', 'describeTrackerResponse_trackerName' - The name of the tracker resource.
--
-- 'updateTime', 'describeTrackerResponse_updateTime' - The timestamp for when the tracker resource was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
newDescribeTrackerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'description'
  Prelude.Text ->
  -- | 'trackerArn'
  Prelude.Text ->
  -- | 'trackerName'
  Prelude.Text ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  DescribeTrackerResponse
newDescribeTrackerResponse
  pHttpStatus_
  pCreateTime_
  pDescription_
  pTrackerArn_
  pTrackerName_
  pUpdateTime_ =
    DescribeTrackerResponse'
      { tags = Prelude.Nothing,
        pricingPlanDataSource = Prelude.Nothing,
        pricingPlan = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        positionFiltering = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        createTime = Data._Time Lens.# pCreateTime_,
        description = pDescription_,
        trackerArn = pTrackerArn_,
        trackerName = pTrackerName_,
        updateTime = Data._Time Lens.# pUpdateTime_
      }

-- | The tags associated with the tracker resource.
describeTrackerResponse_tags :: Lens.Lens' DescribeTrackerResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeTrackerResponse_tags = Lens.lens (\DescribeTrackerResponse' {tags} -> tags) (\s@DescribeTrackerResponse' {} a -> s {tags = a} :: DescribeTrackerResponse) Prelude.. Lens.mapping Lens.coerced

-- | No longer used. Always returns an empty string.
describeTrackerResponse_pricingPlanDataSource :: Lens.Lens' DescribeTrackerResponse (Prelude.Maybe Prelude.Text)
describeTrackerResponse_pricingPlanDataSource = Lens.lens (\DescribeTrackerResponse' {pricingPlanDataSource} -> pricingPlanDataSource) (\s@DescribeTrackerResponse' {} a -> s {pricingPlanDataSource = a} :: DescribeTrackerResponse)

-- | Always returns @RequestBasedUsage@.
describeTrackerResponse_pricingPlan :: Lens.Lens' DescribeTrackerResponse (Prelude.Maybe PricingPlan)
describeTrackerResponse_pricingPlan = Lens.lens (\DescribeTrackerResponse' {pricingPlan} -> pricingPlan) (\s@DescribeTrackerResponse' {} a -> s {pricingPlan = a} :: DescribeTrackerResponse)

-- | A key identifier for an
-- <https://docs.aws.amazon.com/kms/latest/developerguide/create-keys.html AWS KMS customer managed key>
-- assigned to the Amazon Location resource.
describeTrackerResponse_kmsKeyId :: Lens.Lens' DescribeTrackerResponse (Prelude.Maybe Prelude.Text)
describeTrackerResponse_kmsKeyId = Lens.lens (\DescribeTrackerResponse' {kmsKeyId} -> kmsKeyId) (\s@DescribeTrackerResponse' {} a -> s {kmsKeyId = a} :: DescribeTrackerResponse)

-- | The position filtering method of the tracker resource.
describeTrackerResponse_positionFiltering :: Lens.Lens' DescribeTrackerResponse (Prelude.Maybe PositionFiltering)
describeTrackerResponse_positionFiltering = Lens.lens (\DescribeTrackerResponse' {positionFiltering} -> positionFiltering) (\s@DescribeTrackerResponse' {} a -> s {positionFiltering = a} :: DescribeTrackerResponse)

-- | The response's http status code.
describeTrackerResponse_httpStatus :: Lens.Lens' DescribeTrackerResponse Prelude.Int
describeTrackerResponse_httpStatus = Lens.lens (\DescribeTrackerResponse' {httpStatus} -> httpStatus) (\s@DescribeTrackerResponse' {} a -> s {httpStatus = a} :: DescribeTrackerResponse)

-- | The timestamp for when the tracker resource was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
describeTrackerResponse_createTime :: Lens.Lens' DescribeTrackerResponse Prelude.UTCTime
describeTrackerResponse_createTime = Lens.lens (\DescribeTrackerResponse' {createTime} -> createTime) (\s@DescribeTrackerResponse' {} a -> s {createTime = a} :: DescribeTrackerResponse) Prelude.. Data._Time

-- | The optional description for the tracker resource.
describeTrackerResponse_description :: Lens.Lens' DescribeTrackerResponse Prelude.Text
describeTrackerResponse_description = Lens.lens (\DescribeTrackerResponse' {description} -> description) (\s@DescribeTrackerResponse' {} a -> s {description = a} :: DescribeTrackerResponse)

-- | The Amazon Resource Name (ARN) for the tracker resource. Used when you
-- need to specify a resource across all AWS.
--
-- -   Format example:
--     @arn:aws:geo:region:account-id:tracker\/ExampleTracker@
describeTrackerResponse_trackerArn :: Lens.Lens' DescribeTrackerResponse Prelude.Text
describeTrackerResponse_trackerArn = Lens.lens (\DescribeTrackerResponse' {trackerArn} -> trackerArn) (\s@DescribeTrackerResponse' {} a -> s {trackerArn = a} :: DescribeTrackerResponse)

-- | The name of the tracker resource.
describeTrackerResponse_trackerName :: Lens.Lens' DescribeTrackerResponse Prelude.Text
describeTrackerResponse_trackerName = Lens.lens (\DescribeTrackerResponse' {trackerName} -> trackerName) (\s@DescribeTrackerResponse' {} a -> s {trackerName = a} :: DescribeTrackerResponse)

-- | The timestamp for when the tracker resource was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
describeTrackerResponse_updateTime :: Lens.Lens' DescribeTrackerResponse Prelude.UTCTime
describeTrackerResponse_updateTime = Lens.lens (\DescribeTrackerResponse' {updateTime} -> updateTime) (\s@DescribeTrackerResponse' {} a -> s {updateTime = a} :: DescribeTrackerResponse) Prelude.. Data._Time

instance Prelude.NFData DescribeTrackerResponse where
  rnf DescribeTrackerResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf pricingPlanDataSource
      `Prelude.seq` Prelude.rnf pricingPlan
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf positionFiltering
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf trackerArn
      `Prelude.seq` Prelude.rnf trackerName
      `Prelude.seq` Prelude.rnf updateTime
