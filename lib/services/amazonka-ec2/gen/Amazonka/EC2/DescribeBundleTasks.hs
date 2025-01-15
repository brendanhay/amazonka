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
-- Module      : Amazonka.EC2.DescribeBundleTasks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified bundle tasks or all of your bundle tasks.
--
-- Completed bundle tasks are listed for only a limited time. If your
-- bundle task is no longer in the list, you can still register an AMI from
-- it. Just use @RegisterImage@ with the Amazon S3 bucket name and image
-- manifest name you provided to the bundle task.
module Amazonka.EC2.DescribeBundleTasks
  ( -- * Creating a Request
    DescribeBundleTasks (..),
    newDescribeBundleTasks,

    -- * Request Lenses
    describeBundleTasks_bundleIds,
    describeBundleTasks_dryRun,
    describeBundleTasks_filters,

    -- * Destructuring the Response
    DescribeBundleTasksResponse (..),
    newDescribeBundleTasksResponse,

    -- * Response Lenses
    describeBundleTasksResponse_bundleTasks,
    describeBundleTasksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeBundleTasks' smart constructor.
data DescribeBundleTasks = DescribeBundleTasks'
  { -- | The bundle task IDs.
    --
    -- Default: Describes all your bundle tasks.
    bundleIds :: Prelude.Maybe [Prelude.Text],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The filters.
    --
    -- -   @bundle-id@ - The ID of the bundle task.
    --
    -- -   @error-code@ - If the task failed, the error code returned.
    --
    -- -   @error-message@ - If the task failed, the error message returned.
    --
    -- -   @instance-id@ - The ID of the instance.
    --
    -- -   @progress@ - The level of task completion, as a percentage (for
    --     example, 20%).
    --
    -- -   @s3-bucket@ - The Amazon S3 bucket to store the AMI.
    --
    -- -   @s3-prefix@ - The beginning of the AMI name.
    --
    -- -   @start-time@ - The time the task started (for example,
    --     2013-09-15T17:15:20.000Z).
    --
    -- -   @state@ - The state of the task (@pending@ | @waiting-for-shutdown@
    --     | @bundling@ | @storing@ | @cancelling@ | @complete@ | @failed@).
    --
    -- -   @update-time@ - The time of the most recent update for the task.
    filters :: Prelude.Maybe [Filter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBundleTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bundleIds', 'describeBundleTasks_bundleIds' - The bundle task IDs.
--
-- Default: Describes all your bundle tasks.
--
-- 'dryRun', 'describeBundleTasks_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeBundleTasks_filters' - The filters.
--
-- -   @bundle-id@ - The ID of the bundle task.
--
-- -   @error-code@ - If the task failed, the error code returned.
--
-- -   @error-message@ - If the task failed, the error message returned.
--
-- -   @instance-id@ - The ID of the instance.
--
-- -   @progress@ - The level of task completion, as a percentage (for
--     example, 20%).
--
-- -   @s3-bucket@ - The Amazon S3 bucket to store the AMI.
--
-- -   @s3-prefix@ - The beginning of the AMI name.
--
-- -   @start-time@ - The time the task started (for example,
--     2013-09-15T17:15:20.000Z).
--
-- -   @state@ - The state of the task (@pending@ | @waiting-for-shutdown@
--     | @bundling@ | @storing@ | @cancelling@ | @complete@ | @failed@).
--
-- -   @update-time@ - The time of the most recent update for the task.
newDescribeBundleTasks ::
  DescribeBundleTasks
newDescribeBundleTasks =
  DescribeBundleTasks'
    { bundleIds = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | The bundle task IDs.
--
-- Default: Describes all your bundle tasks.
describeBundleTasks_bundleIds :: Lens.Lens' DescribeBundleTasks (Prelude.Maybe [Prelude.Text])
describeBundleTasks_bundleIds = Lens.lens (\DescribeBundleTasks' {bundleIds} -> bundleIds) (\s@DescribeBundleTasks' {} a -> s {bundleIds = a} :: DescribeBundleTasks) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeBundleTasks_dryRun :: Lens.Lens' DescribeBundleTasks (Prelude.Maybe Prelude.Bool)
describeBundleTasks_dryRun = Lens.lens (\DescribeBundleTasks' {dryRun} -> dryRun) (\s@DescribeBundleTasks' {} a -> s {dryRun = a} :: DescribeBundleTasks)

-- | The filters.
--
-- -   @bundle-id@ - The ID of the bundle task.
--
-- -   @error-code@ - If the task failed, the error code returned.
--
-- -   @error-message@ - If the task failed, the error message returned.
--
-- -   @instance-id@ - The ID of the instance.
--
-- -   @progress@ - The level of task completion, as a percentage (for
--     example, 20%).
--
-- -   @s3-bucket@ - The Amazon S3 bucket to store the AMI.
--
-- -   @s3-prefix@ - The beginning of the AMI name.
--
-- -   @start-time@ - The time the task started (for example,
--     2013-09-15T17:15:20.000Z).
--
-- -   @state@ - The state of the task (@pending@ | @waiting-for-shutdown@
--     | @bundling@ | @storing@ | @cancelling@ | @complete@ | @failed@).
--
-- -   @update-time@ - The time of the most recent update for the task.
describeBundleTasks_filters :: Lens.Lens' DescribeBundleTasks (Prelude.Maybe [Filter])
describeBundleTasks_filters = Lens.lens (\DescribeBundleTasks' {filters} -> filters) (\s@DescribeBundleTasks' {} a -> s {filters = a} :: DescribeBundleTasks) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest DescribeBundleTasks where
  type
    AWSResponse DescribeBundleTasks =
      DescribeBundleTasksResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeBundleTasksResponse'
            Prelude.<$> ( x
                            Data..@? "bundleInstanceTasksSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeBundleTasks where
  hashWithSalt _salt DescribeBundleTasks' {..} =
    _salt
      `Prelude.hashWithSalt` bundleIds
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters

instance Prelude.NFData DescribeBundleTasks where
  rnf DescribeBundleTasks' {..} =
    Prelude.rnf bundleIds `Prelude.seq`
      Prelude.rnf dryRun `Prelude.seq`
        Prelude.rnf filters

instance Data.ToHeaders DescribeBundleTasks where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeBundleTasks where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeBundleTasks where
  toQuery DescribeBundleTasks' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeBundleTasks" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        Data.toQuery
          (Data.toQueryList "BundleId" Prelude.<$> bundleIds),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters)
      ]

-- | /See:/ 'newDescribeBundleTasksResponse' smart constructor.
data DescribeBundleTasksResponse = DescribeBundleTasksResponse'
  { -- | Information about the bundle tasks.
    bundleTasks :: Prelude.Maybe [BundleTask],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBundleTasksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bundleTasks', 'describeBundleTasksResponse_bundleTasks' - Information about the bundle tasks.
--
-- 'httpStatus', 'describeBundleTasksResponse_httpStatus' - The response's http status code.
newDescribeBundleTasksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeBundleTasksResponse
newDescribeBundleTasksResponse pHttpStatus_ =
  DescribeBundleTasksResponse'
    { bundleTasks =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the bundle tasks.
describeBundleTasksResponse_bundleTasks :: Lens.Lens' DescribeBundleTasksResponse (Prelude.Maybe [BundleTask])
describeBundleTasksResponse_bundleTasks = Lens.lens (\DescribeBundleTasksResponse' {bundleTasks} -> bundleTasks) (\s@DescribeBundleTasksResponse' {} a -> s {bundleTasks = a} :: DescribeBundleTasksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeBundleTasksResponse_httpStatus :: Lens.Lens' DescribeBundleTasksResponse Prelude.Int
describeBundleTasksResponse_httpStatus = Lens.lens (\DescribeBundleTasksResponse' {httpStatus} -> httpStatus) (\s@DescribeBundleTasksResponse' {} a -> s {httpStatus = a} :: DescribeBundleTasksResponse)

instance Prelude.NFData DescribeBundleTasksResponse where
  rnf DescribeBundleTasksResponse' {..} =
    Prelude.rnf bundleTasks `Prelude.seq`
      Prelude.rnf httpStatus
