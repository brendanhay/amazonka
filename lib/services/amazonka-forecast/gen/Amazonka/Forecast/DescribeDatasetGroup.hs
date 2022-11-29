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
-- Module      : Amazonka.Forecast.DescribeDatasetGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a dataset group created using the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_CreateDatasetGroup.html CreateDatasetGroup>
-- operation.
--
-- In addition to listing the parameters provided in the
-- @CreateDatasetGroup@ request, this operation includes the following
-- properties:
--
-- -   @DatasetArns@ - The datasets belonging to the group.
--
-- -   @CreationTime@
--
-- -   @LastModificationTime@
--
-- -   @Status@
module Amazonka.Forecast.DescribeDatasetGroup
  ( -- * Creating a Request
    DescribeDatasetGroup (..),
    newDescribeDatasetGroup,

    -- * Request Lenses
    describeDatasetGroup_datasetGroupArn,

    -- * Destructuring the Response
    DescribeDatasetGroupResponse (..),
    newDescribeDatasetGroupResponse,

    -- * Response Lenses
    describeDatasetGroupResponse_lastModificationTime,
    describeDatasetGroupResponse_datasetGroupName,
    describeDatasetGroupResponse_domain,
    describeDatasetGroupResponse_status,
    describeDatasetGroupResponse_datasetArns,
    describeDatasetGroupResponse_creationTime,
    describeDatasetGroupResponse_datasetGroupArn,
    describeDatasetGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDatasetGroup' smart constructor.
data DescribeDatasetGroup = DescribeDatasetGroup'
  { -- | The Amazon Resource Name (ARN) of the dataset group.
    datasetGroupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDatasetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetGroupArn', 'describeDatasetGroup_datasetGroupArn' - The Amazon Resource Name (ARN) of the dataset group.
newDescribeDatasetGroup ::
  -- | 'datasetGroupArn'
  Prelude.Text ->
  DescribeDatasetGroup
newDescribeDatasetGroup pDatasetGroupArn_ =
  DescribeDatasetGroup'
    { datasetGroupArn =
        pDatasetGroupArn_
    }

-- | The Amazon Resource Name (ARN) of the dataset group.
describeDatasetGroup_datasetGroupArn :: Lens.Lens' DescribeDatasetGroup Prelude.Text
describeDatasetGroup_datasetGroupArn = Lens.lens (\DescribeDatasetGroup' {datasetGroupArn} -> datasetGroupArn) (\s@DescribeDatasetGroup' {} a -> s {datasetGroupArn = a} :: DescribeDatasetGroup)

instance Core.AWSRequest DescribeDatasetGroup where
  type
    AWSResponse DescribeDatasetGroup =
      DescribeDatasetGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDatasetGroupResponse'
            Prelude.<$> (x Core..?> "LastModificationTime")
            Prelude.<*> (x Core..?> "DatasetGroupName")
            Prelude.<*> (x Core..?> "Domain")
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "DatasetArns" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "DatasetGroupArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDatasetGroup where
  hashWithSalt _salt DescribeDatasetGroup' {..} =
    _salt `Prelude.hashWithSalt` datasetGroupArn

instance Prelude.NFData DescribeDatasetGroup where
  rnf DescribeDatasetGroup' {..} =
    Prelude.rnf datasetGroupArn

instance Core.ToHeaders DescribeDatasetGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonForecast.DescribeDatasetGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeDatasetGroup where
  toJSON DescribeDatasetGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DatasetGroupArn" Core..= datasetGroupArn)
          ]
      )

instance Core.ToPath DescribeDatasetGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeDatasetGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDatasetGroupResponse' smart constructor.
data DescribeDatasetGroupResponse = DescribeDatasetGroupResponse'
  { -- | When the dataset group was created or last updated from a call to the
    -- <https://docs.aws.amazon.com/forecast/latest/dg/API_UpdateDatasetGroup.html UpdateDatasetGroup>
    -- operation. While the dataset group is being updated,
    -- @LastModificationTime@ is the current time of the @DescribeDatasetGroup@
    -- call.
    lastModificationTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the dataset group.
    datasetGroupName :: Prelude.Maybe Prelude.Text,
    -- | The domain associated with the dataset group.
    domain :: Prelude.Maybe Domain,
    -- | The status of the dataset group. States include:
    --
    -- -   @ACTIVE@
    --
    -- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
    --
    -- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
    --
    -- -   @UPDATE_PENDING@, @UPDATE_IN_PROGRESS@, @UPDATE_FAILED@
    --
    -- The @UPDATE@ states apply when you call the
    -- <https://docs.aws.amazon.com/forecast/latest/dg/API_UpdateDatasetGroup.html UpdateDatasetGroup>
    -- operation.
    --
    -- The @Status@ of the dataset group must be @ACTIVE@ before you can use
    -- the dataset group to create a predictor.
    status :: Prelude.Maybe Prelude.Text,
    -- | An array of Amazon Resource Names (ARNs) of the datasets contained in
    -- the dataset group.
    datasetArns :: Prelude.Maybe [Prelude.Text],
    -- | When the dataset group was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The ARN of the dataset group.
    datasetGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDatasetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModificationTime', 'describeDatasetGroupResponse_lastModificationTime' - When the dataset group was created or last updated from a call to the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_UpdateDatasetGroup.html UpdateDatasetGroup>
-- operation. While the dataset group is being updated,
-- @LastModificationTime@ is the current time of the @DescribeDatasetGroup@
-- call.
--
-- 'datasetGroupName', 'describeDatasetGroupResponse_datasetGroupName' - The name of the dataset group.
--
-- 'domain', 'describeDatasetGroupResponse_domain' - The domain associated with the dataset group.
--
-- 'status', 'describeDatasetGroupResponse_status' - The status of the dataset group. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
--
-- -   @UPDATE_PENDING@, @UPDATE_IN_PROGRESS@, @UPDATE_FAILED@
--
-- The @UPDATE@ states apply when you call the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_UpdateDatasetGroup.html UpdateDatasetGroup>
-- operation.
--
-- The @Status@ of the dataset group must be @ACTIVE@ before you can use
-- the dataset group to create a predictor.
--
-- 'datasetArns', 'describeDatasetGroupResponse_datasetArns' - An array of Amazon Resource Names (ARNs) of the datasets contained in
-- the dataset group.
--
-- 'creationTime', 'describeDatasetGroupResponse_creationTime' - When the dataset group was created.
--
-- 'datasetGroupArn', 'describeDatasetGroupResponse_datasetGroupArn' - The ARN of the dataset group.
--
-- 'httpStatus', 'describeDatasetGroupResponse_httpStatus' - The response's http status code.
newDescribeDatasetGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDatasetGroupResponse
newDescribeDatasetGroupResponse pHttpStatus_ =
  DescribeDatasetGroupResponse'
    { lastModificationTime =
        Prelude.Nothing,
      datasetGroupName = Prelude.Nothing,
      domain = Prelude.Nothing,
      status = Prelude.Nothing,
      datasetArns = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      datasetGroupArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When the dataset group was created or last updated from a call to the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_UpdateDatasetGroup.html UpdateDatasetGroup>
-- operation. While the dataset group is being updated,
-- @LastModificationTime@ is the current time of the @DescribeDatasetGroup@
-- call.
describeDatasetGroupResponse_lastModificationTime :: Lens.Lens' DescribeDatasetGroupResponse (Prelude.Maybe Prelude.UTCTime)
describeDatasetGroupResponse_lastModificationTime = Lens.lens (\DescribeDatasetGroupResponse' {lastModificationTime} -> lastModificationTime) (\s@DescribeDatasetGroupResponse' {} a -> s {lastModificationTime = a} :: DescribeDatasetGroupResponse) Prelude.. Lens.mapping Core._Time

-- | The name of the dataset group.
describeDatasetGroupResponse_datasetGroupName :: Lens.Lens' DescribeDatasetGroupResponse (Prelude.Maybe Prelude.Text)
describeDatasetGroupResponse_datasetGroupName = Lens.lens (\DescribeDatasetGroupResponse' {datasetGroupName} -> datasetGroupName) (\s@DescribeDatasetGroupResponse' {} a -> s {datasetGroupName = a} :: DescribeDatasetGroupResponse)

-- | The domain associated with the dataset group.
describeDatasetGroupResponse_domain :: Lens.Lens' DescribeDatasetGroupResponse (Prelude.Maybe Domain)
describeDatasetGroupResponse_domain = Lens.lens (\DescribeDatasetGroupResponse' {domain} -> domain) (\s@DescribeDatasetGroupResponse' {} a -> s {domain = a} :: DescribeDatasetGroupResponse)

-- | The status of the dataset group. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
--
-- -   @UPDATE_PENDING@, @UPDATE_IN_PROGRESS@, @UPDATE_FAILED@
--
-- The @UPDATE@ states apply when you call the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_UpdateDatasetGroup.html UpdateDatasetGroup>
-- operation.
--
-- The @Status@ of the dataset group must be @ACTIVE@ before you can use
-- the dataset group to create a predictor.
describeDatasetGroupResponse_status :: Lens.Lens' DescribeDatasetGroupResponse (Prelude.Maybe Prelude.Text)
describeDatasetGroupResponse_status = Lens.lens (\DescribeDatasetGroupResponse' {status} -> status) (\s@DescribeDatasetGroupResponse' {} a -> s {status = a} :: DescribeDatasetGroupResponse)

-- | An array of Amazon Resource Names (ARNs) of the datasets contained in
-- the dataset group.
describeDatasetGroupResponse_datasetArns :: Lens.Lens' DescribeDatasetGroupResponse (Prelude.Maybe [Prelude.Text])
describeDatasetGroupResponse_datasetArns = Lens.lens (\DescribeDatasetGroupResponse' {datasetArns} -> datasetArns) (\s@DescribeDatasetGroupResponse' {} a -> s {datasetArns = a} :: DescribeDatasetGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | When the dataset group was created.
describeDatasetGroupResponse_creationTime :: Lens.Lens' DescribeDatasetGroupResponse (Prelude.Maybe Prelude.UTCTime)
describeDatasetGroupResponse_creationTime = Lens.lens (\DescribeDatasetGroupResponse' {creationTime} -> creationTime) (\s@DescribeDatasetGroupResponse' {} a -> s {creationTime = a} :: DescribeDatasetGroupResponse) Prelude.. Lens.mapping Core._Time

-- | The ARN of the dataset group.
describeDatasetGroupResponse_datasetGroupArn :: Lens.Lens' DescribeDatasetGroupResponse (Prelude.Maybe Prelude.Text)
describeDatasetGroupResponse_datasetGroupArn = Lens.lens (\DescribeDatasetGroupResponse' {datasetGroupArn} -> datasetGroupArn) (\s@DescribeDatasetGroupResponse' {} a -> s {datasetGroupArn = a} :: DescribeDatasetGroupResponse)

-- | The response's http status code.
describeDatasetGroupResponse_httpStatus :: Lens.Lens' DescribeDatasetGroupResponse Prelude.Int
describeDatasetGroupResponse_httpStatus = Lens.lens (\DescribeDatasetGroupResponse' {httpStatus} -> httpStatus) (\s@DescribeDatasetGroupResponse' {} a -> s {httpStatus = a} :: DescribeDatasetGroupResponse)

instance Prelude.NFData DescribeDatasetGroupResponse where
  rnf DescribeDatasetGroupResponse' {..} =
    Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf datasetGroupName
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf datasetArns
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf datasetGroupArn
      `Prelude.seq` Prelude.rnf httpStatus
