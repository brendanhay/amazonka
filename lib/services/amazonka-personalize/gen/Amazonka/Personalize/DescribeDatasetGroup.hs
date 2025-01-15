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
-- Module      : Amazonka.Personalize.DescribeDatasetGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the given dataset group. For more information on dataset
-- groups, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_CreateDatasetGroup.html CreateDatasetGroup>.
module Amazonka.Personalize.DescribeDatasetGroup
  ( -- * Creating a Request
    DescribeDatasetGroup (..),
    newDescribeDatasetGroup,

    -- * Request Lenses
    describeDatasetGroup_datasetGroupArn,

    -- * Destructuring the Response
    DescribeDatasetGroupResponse (..),
    newDescribeDatasetGroupResponse,

    -- * Response Lenses
    describeDatasetGroupResponse_datasetGroup,
    describeDatasetGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDatasetGroup' smart constructor.
data DescribeDatasetGroup = DescribeDatasetGroup'
  { -- | The Amazon Resource Name (ARN) of the dataset group to describe.
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
-- 'datasetGroupArn', 'describeDatasetGroup_datasetGroupArn' - The Amazon Resource Name (ARN) of the dataset group to describe.
newDescribeDatasetGroup ::
  -- | 'datasetGroupArn'
  Prelude.Text ->
  DescribeDatasetGroup
newDescribeDatasetGroup pDatasetGroupArn_ =
  DescribeDatasetGroup'
    { datasetGroupArn =
        pDatasetGroupArn_
    }

-- | The Amazon Resource Name (ARN) of the dataset group to describe.
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
            Prelude.<$> (x Data..?> "datasetGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDatasetGroup where
  hashWithSalt _salt DescribeDatasetGroup' {..} =
    _salt `Prelude.hashWithSalt` datasetGroupArn

instance Prelude.NFData DescribeDatasetGroup where
  rnf DescribeDatasetGroup' {..} =
    Prelude.rnf datasetGroupArn

instance Data.ToHeaders DescribeDatasetGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.DescribeDatasetGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeDatasetGroup where
  toJSON DescribeDatasetGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("datasetGroupArn" Data..= datasetGroupArn)
          ]
      )

instance Data.ToPath DescribeDatasetGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDatasetGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDatasetGroupResponse' smart constructor.
data DescribeDatasetGroupResponse = DescribeDatasetGroupResponse'
  { -- | A listing of the dataset group\'s properties.
    datasetGroup :: Prelude.Maybe DatasetGroup,
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
-- 'datasetGroup', 'describeDatasetGroupResponse_datasetGroup' - A listing of the dataset group\'s properties.
--
-- 'httpStatus', 'describeDatasetGroupResponse_httpStatus' - The response's http status code.
newDescribeDatasetGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDatasetGroupResponse
newDescribeDatasetGroupResponse pHttpStatus_ =
  DescribeDatasetGroupResponse'
    { datasetGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A listing of the dataset group\'s properties.
describeDatasetGroupResponse_datasetGroup :: Lens.Lens' DescribeDatasetGroupResponse (Prelude.Maybe DatasetGroup)
describeDatasetGroupResponse_datasetGroup = Lens.lens (\DescribeDatasetGroupResponse' {datasetGroup} -> datasetGroup) (\s@DescribeDatasetGroupResponse' {} a -> s {datasetGroup = a} :: DescribeDatasetGroupResponse)

-- | The response's http status code.
describeDatasetGroupResponse_httpStatus :: Lens.Lens' DescribeDatasetGroupResponse Prelude.Int
describeDatasetGroupResponse_httpStatus = Lens.lens (\DescribeDatasetGroupResponse' {httpStatus} -> httpStatus) (\s@DescribeDatasetGroupResponse' {} a -> s {httpStatus = a} :: DescribeDatasetGroupResponse)

instance Prelude.NFData DescribeDatasetGroupResponse where
  rnf DescribeDatasetGroupResponse' {..} =
    Prelude.rnf datasetGroup `Prelude.seq`
      Prelude.rnf httpStatus
