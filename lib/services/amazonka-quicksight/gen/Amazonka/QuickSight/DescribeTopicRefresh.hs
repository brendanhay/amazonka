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
-- Module      : Amazonka.QuickSight.DescribeTopicRefresh
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the status of a topic refresh.
module Amazonka.QuickSight.DescribeTopicRefresh
  ( -- * Creating a Request
    DescribeTopicRefresh (..),
    newDescribeTopicRefresh,

    -- * Request Lenses
    describeTopicRefresh_awsAccountId,
    describeTopicRefresh_topicId,
    describeTopicRefresh_refreshId,

    -- * Destructuring the Response
    DescribeTopicRefreshResponse (..),
    newDescribeTopicRefreshResponse,

    -- * Response Lenses
    describeTopicRefreshResponse_refreshDetails,
    describeTopicRefreshResponse_requestId,
    describeTopicRefreshResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTopicRefresh' smart constructor.
data DescribeTopicRefresh = DescribeTopicRefresh'
  { -- | The ID of the Amazon Web Services account that contains the topic whose
    -- refresh you want to describe.
    awsAccountId :: Prelude.Text,
    -- | The ID of the topic that you want to describe. This ID is unique per
    -- Amazon Web Services Region for each Amazon Web Services account.
    topicId :: Prelude.Text,
    -- | The ID of the refresh, which is performed when the topic is created or
    -- updated.
    refreshId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTopicRefresh' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'describeTopicRefresh_awsAccountId' - The ID of the Amazon Web Services account that contains the topic whose
-- refresh you want to describe.
--
-- 'topicId', 'describeTopicRefresh_topicId' - The ID of the topic that you want to describe. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
--
-- 'refreshId', 'describeTopicRefresh_refreshId' - The ID of the refresh, which is performed when the topic is created or
-- updated.
newDescribeTopicRefresh ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'topicId'
  Prelude.Text ->
  -- | 'refreshId'
  Prelude.Text ->
  DescribeTopicRefresh
newDescribeTopicRefresh
  pAwsAccountId_
  pTopicId_
  pRefreshId_ =
    DescribeTopicRefresh'
      { awsAccountId =
          pAwsAccountId_,
        topicId = pTopicId_,
        refreshId = pRefreshId_
      }

-- | The ID of the Amazon Web Services account that contains the topic whose
-- refresh you want to describe.
describeTopicRefresh_awsAccountId :: Lens.Lens' DescribeTopicRefresh Prelude.Text
describeTopicRefresh_awsAccountId = Lens.lens (\DescribeTopicRefresh' {awsAccountId} -> awsAccountId) (\s@DescribeTopicRefresh' {} a -> s {awsAccountId = a} :: DescribeTopicRefresh)

-- | The ID of the topic that you want to describe. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
describeTopicRefresh_topicId :: Lens.Lens' DescribeTopicRefresh Prelude.Text
describeTopicRefresh_topicId = Lens.lens (\DescribeTopicRefresh' {topicId} -> topicId) (\s@DescribeTopicRefresh' {} a -> s {topicId = a} :: DescribeTopicRefresh)

-- | The ID of the refresh, which is performed when the topic is created or
-- updated.
describeTopicRefresh_refreshId :: Lens.Lens' DescribeTopicRefresh Prelude.Text
describeTopicRefresh_refreshId = Lens.lens (\DescribeTopicRefresh' {refreshId} -> refreshId) (\s@DescribeTopicRefresh' {} a -> s {refreshId = a} :: DescribeTopicRefresh)

instance Core.AWSRequest DescribeTopicRefresh where
  type
    AWSResponse DescribeTopicRefresh =
      DescribeTopicRefreshResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTopicRefreshResponse'
            Prelude.<$> (x Data..?> "RefreshDetails")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTopicRefresh where
  hashWithSalt _salt DescribeTopicRefresh' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` topicId
      `Prelude.hashWithSalt` refreshId

instance Prelude.NFData DescribeTopicRefresh where
  rnf DescribeTopicRefresh' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf topicId
      `Prelude.seq` Prelude.rnf refreshId

instance Data.ToHeaders DescribeTopicRefresh where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeTopicRefresh where
  toPath DescribeTopicRefresh' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/topics/",
        Data.toBS topicId,
        "/refresh/",
        Data.toBS refreshId
      ]

instance Data.ToQuery DescribeTopicRefresh where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTopicRefreshResponse' smart constructor.
data DescribeTopicRefreshResponse = DescribeTopicRefreshResponse'
  { -- | Details of the refresh, which is performed when the topic is created or
    -- updated.
    refreshDetails :: Prelude.Maybe TopicRefreshDetails,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTopicRefreshResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'refreshDetails', 'describeTopicRefreshResponse_refreshDetails' - Details of the refresh, which is performed when the topic is created or
-- updated.
--
-- 'requestId', 'describeTopicRefreshResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'describeTopicRefreshResponse_status' - The HTTP status of the request.
newDescribeTopicRefreshResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeTopicRefreshResponse
newDescribeTopicRefreshResponse pStatus_ =
  DescribeTopicRefreshResponse'
    { refreshDetails =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | Details of the refresh, which is performed when the topic is created or
-- updated.
describeTopicRefreshResponse_refreshDetails :: Lens.Lens' DescribeTopicRefreshResponse (Prelude.Maybe TopicRefreshDetails)
describeTopicRefreshResponse_refreshDetails = Lens.lens (\DescribeTopicRefreshResponse' {refreshDetails} -> refreshDetails) (\s@DescribeTopicRefreshResponse' {} a -> s {refreshDetails = a} :: DescribeTopicRefreshResponse)

-- | The Amazon Web Services request ID for this operation.
describeTopicRefreshResponse_requestId :: Lens.Lens' DescribeTopicRefreshResponse (Prelude.Maybe Prelude.Text)
describeTopicRefreshResponse_requestId = Lens.lens (\DescribeTopicRefreshResponse' {requestId} -> requestId) (\s@DescribeTopicRefreshResponse' {} a -> s {requestId = a} :: DescribeTopicRefreshResponse)

-- | The HTTP status of the request.
describeTopicRefreshResponse_status :: Lens.Lens' DescribeTopicRefreshResponse Prelude.Int
describeTopicRefreshResponse_status = Lens.lens (\DescribeTopicRefreshResponse' {status} -> status) (\s@DescribeTopicRefreshResponse' {} a -> s {status = a} :: DescribeTopicRefreshResponse)

instance Prelude.NFData DescribeTopicRefreshResponse where
  rnf DescribeTopicRefreshResponse' {..} =
    Prelude.rnf refreshDetails
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
