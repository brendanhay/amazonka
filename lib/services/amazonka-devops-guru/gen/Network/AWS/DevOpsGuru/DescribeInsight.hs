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
-- Module      : Network.AWS.DevOpsGuru.DescribeInsight
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about an insight that you specify using its ID.
module Network.AWS.DevOpsGuru.DescribeInsight
  ( -- * Creating a Request
    DescribeInsight (..),
    newDescribeInsight,

    -- * Request Lenses
    describeInsight_id,

    -- * Destructuring the Response
    DescribeInsightResponse (..),
    newDescribeInsightResponse,

    -- * Response Lenses
    describeInsightResponse_proactiveInsight,
    describeInsightResponse_reactiveInsight,
    describeInsightResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DevOpsGuru.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeInsight' smart constructor.
data DescribeInsight = DescribeInsight'
  { -- | The ID of the insight.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInsight' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'describeInsight_id' - The ID of the insight.
newDescribeInsight ::
  -- | 'id'
  Prelude.Text ->
  DescribeInsight
newDescribeInsight pId_ = DescribeInsight' {id = pId_}

-- | The ID of the insight.
describeInsight_id :: Lens.Lens' DescribeInsight Prelude.Text
describeInsight_id = Lens.lens (\DescribeInsight' {id} -> id) (\s@DescribeInsight' {} a -> s {id = a} :: DescribeInsight)

instance Core.AWSRequest DescribeInsight where
  type
    AWSResponse DescribeInsight =
      DescribeInsightResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInsightResponse'
            Prelude.<$> (x Core..?> "ProactiveInsight")
            Prelude.<*> (x Core..?> "ReactiveInsight")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInsight

instance Prelude.NFData DescribeInsight

instance Core.ToHeaders DescribeInsight where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeInsight where
  toPath DescribeInsight' {..} =
    Prelude.mconcat ["/insights/", Core.toBS id]

instance Core.ToQuery DescribeInsight where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeInsightResponse' smart constructor.
data DescribeInsightResponse = DescribeInsightResponse'
  { -- | A @ProactiveInsight@ object that represents the requested insight.
    proactiveInsight :: Prelude.Maybe ProactiveInsight,
    -- | A @ReactiveInsight@ object that represents the requested insight.
    reactiveInsight :: Prelude.Maybe ReactiveInsight,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInsightResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'proactiveInsight', 'describeInsightResponse_proactiveInsight' - A @ProactiveInsight@ object that represents the requested insight.
--
-- 'reactiveInsight', 'describeInsightResponse_reactiveInsight' - A @ReactiveInsight@ object that represents the requested insight.
--
-- 'httpStatus', 'describeInsightResponse_httpStatus' - The response's http status code.
newDescribeInsightResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInsightResponse
newDescribeInsightResponse pHttpStatus_ =
  DescribeInsightResponse'
    { proactiveInsight =
        Prelude.Nothing,
      reactiveInsight = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A @ProactiveInsight@ object that represents the requested insight.
describeInsightResponse_proactiveInsight :: Lens.Lens' DescribeInsightResponse (Prelude.Maybe ProactiveInsight)
describeInsightResponse_proactiveInsight = Lens.lens (\DescribeInsightResponse' {proactiveInsight} -> proactiveInsight) (\s@DescribeInsightResponse' {} a -> s {proactiveInsight = a} :: DescribeInsightResponse)

-- | A @ReactiveInsight@ object that represents the requested insight.
describeInsightResponse_reactiveInsight :: Lens.Lens' DescribeInsightResponse (Prelude.Maybe ReactiveInsight)
describeInsightResponse_reactiveInsight = Lens.lens (\DescribeInsightResponse' {reactiveInsight} -> reactiveInsight) (\s@DescribeInsightResponse' {} a -> s {reactiveInsight = a} :: DescribeInsightResponse)

-- | The response's http status code.
describeInsightResponse_httpStatus :: Lens.Lens' DescribeInsightResponse Prelude.Int
describeInsightResponse_httpStatus = Lens.lens (\DescribeInsightResponse' {httpStatus} -> httpStatus) (\s@DescribeInsightResponse' {} a -> s {httpStatus = a} :: DescribeInsightResponse)

instance Prelude.NFData DescribeInsightResponse
