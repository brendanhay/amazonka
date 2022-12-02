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
-- Module      : Amazonka.DevOpsGuru.DescribeAnomaly
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about an anomaly that you specify using its ID.
module Amazonka.DevOpsGuru.DescribeAnomaly
  ( -- * Creating a Request
    DescribeAnomaly (..),
    newDescribeAnomaly,

    -- * Request Lenses
    describeAnomaly_accountId,
    describeAnomaly_id,

    -- * Destructuring the Response
    DescribeAnomalyResponse (..),
    newDescribeAnomalyResponse,

    -- * Response Lenses
    describeAnomalyResponse_reactiveAnomaly,
    describeAnomalyResponse_proactiveAnomaly,
    describeAnomalyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAnomaly' smart constructor.
data DescribeAnomaly = DescribeAnomaly'
  { -- | The ID of the member account.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the anomaly.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAnomaly' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'describeAnomaly_accountId' - The ID of the member account.
--
-- 'id', 'describeAnomaly_id' - The ID of the anomaly.
newDescribeAnomaly ::
  -- | 'id'
  Prelude.Text ->
  DescribeAnomaly
newDescribeAnomaly pId_ =
  DescribeAnomaly'
    { accountId = Prelude.Nothing,
      id = pId_
    }

-- | The ID of the member account.
describeAnomaly_accountId :: Lens.Lens' DescribeAnomaly (Prelude.Maybe Prelude.Text)
describeAnomaly_accountId = Lens.lens (\DescribeAnomaly' {accountId} -> accountId) (\s@DescribeAnomaly' {} a -> s {accountId = a} :: DescribeAnomaly)

-- | The ID of the anomaly.
describeAnomaly_id :: Lens.Lens' DescribeAnomaly Prelude.Text
describeAnomaly_id = Lens.lens (\DescribeAnomaly' {id} -> id) (\s@DescribeAnomaly' {} a -> s {id = a} :: DescribeAnomaly)

instance Core.AWSRequest DescribeAnomaly where
  type
    AWSResponse DescribeAnomaly =
      DescribeAnomalyResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAnomalyResponse'
            Prelude.<$> (x Data..?> "ReactiveAnomaly")
            Prelude.<*> (x Data..?> "ProactiveAnomaly")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAnomaly where
  hashWithSalt _salt DescribeAnomaly' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` id

instance Prelude.NFData DescribeAnomaly where
  rnf DescribeAnomaly' {..} =
    Prelude.rnf accountId `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders DescribeAnomaly where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeAnomaly where
  toPath DescribeAnomaly' {..} =
    Prelude.mconcat ["/anomalies/", Data.toBS id]

instance Data.ToQuery DescribeAnomaly where
  toQuery DescribeAnomaly' {..} =
    Prelude.mconcat ["AccountId" Data.=: accountId]

-- | /See:/ 'newDescribeAnomalyResponse' smart constructor.
data DescribeAnomalyResponse = DescribeAnomalyResponse'
  { -- | A @ReactiveAnomaly@ object that represents the requested anomaly.
    reactiveAnomaly :: Prelude.Maybe ReactiveAnomaly,
    -- | A @ProactiveAnomaly@ object that represents the requested anomaly.
    proactiveAnomaly :: Prelude.Maybe ProactiveAnomaly,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAnomalyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reactiveAnomaly', 'describeAnomalyResponse_reactiveAnomaly' - A @ReactiveAnomaly@ object that represents the requested anomaly.
--
-- 'proactiveAnomaly', 'describeAnomalyResponse_proactiveAnomaly' - A @ProactiveAnomaly@ object that represents the requested anomaly.
--
-- 'httpStatus', 'describeAnomalyResponse_httpStatus' - The response's http status code.
newDescribeAnomalyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAnomalyResponse
newDescribeAnomalyResponse pHttpStatus_ =
  DescribeAnomalyResponse'
    { reactiveAnomaly =
        Prelude.Nothing,
      proactiveAnomaly = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A @ReactiveAnomaly@ object that represents the requested anomaly.
describeAnomalyResponse_reactiveAnomaly :: Lens.Lens' DescribeAnomalyResponse (Prelude.Maybe ReactiveAnomaly)
describeAnomalyResponse_reactiveAnomaly = Lens.lens (\DescribeAnomalyResponse' {reactiveAnomaly} -> reactiveAnomaly) (\s@DescribeAnomalyResponse' {} a -> s {reactiveAnomaly = a} :: DescribeAnomalyResponse)

-- | A @ProactiveAnomaly@ object that represents the requested anomaly.
describeAnomalyResponse_proactiveAnomaly :: Lens.Lens' DescribeAnomalyResponse (Prelude.Maybe ProactiveAnomaly)
describeAnomalyResponse_proactiveAnomaly = Lens.lens (\DescribeAnomalyResponse' {proactiveAnomaly} -> proactiveAnomaly) (\s@DescribeAnomalyResponse' {} a -> s {proactiveAnomaly = a} :: DescribeAnomalyResponse)

-- | The response's http status code.
describeAnomalyResponse_httpStatus :: Lens.Lens' DescribeAnomalyResponse Prelude.Int
describeAnomalyResponse_httpStatus = Lens.lens (\DescribeAnomalyResponse' {httpStatus} -> httpStatus) (\s@DescribeAnomalyResponse' {} a -> s {httpStatus = a} :: DescribeAnomalyResponse)

instance Prelude.NFData DescribeAnomalyResponse where
  rnf DescribeAnomalyResponse' {..} =
    Prelude.rnf reactiveAnomaly
      `Prelude.seq` Prelude.rnf proactiveAnomaly
      `Prelude.seq` Prelude.rnf httpStatus
