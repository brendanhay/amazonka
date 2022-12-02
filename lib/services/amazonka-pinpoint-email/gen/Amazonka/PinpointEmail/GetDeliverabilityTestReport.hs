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
-- Module      : Amazonka.PinpointEmail.GetDeliverabilityTestReport
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve the results of a predictive inbox placement test.
module Amazonka.PinpointEmail.GetDeliverabilityTestReport
  ( -- * Creating a Request
    GetDeliverabilityTestReport (..),
    newGetDeliverabilityTestReport,

    -- * Request Lenses
    getDeliverabilityTestReport_reportId,

    -- * Destructuring the Response
    GetDeliverabilityTestReportResponse (..),
    newGetDeliverabilityTestReportResponse,

    -- * Response Lenses
    getDeliverabilityTestReportResponse_tags,
    getDeliverabilityTestReportResponse_message,
    getDeliverabilityTestReportResponse_httpStatus,
    getDeliverabilityTestReportResponse_deliverabilityTestReport,
    getDeliverabilityTestReportResponse_overallPlacement,
    getDeliverabilityTestReportResponse_ispPlacements,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointEmail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to retrieve the results of a predictive inbox placement test.
--
-- /See:/ 'newGetDeliverabilityTestReport' smart constructor.
data GetDeliverabilityTestReport = GetDeliverabilityTestReport'
  { -- | A unique string that identifies the predictive inbox placement test.
    reportId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDeliverabilityTestReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportId', 'getDeliverabilityTestReport_reportId' - A unique string that identifies the predictive inbox placement test.
newGetDeliverabilityTestReport ::
  -- | 'reportId'
  Prelude.Text ->
  GetDeliverabilityTestReport
newGetDeliverabilityTestReport pReportId_ =
  GetDeliverabilityTestReport' {reportId = pReportId_}

-- | A unique string that identifies the predictive inbox placement test.
getDeliverabilityTestReport_reportId :: Lens.Lens' GetDeliverabilityTestReport Prelude.Text
getDeliverabilityTestReport_reportId = Lens.lens (\GetDeliverabilityTestReport' {reportId} -> reportId) (\s@GetDeliverabilityTestReport' {} a -> s {reportId = a} :: GetDeliverabilityTestReport)

instance Core.AWSRequest GetDeliverabilityTestReport where
  type
    AWSResponse GetDeliverabilityTestReport =
      GetDeliverabilityTestReportResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeliverabilityTestReportResponse'
            Prelude.<$> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Message")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "DeliverabilityTestReport")
            Prelude.<*> (x Data..:> "OverallPlacement")
            Prelude.<*> (x Data..?> "IspPlacements" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable GetDeliverabilityTestReport where
  hashWithSalt _salt GetDeliverabilityTestReport' {..} =
    _salt `Prelude.hashWithSalt` reportId

instance Prelude.NFData GetDeliverabilityTestReport where
  rnf GetDeliverabilityTestReport' {..} =
    Prelude.rnf reportId

instance Data.ToHeaders GetDeliverabilityTestReport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetDeliverabilityTestReport where
  toPath GetDeliverabilityTestReport' {..} =
    Prelude.mconcat
      [ "/v1/email/deliverability-dashboard/test-reports/",
        Data.toBS reportId
      ]

instance Data.ToQuery GetDeliverabilityTestReport where
  toQuery = Prelude.const Prelude.mempty

-- | The results of the predictive inbox placement test.
--
-- /See:/ 'newGetDeliverabilityTestReportResponse' smart constructor.
data GetDeliverabilityTestReportResponse = GetDeliverabilityTestReportResponse'
  { -- | An array of objects that define the tags (keys and values) that are
    -- associated with the predictive inbox placement test.
    tags :: Prelude.Maybe [Tag],
    -- | An object that contains the message that you sent when you performed
    -- this predictive inbox placement test.
    message :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An object that contains the results of the predictive inbox placement
    -- test.
    deliverabilityTestReport :: DeliverabilityTestReport,
    -- | An object that specifies how many test messages that were sent during
    -- the predictive inbox placement test were delivered to recipients\'
    -- inboxes, how many were sent to recipients\' spam folders, and how many
    -- weren\'t delivered.
    overallPlacement :: PlacementStatistics,
    -- | An object that describes how the test email was handled by several email
    -- providers, including Gmail, Hotmail, Yahoo, AOL, and others.
    ispPlacements :: [IspPlacement]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDeliverabilityTestReportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'getDeliverabilityTestReportResponse_tags' - An array of objects that define the tags (keys and values) that are
-- associated with the predictive inbox placement test.
--
-- 'message', 'getDeliverabilityTestReportResponse_message' - An object that contains the message that you sent when you performed
-- this predictive inbox placement test.
--
-- 'httpStatus', 'getDeliverabilityTestReportResponse_httpStatus' - The response's http status code.
--
-- 'deliverabilityTestReport', 'getDeliverabilityTestReportResponse_deliverabilityTestReport' - An object that contains the results of the predictive inbox placement
-- test.
--
-- 'overallPlacement', 'getDeliverabilityTestReportResponse_overallPlacement' - An object that specifies how many test messages that were sent during
-- the predictive inbox placement test were delivered to recipients\'
-- inboxes, how many were sent to recipients\' spam folders, and how many
-- weren\'t delivered.
--
-- 'ispPlacements', 'getDeliverabilityTestReportResponse_ispPlacements' - An object that describes how the test email was handled by several email
-- providers, including Gmail, Hotmail, Yahoo, AOL, and others.
newGetDeliverabilityTestReportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'deliverabilityTestReport'
  DeliverabilityTestReport ->
  -- | 'overallPlacement'
  PlacementStatistics ->
  GetDeliverabilityTestReportResponse
newGetDeliverabilityTestReportResponse
  pHttpStatus_
  pDeliverabilityTestReport_
  pOverallPlacement_ =
    GetDeliverabilityTestReportResponse'
      { tags =
          Prelude.Nothing,
        message = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        deliverabilityTestReport =
          pDeliverabilityTestReport_,
        overallPlacement = pOverallPlacement_,
        ispPlacements = Prelude.mempty
      }

-- | An array of objects that define the tags (keys and values) that are
-- associated with the predictive inbox placement test.
getDeliverabilityTestReportResponse_tags :: Lens.Lens' GetDeliverabilityTestReportResponse (Prelude.Maybe [Tag])
getDeliverabilityTestReportResponse_tags = Lens.lens (\GetDeliverabilityTestReportResponse' {tags} -> tags) (\s@GetDeliverabilityTestReportResponse' {} a -> s {tags = a} :: GetDeliverabilityTestReportResponse) Prelude.. Lens.mapping Lens.coerced

-- | An object that contains the message that you sent when you performed
-- this predictive inbox placement test.
getDeliverabilityTestReportResponse_message :: Lens.Lens' GetDeliverabilityTestReportResponse (Prelude.Maybe Prelude.Text)
getDeliverabilityTestReportResponse_message = Lens.lens (\GetDeliverabilityTestReportResponse' {message} -> message) (\s@GetDeliverabilityTestReportResponse' {} a -> s {message = a} :: GetDeliverabilityTestReportResponse)

-- | The response's http status code.
getDeliverabilityTestReportResponse_httpStatus :: Lens.Lens' GetDeliverabilityTestReportResponse Prelude.Int
getDeliverabilityTestReportResponse_httpStatus = Lens.lens (\GetDeliverabilityTestReportResponse' {httpStatus} -> httpStatus) (\s@GetDeliverabilityTestReportResponse' {} a -> s {httpStatus = a} :: GetDeliverabilityTestReportResponse)

-- | An object that contains the results of the predictive inbox placement
-- test.
getDeliverabilityTestReportResponse_deliverabilityTestReport :: Lens.Lens' GetDeliverabilityTestReportResponse DeliverabilityTestReport
getDeliverabilityTestReportResponse_deliverabilityTestReport = Lens.lens (\GetDeliverabilityTestReportResponse' {deliverabilityTestReport} -> deliverabilityTestReport) (\s@GetDeliverabilityTestReportResponse' {} a -> s {deliverabilityTestReport = a} :: GetDeliverabilityTestReportResponse)

-- | An object that specifies how many test messages that were sent during
-- the predictive inbox placement test were delivered to recipients\'
-- inboxes, how many were sent to recipients\' spam folders, and how many
-- weren\'t delivered.
getDeliverabilityTestReportResponse_overallPlacement :: Lens.Lens' GetDeliverabilityTestReportResponse PlacementStatistics
getDeliverabilityTestReportResponse_overallPlacement = Lens.lens (\GetDeliverabilityTestReportResponse' {overallPlacement} -> overallPlacement) (\s@GetDeliverabilityTestReportResponse' {} a -> s {overallPlacement = a} :: GetDeliverabilityTestReportResponse)

-- | An object that describes how the test email was handled by several email
-- providers, including Gmail, Hotmail, Yahoo, AOL, and others.
getDeliverabilityTestReportResponse_ispPlacements :: Lens.Lens' GetDeliverabilityTestReportResponse [IspPlacement]
getDeliverabilityTestReportResponse_ispPlacements = Lens.lens (\GetDeliverabilityTestReportResponse' {ispPlacements} -> ispPlacements) (\s@GetDeliverabilityTestReportResponse' {} a -> s {ispPlacements = a} :: GetDeliverabilityTestReportResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    GetDeliverabilityTestReportResponse
  where
  rnf GetDeliverabilityTestReportResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf deliverabilityTestReport
      `Prelude.seq` Prelude.rnf overallPlacement
      `Prelude.seq` Prelude.rnf ispPlacements
