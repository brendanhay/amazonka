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
-- Module      : Amazonka.SESV2.CreateDeliverabilityTestReport
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new predictive inbox placement test. Predictive inbox placement
-- tests can help you predict how your messages will be handled by various
-- email providers around the world. When you perform a predictive inbox
-- placement test, you provide a sample message that contains the content
-- that you plan to send to your customers. Amazon SES then sends that
-- message to special email addresses spread across several major email
-- providers. After about 24 hours, the test is complete, and you can use
-- the @GetDeliverabilityTestReport@ operation to view the results of the
-- test.
module Amazonka.SESV2.CreateDeliverabilityTestReport
  ( -- * Creating a Request
    CreateDeliverabilityTestReport (..),
    newCreateDeliverabilityTestReport,

    -- * Request Lenses
    createDeliverabilityTestReport_reportName,
    createDeliverabilityTestReport_tags,
    createDeliverabilityTestReport_fromEmailAddress,
    createDeliverabilityTestReport_content,

    -- * Destructuring the Response
    CreateDeliverabilityTestReportResponse (..),
    newCreateDeliverabilityTestReportResponse,

    -- * Response Lenses
    createDeliverabilityTestReportResponse_httpStatus,
    createDeliverabilityTestReportResponse_reportId,
    createDeliverabilityTestReportResponse_deliverabilityTestStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | A request to perform a predictive inbox placement test. Predictive inbox
-- placement tests can help you predict how your messages will be handled
-- by various email providers around the world. When you perform a
-- predictive inbox placement test, you provide a sample message that
-- contains the content that you plan to send to your customers. We send
-- that message to special email addresses spread across several major
-- email providers around the world. The test takes about 24 hours to
-- complete. When the test is complete, you can use the
-- @GetDeliverabilityTestReport@ operation to view the results of the test.
--
-- /See:/ 'newCreateDeliverabilityTestReport' smart constructor.
data CreateDeliverabilityTestReport = CreateDeliverabilityTestReport'
  { -- | A unique name that helps you to identify the predictive inbox placement
    -- test when you retrieve the results.
    reportName :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that define the tags (keys and values) that you want
    -- to associate with the predictive inbox placement test.
    tags :: Prelude.Maybe [Tag],
    -- | The email address that the predictive inbox placement test email was
    -- sent from.
    fromEmailAddress :: Prelude.Text,
    -- | The HTML body of the message that you sent when you performed the
    -- predictive inbox placement test.
    content :: EmailContent
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDeliverabilityTestReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportName', 'createDeliverabilityTestReport_reportName' - A unique name that helps you to identify the predictive inbox placement
-- test when you retrieve the results.
--
-- 'tags', 'createDeliverabilityTestReport_tags' - An array of objects that define the tags (keys and values) that you want
-- to associate with the predictive inbox placement test.
--
-- 'fromEmailAddress', 'createDeliverabilityTestReport_fromEmailAddress' - The email address that the predictive inbox placement test email was
-- sent from.
--
-- 'content', 'createDeliverabilityTestReport_content' - The HTML body of the message that you sent when you performed the
-- predictive inbox placement test.
newCreateDeliverabilityTestReport ::
  -- | 'fromEmailAddress'
  Prelude.Text ->
  -- | 'content'
  EmailContent ->
  CreateDeliverabilityTestReport
newCreateDeliverabilityTestReport
  pFromEmailAddress_
  pContent_ =
    CreateDeliverabilityTestReport'
      { reportName =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        fromEmailAddress = pFromEmailAddress_,
        content = pContent_
      }

-- | A unique name that helps you to identify the predictive inbox placement
-- test when you retrieve the results.
createDeliverabilityTestReport_reportName :: Lens.Lens' CreateDeliverabilityTestReport (Prelude.Maybe Prelude.Text)
createDeliverabilityTestReport_reportName = Lens.lens (\CreateDeliverabilityTestReport' {reportName} -> reportName) (\s@CreateDeliverabilityTestReport' {} a -> s {reportName = a} :: CreateDeliverabilityTestReport)

-- | An array of objects that define the tags (keys and values) that you want
-- to associate with the predictive inbox placement test.
createDeliverabilityTestReport_tags :: Lens.Lens' CreateDeliverabilityTestReport (Prelude.Maybe [Tag])
createDeliverabilityTestReport_tags = Lens.lens (\CreateDeliverabilityTestReport' {tags} -> tags) (\s@CreateDeliverabilityTestReport' {} a -> s {tags = a} :: CreateDeliverabilityTestReport) Prelude.. Lens.mapping Lens.coerced

-- | The email address that the predictive inbox placement test email was
-- sent from.
createDeliverabilityTestReport_fromEmailAddress :: Lens.Lens' CreateDeliverabilityTestReport Prelude.Text
createDeliverabilityTestReport_fromEmailAddress = Lens.lens (\CreateDeliverabilityTestReport' {fromEmailAddress} -> fromEmailAddress) (\s@CreateDeliverabilityTestReport' {} a -> s {fromEmailAddress = a} :: CreateDeliverabilityTestReport)

-- | The HTML body of the message that you sent when you performed the
-- predictive inbox placement test.
createDeliverabilityTestReport_content :: Lens.Lens' CreateDeliverabilityTestReport EmailContent
createDeliverabilityTestReport_content = Lens.lens (\CreateDeliverabilityTestReport' {content} -> content) (\s@CreateDeliverabilityTestReport' {} a -> s {content = a} :: CreateDeliverabilityTestReport)

instance
  Core.AWSRequest
    CreateDeliverabilityTestReport
  where
  type
    AWSResponse CreateDeliverabilityTestReport =
      CreateDeliverabilityTestReportResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDeliverabilityTestReportResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ReportId")
            Prelude.<*> (x Data..:> "DeliverabilityTestStatus")
      )

instance
  Prelude.Hashable
    CreateDeliverabilityTestReport
  where
  hashWithSalt
    _salt
    CreateDeliverabilityTestReport' {..} =
      _salt `Prelude.hashWithSalt` reportName
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` fromEmailAddress
        `Prelude.hashWithSalt` content

instance
  Prelude.NFData
    CreateDeliverabilityTestReport
  where
  rnf CreateDeliverabilityTestReport' {..} =
    Prelude.rnf reportName
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf fromEmailAddress
      `Prelude.seq` Prelude.rnf content

instance
  Data.ToHeaders
    CreateDeliverabilityTestReport
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDeliverabilityTestReport where
  toJSON CreateDeliverabilityTestReport' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ReportName" Data..=) Prelude.<$> reportName,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("FromEmailAddress" Data..= fromEmailAddress),
            Prelude.Just ("Content" Data..= content)
          ]
      )

instance Data.ToPath CreateDeliverabilityTestReport where
  toPath =
    Prelude.const
      "/v2/email/deliverability-dashboard/test"

instance Data.ToQuery CreateDeliverabilityTestReport where
  toQuery = Prelude.const Prelude.mempty

-- | Information about the predictive inbox placement test that you created.
--
-- /See:/ 'newCreateDeliverabilityTestReportResponse' smart constructor.
data CreateDeliverabilityTestReportResponse = CreateDeliverabilityTestReportResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A unique string that identifies the predictive inbox placement test.
    reportId :: Prelude.Text,
    -- | The status of the predictive inbox placement test. If the status is
    -- @IN_PROGRESS@, then the predictive inbox placement test is currently
    -- running. Predictive inbox placement tests are usually complete within 24
    -- hours of creating the test. If the status is @COMPLETE@, then the test
    -- is finished, and you can use the @GetDeliverabilityTestReport@ to view
    -- the results of the test.
    deliverabilityTestStatus :: DeliverabilityTestStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDeliverabilityTestReportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createDeliverabilityTestReportResponse_httpStatus' - The response's http status code.
--
-- 'reportId', 'createDeliverabilityTestReportResponse_reportId' - A unique string that identifies the predictive inbox placement test.
--
-- 'deliverabilityTestStatus', 'createDeliverabilityTestReportResponse_deliverabilityTestStatus' - The status of the predictive inbox placement test. If the status is
-- @IN_PROGRESS@, then the predictive inbox placement test is currently
-- running. Predictive inbox placement tests are usually complete within 24
-- hours of creating the test. If the status is @COMPLETE@, then the test
-- is finished, and you can use the @GetDeliverabilityTestReport@ to view
-- the results of the test.
newCreateDeliverabilityTestReportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'reportId'
  Prelude.Text ->
  -- | 'deliverabilityTestStatus'
  DeliverabilityTestStatus ->
  CreateDeliverabilityTestReportResponse
newCreateDeliverabilityTestReportResponse
  pHttpStatus_
  pReportId_
  pDeliverabilityTestStatus_ =
    CreateDeliverabilityTestReportResponse'
      { httpStatus =
          pHttpStatus_,
        reportId = pReportId_,
        deliverabilityTestStatus =
          pDeliverabilityTestStatus_
      }

-- | The response's http status code.
createDeliverabilityTestReportResponse_httpStatus :: Lens.Lens' CreateDeliverabilityTestReportResponse Prelude.Int
createDeliverabilityTestReportResponse_httpStatus = Lens.lens (\CreateDeliverabilityTestReportResponse' {httpStatus} -> httpStatus) (\s@CreateDeliverabilityTestReportResponse' {} a -> s {httpStatus = a} :: CreateDeliverabilityTestReportResponse)

-- | A unique string that identifies the predictive inbox placement test.
createDeliverabilityTestReportResponse_reportId :: Lens.Lens' CreateDeliverabilityTestReportResponse Prelude.Text
createDeliverabilityTestReportResponse_reportId = Lens.lens (\CreateDeliverabilityTestReportResponse' {reportId} -> reportId) (\s@CreateDeliverabilityTestReportResponse' {} a -> s {reportId = a} :: CreateDeliverabilityTestReportResponse)

-- | The status of the predictive inbox placement test. If the status is
-- @IN_PROGRESS@, then the predictive inbox placement test is currently
-- running. Predictive inbox placement tests are usually complete within 24
-- hours of creating the test. If the status is @COMPLETE@, then the test
-- is finished, and you can use the @GetDeliverabilityTestReport@ to view
-- the results of the test.
createDeliverabilityTestReportResponse_deliverabilityTestStatus :: Lens.Lens' CreateDeliverabilityTestReportResponse DeliverabilityTestStatus
createDeliverabilityTestReportResponse_deliverabilityTestStatus = Lens.lens (\CreateDeliverabilityTestReportResponse' {deliverabilityTestStatus} -> deliverabilityTestStatus) (\s@CreateDeliverabilityTestReportResponse' {} a -> s {deliverabilityTestStatus = a} :: CreateDeliverabilityTestReportResponse)

instance
  Prelude.NFData
    CreateDeliverabilityTestReportResponse
  where
  rnf CreateDeliverabilityTestReportResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf reportId
      `Prelude.seq` Prelude.rnf deliverabilityTestStatus
