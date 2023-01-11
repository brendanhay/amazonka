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
-- Module      : Amazonka.SageMaker.DescribeHumanTaskUi
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the requested human task user interface
-- (worker task template).
module Amazonka.SageMaker.DescribeHumanTaskUi
  ( -- * Creating a Request
    DescribeHumanTaskUi (..),
    newDescribeHumanTaskUi,

    -- * Request Lenses
    describeHumanTaskUi_humanTaskUiName,

    -- * Destructuring the Response
    DescribeHumanTaskUiResponse (..),
    newDescribeHumanTaskUiResponse,

    -- * Response Lenses
    describeHumanTaskUiResponse_humanTaskUiStatus,
    describeHumanTaskUiResponse_httpStatus,
    describeHumanTaskUiResponse_humanTaskUiArn,
    describeHumanTaskUiResponse_humanTaskUiName,
    describeHumanTaskUiResponse_creationTime,
    describeHumanTaskUiResponse_uiTemplate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeHumanTaskUi' smart constructor.
data DescribeHumanTaskUi = DescribeHumanTaskUi'
  { -- | The name of the human task user interface (worker task template) you
    -- want information about.
    humanTaskUiName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeHumanTaskUi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'humanTaskUiName', 'describeHumanTaskUi_humanTaskUiName' - The name of the human task user interface (worker task template) you
-- want information about.
newDescribeHumanTaskUi ::
  -- | 'humanTaskUiName'
  Prelude.Text ->
  DescribeHumanTaskUi
newDescribeHumanTaskUi pHumanTaskUiName_ =
  DescribeHumanTaskUi'
    { humanTaskUiName =
        pHumanTaskUiName_
    }

-- | The name of the human task user interface (worker task template) you
-- want information about.
describeHumanTaskUi_humanTaskUiName :: Lens.Lens' DescribeHumanTaskUi Prelude.Text
describeHumanTaskUi_humanTaskUiName = Lens.lens (\DescribeHumanTaskUi' {humanTaskUiName} -> humanTaskUiName) (\s@DescribeHumanTaskUi' {} a -> s {humanTaskUiName = a} :: DescribeHumanTaskUi)

instance Core.AWSRequest DescribeHumanTaskUi where
  type
    AWSResponse DescribeHumanTaskUi =
      DescribeHumanTaskUiResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeHumanTaskUiResponse'
            Prelude.<$> (x Data..?> "HumanTaskUiStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "HumanTaskUiArn")
            Prelude.<*> (x Data..:> "HumanTaskUiName")
            Prelude.<*> (x Data..:> "CreationTime")
            Prelude.<*> (x Data..:> "UiTemplate")
      )

instance Prelude.Hashable DescribeHumanTaskUi where
  hashWithSalt _salt DescribeHumanTaskUi' {..} =
    _salt `Prelude.hashWithSalt` humanTaskUiName

instance Prelude.NFData DescribeHumanTaskUi where
  rnf DescribeHumanTaskUi' {..} =
    Prelude.rnf humanTaskUiName

instance Data.ToHeaders DescribeHumanTaskUi where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DescribeHumanTaskUi" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeHumanTaskUi where
  toJSON DescribeHumanTaskUi' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("HumanTaskUiName" Data..= humanTaskUiName)
          ]
      )

instance Data.ToPath DescribeHumanTaskUi where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeHumanTaskUi where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeHumanTaskUiResponse' smart constructor.
data DescribeHumanTaskUiResponse = DescribeHumanTaskUiResponse'
  { -- | The status of the human task user interface (worker task template).
    -- Valid values are listed below.
    humanTaskUiStatus :: Prelude.Maybe HumanTaskUiStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the human task user interface (worker
    -- task template).
    humanTaskUiArn :: Prelude.Text,
    -- | The name of the human task user interface (worker task template).
    humanTaskUiName :: Prelude.Text,
    -- | The timestamp when the human task user interface was created.
    creationTime :: Data.POSIX,
    uiTemplate :: UiTemplateInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeHumanTaskUiResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'humanTaskUiStatus', 'describeHumanTaskUiResponse_humanTaskUiStatus' - The status of the human task user interface (worker task template).
-- Valid values are listed below.
--
-- 'httpStatus', 'describeHumanTaskUiResponse_httpStatus' - The response's http status code.
--
-- 'humanTaskUiArn', 'describeHumanTaskUiResponse_humanTaskUiArn' - The Amazon Resource Name (ARN) of the human task user interface (worker
-- task template).
--
-- 'humanTaskUiName', 'describeHumanTaskUiResponse_humanTaskUiName' - The name of the human task user interface (worker task template).
--
-- 'creationTime', 'describeHumanTaskUiResponse_creationTime' - The timestamp when the human task user interface was created.
--
-- 'uiTemplate', 'describeHumanTaskUiResponse_uiTemplate' - Undocumented member.
newDescribeHumanTaskUiResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'humanTaskUiArn'
  Prelude.Text ->
  -- | 'humanTaskUiName'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'uiTemplate'
  UiTemplateInfo ->
  DescribeHumanTaskUiResponse
newDescribeHumanTaskUiResponse
  pHttpStatus_
  pHumanTaskUiArn_
  pHumanTaskUiName_
  pCreationTime_
  pUiTemplate_ =
    DescribeHumanTaskUiResponse'
      { humanTaskUiStatus =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        humanTaskUiArn = pHumanTaskUiArn_,
        humanTaskUiName = pHumanTaskUiName_,
        creationTime =
          Data._Time Lens.# pCreationTime_,
        uiTemplate = pUiTemplate_
      }

-- | The status of the human task user interface (worker task template).
-- Valid values are listed below.
describeHumanTaskUiResponse_humanTaskUiStatus :: Lens.Lens' DescribeHumanTaskUiResponse (Prelude.Maybe HumanTaskUiStatus)
describeHumanTaskUiResponse_humanTaskUiStatus = Lens.lens (\DescribeHumanTaskUiResponse' {humanTaskUiStatus} -> humanTaskUiStatus) (\s@DescribeHumanTaskUiResponse' {} a -> s {humanTaskUiStatus = a} :: DescribeHumanTaskUiResponse)

-- | The response's http status code.
describeHumanTaskUiResponse_httpStatus :: Lens.Lens' DescribeHumanTaskUiResponse Prelude.Int
describeHumanTaskUiResponse_httpStatus = Lens.lens (\DescribeHumanTaskUiResponse' {httpStatus} -> httpStatus) (\s@DescribeHumanTaskUiResponse' {} a -> s {httpStatus = a} :: DescribeHumanTaskUiResponse)

-- | The Amazon Resource Name (ARN) of the human task user interface (worker
-- task template).
describeHumanTaskUiResponse_humanTaskUiArn :: Lens.Lens' DescribeHumanTaskUiResponse Prelude.Text
describeHumanTaskUiResponse_humanTaskUiArn = Lens.lens (\DescribeHumanTaskUiResponse' {humanTaskUiArn} -> humanTaskUiArn) (\s@DescribeHumanTaskUiResponse' {} a -> s {humanTaskUiArn = a} :: DescribeHumanTaskUiResponse)

-- | The name of the human task user interface (worker task template).
describeHumanTaskUiResponse_humanTaskUiName :: Lens.Lens' DescribeHumanTaskUiResponse Prelude.Text
describeHumanTaskUiResponse_humanTaskUiName = Lens.lens (\DescribeHumanTaskUiResponse' {humanTaskUiName} -> humanTaskUiName) (\s@DescribeHumanTaskUiResponse' {} a -> s {humanTaskUiName = a} :: DescribeHumanTaskUiResponse)

-- | The timestamp when the human task user interface was created.
describeHumanTaskUiResponse_creationTime :: Lens.Lens' DescribeHumanTaskUiResponse Prelude.UTCTime
describeHumanTaskUiResponse_creationTime = Lens.lens (\DescribeHumanTaskUiResponse' {creationTime} -> creationTime) (\s@DescribeHumanTaskUiResponse' {} a -> s {creationTime = a} :: DescribeHumanTaskUiResponse) Prelude.. Data._Time

-- | Undocumented member.
describeHumanTaskUiResponse_uiTemplate :: Lens.Lens' DescribeHumanTaskUiResponse UiTemplateInfo
describeHumanTaskUiResponse_uiTemplate = Lens.lens (\DescribeHumanTaskUiResponse' {uiTemplate} -> uiTemplate) (\s@DescribeHumanTaskUiResponse' {} a -> s {uiTemplate = a} :: DescribeHumanTaskUiResponse)

instance Prelude.NFData DescribeHumanTaskUiResponse where
  rnf DescribeHumanTaskUiResponse' {..} =
    Prelude.rnf humanTaskUiStatus
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf humanTaskUiArn
      `Prelude.seq` Prelude.rnf humanTaskUiName
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf uiTemplate
