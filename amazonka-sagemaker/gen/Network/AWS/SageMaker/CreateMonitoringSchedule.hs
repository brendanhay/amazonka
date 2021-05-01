{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.CreateMonitoringSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a schedule that regularly starts Amazon SageMaker Processing
-- Jobs to monitor the data captured for an Amazon SageMaker Endoint.
module Network.AWS.SageMaker.CreateMonitoringSchedule
  ( -- * Creating a Request
    CreateMonitoringSchedule (..),
    newCreateMonitoringSchedule,

    -- * Request Lenses
    createMonitoringSchedule_tags,
    createMonitoringSchedule_monitoringScheduleName,
    createMonitoringSchedule_monitoringScheduleConfig,

    -- * Destructuring the Response
    CreateMonitoringScheduleResponse (..),
    newCreateMonitoringScheduleResponse,

    -- * Response Lenses
    createMonitoringScheduleResponse_httpStatus,
    createMonitoringScheduleResponse_monitoringScheduleArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateMonitoringSchedule' smart constructor.
data CreateMonitoringSchedule = CreateMonitoringSchedule'
  { -- | (Optional) An array of key-value pairs. For more information, see
    -- <%20https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
    -- in the /AWS Billing and Cost Management User Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the monitoring schedule. The name must be unique within an
    -- AWS Region within an AWS account.
    monitoringScheduleName :: Prelude.Text,
    -- | The configuration object that specifies the monitoring schedule and
    -- defines the monitoring job.
    monitoringScheduleConfig :: MonitoringScheduleConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateMonitoringSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createMonitoringSchedule_tags' - (Optional) An array of key-value pairs. For more information, see
-- <%20https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
-- in the /AWS Billing and Cost Management User Guide/.
--
-- 'monitoringScheduleName', 'createMonitoringSchedule_monitoringScheduleName' - The name of the monitoring schedule. The name must be unique within an
-- AWS Region within an AWS account.
--
-- 'monitoringScheduleConfig', 'createMonitoringSchedule_monitoringScheduleConfig' - The configuration object that specifies the monitoring schedule and
-- defines the monitoring job.
newCreateMonitoringSchedule ::
  -- | 'monitoringScheduleName'
  Prelude.Text ->
  -- | 'monitoringScheduleConfig'
  MonitoringScheduleConfig ->
  CreateMonitoringSchedule
newCreateMonitoringSchedule
  pMonitoringScheduleName_
  pMonitoringScheduleConfig_ =
    CreateMonitoringSchedule'
      { tags = Prelude.Nothing,
        monitoringScheduleName = pMonitoringScheduleName_,
        monitoringScheduleConfig =
          pMonitoringScheduleConfig_
      }

-- | (Optional) An array of key-value pairs. For more information, see
-- <%20https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html#allocation-whatURL Using Cost Allocation Tags>
-- in the /AWS Billing and Cost Management User Guide/.
createMonitoringSchedule_tags :: Lens.Lens' CreateMonitoringSchedule (Prelude.Maybe [Tag])
createMonitoringSchedule_tags = Lens.lens (\CreateMonitoringSchedule' {tags} -> tags) (\s@CreateMonitoringSchedule' {} a -> s {tags = a} :: CreateMonitoringSchedule) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the monitoring schedule. The name must be unique within an
-- AWS Region within an AWS account.
createMonitoringSchedule_monitoringScheduleName :: Lens.Lens' CreateMonitoringSchedule Prelude.Text
createMonitoringSchedule_monitoringScheduleName = Lens.lens (\CreateMonitoringSchedule' {monitoringScheduleName} -> monitoringScheduleName) (\s@CreateMonitoringSchedule' {} a -> s {monitoringScheduleName = a} :: CreateMonitoringSchedule)

-- | The configuration object that specifies the monitoring schedule and
-- defines the monitoring job.
createMonitoringSchedule_monitoringScheduleConfig :: Lens.Lens' CreateMonitoringSchedule MonitoringScheduleConfig
createMonitoringSchedule_monitoringScheduleConfig = Lens.lens (\CreateMonitoringSchedule' {monitoringScheduleConfig} -> monitoringScheduleConfig) (\s@CreateMonitoringSchedule' {} a -> s {monitoringScheduleConfig = a} :: CreateMonitoringSchedule)

instance Prelude.AWSRequest CreateMonitoringSchedule where
  type
    Rs CreateMonitoringSchedule =
      CreateMonitoringScheduleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMonitoringScheduleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "MonitoringScheduleArn")
      )

instance Prelude.Hashable CreateMonitoringSchedule

instance Prelude.NFData CreateMonitoringSchedule

instance Prelude.ToHeaders CreateMonitoringSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.CreateMonitoringSchedule" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateMonitoringSchedule where
  toJSON CreateMonitoringSchedule' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Tags" Prelude..=) Prelude.<$> tags,
            Prelude.Just
              ( "MonitoringScheduleName"
                  Prelude..= monitoringScheduleName
              ),
            Prelude.Just
              ( "MonitoringScheduleConfig"
                  Prelude..= monitoringScheduleConfig
              )
          ]
      )

instance Prelude.ToPath CreateMonitoringSchedule where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateMonitoringSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateMonitoringScheduleResponse' smart constructor.
data CreateMonitoringScheduleResponse = CreateMonitoringScheduleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the monitoring schedule.
    monitoringScheduleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateMonitoringScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createMonitoringScheduleResponse_httpStatus' - The response's http status code.
--
-- 'monitoringScheduleArn', 'createMonitoringScheduleResponse_monitoringScheduleArn' - The Amazon Resource Name (ARN) of the monitoring schedule.
newCreateMonitoringScheduleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'monitoringScheduleArn'
  Prelude.Text ->
  CreateMonitoringScheduleResponse
newCreateMonitoringScheduleResponse
  pHttpStatus_
  pMonitoringScheduleArn_ =
    CreateMonitoringScheduleResponse'
      { httpStatus =
          pHttpStatus_,
        monitoringScheduleArn =
          pMonitoringScheduleArn_
      }

-- | The response's http status code.
createMonitoringScheduleResponse_httpStatus :: Lens.Lens' CreateMonitoringScheduleResponse Prelude.Int
createMonitoringScheduleResponse_httpStatus = Lens.lens (\CreateMonitoringScheduleResponse' {httpStatus} -> httpStatus) (\s@CreateMonitoringScheduleResponse' {} a -> s {httpStatus = a} :: CreateMonitoringScheduleResponse)

-- | The Amazon Resource Name (ARN) of the monitoring schedule.
createMonitoringScheduleResponse_monitoringScheduleArn :: Lens.Lens' CreateMonitoringScheduleResponse Prelude.Text
createMonitoringScheduleResponse_monitoringScheduleArn = Lens.lens (\CreateMonitoringScheduleResponse' {monitoringScheduleArn} -> monitoringScheduleArn) (\s@CreateMonitoringScheduleResponse' {} a -> s {monitoringScheduleArn = a} :: CreateMonitoringScheduleResponse)

instance
  Prelude.NFData
    CreateMonitoringScheduleResponse
