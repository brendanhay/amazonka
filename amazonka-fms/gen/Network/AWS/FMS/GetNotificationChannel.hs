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
-- Module      : Network.AWS.FMS.GetNotificationChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Information about the Amazon Simple Notification Service (SNS) topic
-- that is used to record AWS Firewall Manager SNS logs.
module Network.AWS.FMS.GetNotificationChannel
  ( -- * Creating a Request
    GetNotificationChannel (..),
    newGetNotificationChannel,

    -- * Destructuring the Response
    GetNotificationChannelResponse (..),
    newGetNotificationChannelResponse,

    -- * Response Lenses
    getNotificationChannelResponse_snsRoleName,
    getNotificationChannelResponse_snsTopicArn,
    getNotificationChannelResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetNotificationChannel' smart constructor.
data GetNotificationChannel = GetNotificationChannel'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetNotificationChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetNotificationChannel ::
  GetNotificationChannel
newGetNotificationChannel = GetNotificationChannel'

instance Core.AWSRequest GetNotificationChannel where
  type
    AWSResponse GetNotificationChannel =
      GetNotificationChannelResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetNotificationChannelResponse'
            Core.<$> (x Core..?> "SnsRoleName")
            Core.<*> (x Core..?> "SnsTopicArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetNotificationChannel

instance Core.NFData GetNotificationChannel

instance Core.ToHeaders GetNotificationChannel where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSFMS_20180101.GetNotificationChannel" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetNotificationChannel where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath GetNotificationChannel where
  toPath = Core.const "/"

instance Core.ToQuery GetNotificationChannel where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetNotificationChannelResponse' smart constructor.
data GetNotificationChannelResponse = GetNotificationChannelResponse'
  { -- | The IAM role that is used by AWS Firewall Manager to record activity to
    -- SNS.
    snsRoleName :: Core.Maybe Core.Text,
    -- | The SNS topic that records AWS Firewall Manager activity.
    snsTopicArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetNotificationChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snsRoleName', 'getNotificationChannelResponse_snsRoleName' - The IAM role that is used by AWS Firewall Manager to record activity to
-- SNS.
--
-- 'snsTopicArn', 'getNotificationChannelResponse_snsTopicArn' - The SNS topic that records AWS Firewall Manager activity.
--
-- 'httpStatus', 'getNotificationChannelResponse_httpStatus' - The response's http status code.
newGetNotificationChannelResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetNotificationChannelResponse
newGetNotificationChannelResponse pHttpStatus_ =
  GetNotificationChannelResponse'
    { snsRoleName =
        Core.Nothing,
      snsTopicArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The IAM role that is used by AWS Firewall Manager to record activity to
-- SNS.
getNotificationChannelResponse_snsRoleName :: Lens.Lens' GetNotificationChannelResponse (Core.Maybe Core.Text)
getNotificationChannelResponse_snsRoleName = Lens.lens (\GetNotificationChannelResponse' {snsRoleName} -> snsRoleName) (\s@GetNotificationChannelResponse' {} a -> s {snsRoleName = a} :: GetNotificationChannelResponse)

-- | The SNS topic that records AWS Firewall Manager activity.
getNotificationChannelResponse_snsTopicArn :: Lens.Lens' GetNotificationChannelResponse (Core.Maybe Core.Text)
getNotificationChannelResponse_snsTopicArn = Lens.lens (\GetNotificationChannelResponse' {snsTopicArn} -> snsTopicArn) (\s@GetNotificationChannelResponse' {} a -> s {snsTopicArn = a} :: GetNotificationChannelResponse)

-- | The response's http status code.
getNotificationChannelResponse_httpStatus :: Lens.Lens' GetNotificationChannelResponse Core.Int
getNotificationChannelResponse_httpStatus = Lens.lens (\GetNotificationChannelResponse' {httpStatus} -> httpStatus) (\s@GetNotificationChannelResponse' {} a -> s {httpStatus = a} :: GetNotificationChannelResponse)

instance Core.NFData GetNotificationChannelResponse
