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

import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetNotificationChannel' smart constructor.
data GetNotificationChannel = GetNotificationChannel'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetNotificationChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetNotificationChannel ::
  GetNotificationChannel
newGetNotificationChannel = GetNotificationChannel'

instance Prelude.AWSRequest GetNotificationChannel where
  type
    Rs GetNotificationChannel =
      GetNotificationChannelResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetNotificationChannelResponse'
            Prelude.<$> (x Prelude..?> "SnsRoleName")
            Prelude.<*> (x Prelude..?> "SnsTopicArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetNotificationChannel

instance Prelude.NFData GetNotificationChannel

instance Prelude.ToHeaders GetNotificationChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSFMS_20180101.GetNotificationChannel" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetNotificationChannel where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath GetNotificationChannel where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetNotificationChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetNotificationChannelResponse' smart constructor.
data GetNotificationChannelResponse = GetNotificationChannelResponse'
  { -- | The IAM role that is used by AWS Firewall Manager to record activity to
    -- SNS.
    snsRoleName :: Prelude.Maybe Prelude.Text,
    -- | The SNS topic that records AWS Firewall Manager activity.
    snsTopicArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetNotificationChannelResponse
newGetNotificationChannelResponse pHttpStatus_ =
  GetNotificationChannelResponse'
    { snsRoleName =
        Prelude.Nothing,
      snsTopicArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The IAM role that is used by AWS Firewall Manager to record activity to
-- SNS.
getNotificationChannelResponse_snsRoleName :: Lens.Lens' GetNotificationChannelResponse (Prelude.Maybe Prelude.Text)
getNotificationChannelResponse_snsRoleName = Lens.lens (\GetNotificationChannelResponse' {snsRoleName} -> snsRoleName) (\s@GetNotificationChannelResponse' {} a -> s {snsRoleName = a} :: GetNotificationChannelResponse)

-- | The SNS topic that records AWS Firewall Manager activity.
getNotificationChannelResponse_snsTopicArn :: Lens.Lens' GetNotificationChannelResponse (Prelude.Maybe Prelude.Text)
getNotificationChannelResponse_snsTopicArn = Lens.lens (\GetNotificationChannelResponse' {snsTopicArn} -> snsTopicArn) (\s@GetNotificationChannelResponse' {} a -> s {snsTopicArn = a} :: GetNotificationChannelResponse)

-- | The response's http status code.
getNotificationChannelResponse_httpStatus :: Lens.Lens' GetNotificationChannelResponse Prelude.Int
getNotificationChannelResponse_httpStatus = Lens.lens (\GetNotificationChannelResponse' {httpStatus} -> httpStatus) (\s@GetNotificationChannelResponse' {} a -> s {httpStatus = a} :: GetNotificationChannelResponse)

instance
  Prelude.NFData
    GetNotificationChannelResponse
