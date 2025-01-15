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
-- Module      : Amazonka.FMS.GetNotificationChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Information about the Amazon Simple Notification Service (SNS) topic
-- that is used to record Firewall Manager SNS logs.
module Amazonka.FMS.GetNotificationChannel
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetNotificationChannel' smart constructor.
data GetNotificationChannel = GetNotificationChannel'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetNotificationChannelResponse'
            Prelude.<$> (x Data..?> "SnsRoleName")
            Prelude.<*> (x Data..?> "SnsTopicArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetNotificationChannel where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetNotificationChannel where
  rnf _ = ()

instance Data.ToHeaders GetNotificationChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSFMS_20180101.GetNotificationChannel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetNotificationChannel where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath GetNotificationChannel where
  toPath = Prelude.const "/"

instance Data.ToQuery GetNotificationChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetNotificationChannelResponse' smart constructor.
data GetNotificationChannelResponse = GetNotificationChannelResponse'
  { -- | The IAM role that is used by Firewall Manager to record activity to SNS.
    snsRoleName :: Prelude.Maybe Prelude.Text,
    -- | The SNS topic that records Firewall Manager activity.
    snsTopicArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNotificationChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snsRoleName', 'getNotificationChannelResponse_snsRoleName' - The IAM role that is used by Firewall Manager to record activity to SNS.
--
-- 'snsTopicArn', 'getNotificationChannelResponse_snsTopicArn' - The SNS topic that records Firewall Manager activity.
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

-- | The IAM role that is used by Firewall Manager to record activity to SNS.
getNotificationChannelResponse_snsRoleName :: Lens.Lens' GetNotificationChannelResponse (Prelude.Maybe Prelude.Text)
getNotificationChannelResponse_snsRoleName = Lens.lens (\GetNotificationChannelResponse' {snsRoleName} -> snsRoleName) (\s@GetNotificationChannelResponse' {} a -> s {snsRoleName = a} :: GetNotificationChannelResponse)

-- | The SNS topic that records Firewall Manager activity.
getNotificationChannelResponse_snsTopicArn :: Lens.Lens' GetNotificationChannelResponse (Prelude.Maybe Prelude.Text)
getNotificationChannelResponse_snsTopicArn = Lens.lens (\GetNotificationChannelResponse' {snsTopicArn} -> snsTopicArn) (\s@GetNotificationChannelResponse' {} a -> s {snsTopicArn = a} :: GetNotificationChannelResponse)

-- | The response's http status code.
getNotificationChannelResponse_httpStatus :: Lens.Lens' GetNotificationChannelResponse Prelude.Int
getNotificationChannelResponse_httpStatus = Lens.lens (\GetNotificationChannelResponse' {httpStatus} -> httpStatus) (\s@GetNotificationChannelResponse' {} a -> s {httpStatus = a} :: GetNotificationChannelResponse)

instance
  Prelude.NFData
    GetNotificationChannelResponse
  where
  rnf GetNotificationChannelResponse' {..} =
    Prelude.rnf snsRoleName `Prelude.seq`
      Prelude.rnf snsTopicArn `Prelude.seq`
        Prelude.rnf httpStatus
