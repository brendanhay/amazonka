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
-- Module      : Network.AWS.Shield.DescribeEmergencyContactSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A list of email addresses and phone numbers that the DDoS Response Team
-- (DRT) can use to contact you if you have proactive engagement enabled,
-- for escalations to the DRT and to initiate proactive customer support.
module Network.AWS.Shield.DescribeEmergencyContactSettings
  ( -- * Creating a Request
    DescribeEmergencyContactSettings (..),
    newDescribeEmergencyContactSettings,

    -- * Destructuring the Response
    DescribeEmergencyContactSettingsResponse (..),
    newDescribeEmergencyContactSettingsResponse,

    -- * Response Lenses
    describeEmergencyContactSettingsResponse_emergencyContactList,
    describeEmergencyContactSettingsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Shield.Types

-- | /See:/ 'newDescribeEmergencyContactSettings' smart constructor.
data DescribeEmergencyContactSettings = DescribeEmergencyContactSettings'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEmergencyContactSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeEmergencyContactSettings ::
  DescribeEmergencyContactSettings
newDescribeEmergencyContactSettings =
  DescribeEmergencyContactSettings'

instance
  Core.AWSRequest
    DescribeEmergencyContactSettings
  where
  type
    AWSResponse DescribeEmergencyContactSettings =
      DescribeEmergencyContactSettingsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEmergencyContactSettingsResponse'
            Prelude.<$> ( x Core..?> "EmergencyContactList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeEmergencyContactSettings

instance
  Prelude.NFData
    DescribeEmergencyContactSettings

instance
  Core.ToHeaders
    DescribeEmergencyContactSettings
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShield_20160616.DescribeEmergencyContactSettings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeEmergencyContactSettings where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath DescribeEmergencyContactSettings where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeEmergencyContactSettings
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEmergencyContactSettingsResponse' smart constructor.
data DescribeEmergencyContactSettingsResponse = DescribeEmergencyContactSettingsResponse'
  { -- | A list of email addresses and phone numbers that the DDoS Response Team
    -- (DRT) can use to contact you if you have proactive engagement enabled,
    -- for escalations to the DRT and to initiate proactive customer support.
    emergencyContactList :: Prelude.Maybe [EmergencyContact],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEmergencyContactSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emergencyContactList', 'describeEmergencyContactSettingsResponse_emergencyContactList' - A list of email addresses and phone numbers that the DDoS Response Team
-- (DRT) can use to contact you if you have proactive engagement enabled,
-- for escalations to the DRT and to initiate proactive customer support.
--
-- 'httpStatus', 'describeEmergencyContactSettingsResponse_httpStatus' - The response's http status code.
newDescribeEmergencyContactSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEmergencyContactSettingsResponse
newDescribeEmergencyContactSettingsResponse
  pHttpStatus_ =
    DescribeEmergencyContactSettingsResponse'
      { emergencyContactList =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of email addresses and phone numbers that the DDoS Response Team
-- (DRT) can use to contact you if you have proactive engagement enabled,
-- for escalations to the DRT and to initiate proactive customer support.
describeEmergencyContactSettingsResponse_emergencyContactList :: Lens.Lens' DescribeEmergencyContactSettingsResponse (Prelude.Maybe [EmergencyContact])
describeEmergencyContactSettingsResponse_emergencyContactList = Lens.lens (\DescribeEmergencyContactSettingsResponse' {emergencyContactList} -> emergencyContactList) (\s@DescribeEmergencyContactSettingsResponse' {} a -> s {emergencyContactList = a} :: DescribeEmergencyContactSettingsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeEmergencyContactSettingsResponse_httpStatus :: Lens.Lens' DescribeEmergencyContactSettingsResponse Prelude.Int
describeEmergencyContactSettingsResponse_httpStatus = Lens.lens (\DescribeEmergencyContactSettingsResponse' {httpStatus} -> httpStatus) (\s@DescribeEmergencyContactSettingsResponse' {} a -> s {httpStatus = a} :: DescribeEmergencyContactSettingsResponse)

instance
  Prelude.NFData
    DescribeEmergencyContactSettingsResponse
