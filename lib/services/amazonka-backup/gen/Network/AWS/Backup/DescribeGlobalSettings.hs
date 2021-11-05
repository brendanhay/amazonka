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
-- Module      : Network.AWS.Backup.DescribeGlobalSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes whether the Amazon Web Services account is opted in to
-- cross-account backup. Returns an error if the account is not a member of
-- an Organizations organization. Example:
-- @describe-global-settings --region us-west-2@
module Network.AWS.Backup.DescribeGlobalSettings
  ( -- * Creating a Request
    DescribeGlobalSettings (..),
    newDescribeGlobalSettings,

    -- * Destructuring the Response
    DescribeGlobalSettingsResponse (..),
    newDescribeGlobalSettingsResponse,

    -- * Response Lenses
    describeGlobalSettingsResponse_globalSettings,
    describeGlobalSettingsResponse_lastUpdateTime,
    describeGlobalSettingsResponse_httpStatus,
  )
where

import Network.AWS.Backup.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeGlobalSettings' smart constructor.
data DescribeGlobalSettings = DescribeGlobalSettings'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGlobalSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeGlobalSettings ::
  DescribeGlobalSettings
newDescribeGlobalSettings = DescribeGlobalSettings'

instance Core.AWSRequest DescribeGlobalSettings where
  type
    AWSResponse DescribeGlobalSettings =
      DescribeGlobalSettingsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGlobalSettingsResponse'
            Prelude.<$> (x Core..?> "GlobalSettings" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "LastUpdateTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeGlobalSettings

instance Prelude.NFData DescribeGlobalSettings

instance Core.ToHeaders DescribeGlobalSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeGlobalSettings where
  toPath = Prelude.const "/global-settings"

instance Core.ToQuery DescribeGlobalSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeGlobalSettingsResponse' smart constructor.
data DescribeGlobalSettingsResponse = DescribeGlobalSettingsResponse'
  { -- | The status of the flag @isCrossAccountBackupEnabled@.
    globalSettings :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The date and time that the flag @isCrossAccountBackupEnabled@ was last
    -- updated. This update is in Unix format and Coordinated Universal Time
    -- (UTC). The value of @LastUpdateTime@ is accurate to milliseconds. For
    -- example, the value 1516925490.087 represents Friday, January 26, 2018
    -- 12:11:30.087 AM.
    lastUpdateTime :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGlobalSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalSettings', 'describeGlobalSettingsResponse_globalSettings' - The status of the flag @isCrossAccountBackupEnabled@.
--
-- 'lastUpdateTime', 'describeGlobalSettingsResponse_lastUpdateTime' - The date and time that the flag @isCrossAccountBackupEnabled@ was last
-- updated. This update is in Unix format and Coordinated Universal Time
-- (UTC). The value of @LastUpdateTime@ is accurate to milliseconds. For
-- example, the value 1516925490.087 represents Friday, January 26, 2018
-- 12:11:30.087 AM.
--
-- 'httpStatus', 'describeGlobalSettingsResponse_httpStatus' - The response's http status code.
newDescribeGlobalSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeGlobalSettingsResponse
newDescribeGlobalSettingsResponse pHttpStatus_ =
  DescribeGlobalSettingsResponse'
    { globalSettings =
        Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the flag @isCrossAccountBackupEnabled@.
describeGlobalSettingsResponse_globalSettings :: Lens.Lens' DescribeGlobalSettingsResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeGlobalSettingsResponse_globalSettings = Lens.lens (\DescribeGlobalSettingsResponse' {globalSettings} -> globalSettings) (\s@DescribeGlobalSettingsResponse' {} a -> s {globalSettings = a} :: DescribeGlobalSettingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The date and time that the flag @isCrossAccountBackupEnabled@ was last
-- updated. This update is in Unix format and Coordinated Universal Time
-- (UTC). The value of @LastUpdateTime@ is accurate to milliseconds. For
-- example, the value 1516925490.087 represents Friday, January 26, 2018
-- 12:11:30.087 AM.
describeGlobalSettingsResponse_lastUpdateTime :: Lens.Lens' DescribeGlobalSettingsResponse (Prelude.Maybe Prelude.UTCTime)
describeGlobalSettingsResponse_lastUpdateTime = Lens.lens (\DescribeGlobalSettingsResponse' {lastUpdateTime} -> lastUpdateTime) (\s@DescribeGlobalSettingsResponse' {} a -> s {lastUpdateTime = a} :: DescribeGlobalSettingsResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
describeGlobalSettingsResponse_httpStatus :: Lens.Lens' DescribeGlobalSettingsResponse Prelude.Int
describeGlobalSettingsResponse_httpStatus = Lens.lens (\DescribeGlobalSettingsResponse' {httpStatus} -> httpStatus) (\s@DescribeGlobalSettingsResponse' {} a -> s {httpStatus = a} :: DescribeGlobalSettingsResponse)

instance
  Prelude.NFData
    DescribeGlobalSettingsResponse
