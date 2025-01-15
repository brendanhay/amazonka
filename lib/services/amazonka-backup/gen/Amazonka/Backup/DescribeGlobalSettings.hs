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
-- Module      : Amazonka.Backup.DescribeGlobalSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes whether the Amazon Web Services account is opted in to
-- cross-account backup. Returns an error if the account is not a member of
-- an Organizations organization. Example:
-- @describe-global-settings --region us-west-2@
module Amazonka.Backup.DescribeGlobalSettings
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

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGlobalSettingsResponse'
            Prelude.<$> (x Data..?> "GlobalSettings" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "LastUpdateTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeGlobalSettings where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DescribeGlobalSettings where
  rnf _ = ()

instance Data.ToHeaders DescribeGlobalSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeGlobalSettings where
  toPath = Prelude.const "/global-settings"

instance Data.ToQuery DescribeGlobalSettings where
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
    lastUpdateTime :: Prelude.Maybe Data.POSIX,
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
describeGlobalSettingsResponse_lastUpdateTime = Lens.lens (\DescribeGlobalSettingsResponse' {lastUpdateTime} -> lastUpdateTime) (\s@DescribeGlobalSettingsResponse' {} a -> s {lastUpdateTime = a} :: DescribeGlobalSettingsResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
describeGlobalSettingsResponse_httpStatus :: Lens.Lens' DescribeGlobalSettingsResponse Prelude.Int
describeGlobalSettingsResponse_httpStatus = Lens.lens (\DescribeGlobalSettingsResponse' {httpStatus} -> httpStatus) (\s@DescribeGlobalSettingsResponse' {} a -> s {httpStatus = a} :: DescribeGlobalSettingsResponse)

instance
  Prelude.NFData
    DescribeGlobalSettingsResponse
  where
  rnf DescribeGlobalSettingsResponse' {..} =
    Prelude.rnf globalSettings `Prelude.seq`
      Prelude.rnf lastUpdateTime `Prelude.seq`
        Prelude.rnf httpStatus
