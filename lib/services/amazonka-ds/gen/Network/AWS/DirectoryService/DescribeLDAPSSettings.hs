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
-- Module      : Network.AWS.DirectoryService.DescribeLDAPSSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the status of LDAP security for the specified directory.
module Network.AWS.DirectoryService.DescribeLDAPSSettings
  ( -- * Creating a Request
    DescribeLDAPSSettings (..),
    newDescribeLDAPSSettings,

    -- * Request Lenses
    describeLDAPSSettings_nextToken,
    describeLDAPSSettings_limit,
    describeLDAPSSettings_type,
    describeLDAPSSettings_directoryId,

    -- * Destructuring the Response
    DescribeLDAPSSettingsResponse (..),
    newDescribeLDAPSSettingsResponse,

    -- * Response Lenses
    describeLDAPSSettingsResponse_lDAPSSettingsInfo,
    describeLDAPSSettingsResponse_nextToken,
    describeLDAPSSettingsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeLDAPSSettings' smart constructor.
data DescribeLDAPSSettings = DescribeLDAPSSettings'
  { -- | The type of next token used for pagination.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the number of items that should be displayed on one page.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The type of LDAP security to enable. Currently only the value @Client@
    -- is supported.
    type' :: Prelude.Maybe LDAPSType,
    -- | The identifier of the directory.
    directoryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLDAPSSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeLDAPSSettings_nextToken' - The type of next token used for pagination.
--
-- 'limit', 'describeLDAPSSettings_limit' - Specifies the number of items that should be displayed on one page.
--
-- 'type'', 'describeLDAPSSettings_type' - The type of LDAP security to enable. Currently only the value @Client@
-- is supported.
--
-- 'directoryId', 'describeLDAPSSettings_directoryId' - The identifier of the directory.
newDescribeLDAPSSettings ::
  -- | 'directoryId'
  Prelude.Text ->
  DescribeLDAPSSettings
newDescribeLDAPSSettings pDirectoryId_ =
  DescribeLDAPSSettings'
    { nextToken = Prelude.Nothing,
      limit = Prelude.Nothing,
      type' = Prelude.Nothing,
      directoryId = pDirectoryId_
    }

-- | The type of next token used for pagination.
describeLDAPSSettings_nextToken :: Lens.Lens' DescribeLDAPSSettings (Prelude.Maybe Prelude.Text)
describeLDAPSSettings_nextToken = Lens.lens (\DescribeLDAPSSettings' {nextToken} -> nextToken) (\s@DescribeLDAPSSettings' {} a -> s {nextToken = a} :: DescribeLDAPSSettings)

-- | Specifies the number of items that should be displayed on one page.
describeLDAPSSettings_limit :: Lens.Lens' DescribeLDAPSSettings (Prelude.Maybe Prelude.Natural)
describeLDAPSSettings_limit = Lens.lens (\DescribeLDAPSSettings' {limit} -> limit) (\s@DescribeLDAPSSettings' {} a -> s {limit = a} :: DescribeLDAPSSettings)

-- | The type of LDAP security to enable. Currently only the value @Client@
-- is supported.
describeLDAPSSettings_type :: Lens.Lens' DescribeLDAPSSettings (Prelude.Maybe LDAPSType)
describeLDAPSSettings_type = Lens.lens (\DescribeLDAPSSettings' {type'} -> type') (\s@DescribeLDAPSSettings' {} a -> s {type' = a} :: DescribeLDAPSSettings)

-- | The identifier of the directory.
describeLDAPSSettings_directoryId :: Lens.Lens' DescribeLDAPSSettings Prelude.Text
describeLDAPSSettings_directoryId = Lens.lens (\DescribeLDAPSSettings' {directoryId} -> directoryId) (\s@DescribeLDAPSSettings' {} a -> s {directoryId = a} :: DescribeLDAPSSettings)

instance Core.AWSRequest DescribeLDAPSSettings where
  type
    AWSResponse DescribeLDAPSSettings =
      DescribeLDAPSSettingsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLDAPSSettingsResponse'
            Prelude.<$> ( x Core..?> "LDAPSSettingsInfo"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeLDAPSSettings

instance Prelude.NFData DescribeLDAPSSettings

instance Core.ToHeaders DescribeLDAPSSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.DescribeLDAPSSettings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeLDAPSSettings where
  toJSON DescribeLDAPSSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Limit" Core..=) Prelude.<$> limit,
            ("Type" Core..=) Prelude.<$> type',
            Prelude.Just ("DirectoryId" Core..= directoryId)
          ]
      )

instance Core.ToPath DescribeLDAPSSettings where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeLDAPSSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeLDAPSSettingsResponse' smart constructor.
data DescribeLDAPSSettingsResponse = DescribeLDAPSSettingsResponse'
  { -- | Information about LDAP security for the specified directory, including
    -- status of enablement, state last updated date time, and the reason for
    -- the state.
    lDAPSSettingsInfo :: Prelude.Maybe [LDAPSSettingInfo],
    -- | The next token used to retrieve the LDAPS settings if the number of
    -- setting types exceeds page limit and there is another page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLDAPSSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lDAPSSettingsInfo', 'describeLDAPSSettingsResponse_lDAPSSettingsInfo' - Information about LDAP security for the specified directory, including
-- status of enablement, state last updated date time, and the reason for
-- the state.
--
-- 'nextToken', 'describeLDAPSSettingsResponse_nextToken' - The next token used to retrieve the LDAPS settings if the number of
-- setting types exceeds page limit and there is another page.
--
-- 'httpStatus', 'describeLDAPSSettingsResponse_httpStatus' - The response's http status code.
newDescribeLDAPSSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLDAPSSettingsResponse
newDescribeLDAPSSettingsResponse pHttpStatus_ =
  DescribeLDAPSSettingsResponse'
    { lDAPSSettingsInfo =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about LDAP security for the specified directory, including
-- status of enablement, state last updated date time, and the reason for
-- the state.
describeLDAPSSettingsResponse_lDAPSSettingsInfo :: Lens.Lens' DescribeLDAPSSettingsResponse (Prelude.Maybe [LDAPSSettingInfo])
describeLDAPSSettingsResponse_lDAPSSettingsInfo = Lens.lens (\DescribeLDAPSSettingsResponse' {lDAPSSettingsInfo} -> lDAPSSettingsInfo) (\s@DescribeLDAPSSettingsResponse' {} a -> s {lDAPSSettingsInfo = a} :: DescribeLDAPSSettingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next token used to retrieve the LDAPS settings if the number of
-- setting types exceeds page limit and there is another page.
describeLDAPSSettingsResponse_nextToken :: Lens.Lens' DescribeLDAPSSettingsResponse (Prelude.Maybe Prelude.Text)
describeLDAPSSettingsResponse_nextToken = Lens.lens (\DescribeLDAPSSettingsResponse' {nextToken} -> nextToken) (\s@DescribeLDAPSSettingsResponse' {} a -> s {nextToken = a} :: DescribeLDAPSSettingsResponse)

-- | The response's http status code.
describeLDAPSSettingsResponse_httpStatus :: Lens.Lens' DescribeLDAPSSettingsResponse Prelude.Int
describeLDAPSSettingsResponse_httpStatus = Lens.lens (\DescribeLDAPSSettingsResponse' {httpStatus} -> httpStatus) (\s@DescribeLDAPSSettingsResponse' {} a -> s {httpStatus = a} :: DescribeLDAPSSettingsResponse)

instance Prelude.NFData DescribeLDAPSSettingsResponse
