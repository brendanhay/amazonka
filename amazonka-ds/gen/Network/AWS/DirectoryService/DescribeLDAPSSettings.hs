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
    describeLDAPSSettings_type,
    describeLDAPSSettings_limit,
    describeLDAPSSettings_directoryId,

    -- * Destructuring the Response
    DescribeLDAPSSettingsResponse (..),
    newDescribeLDAPSSettingsResponse,

    -- * Response Lenses
    describeLDAPSSettingsResponse_nextToken,
    describeLDAPSSettingsResponse_lDAPSSettingsInfo,
    describeLDAPSSettingsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeLDAPSSettings' smart constructor.
data DescribeLDAPSSettings = DescribeLDAPSSettings'
  { -- | The type of next token used for pagination.
    nextToken :: Core.Maybe Core.Text,
    -- | The type of LDAP security to enable. Currently only the value @Client@
    -- is supported.
    type' :: Core.Maybe LDAPSType,
    -- | Specifies the number of items that should be displayed on one page.
    limit :: Core.Maybe Core.Natural,
    -- | The identifier of the directory.
    directoryId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'type'', 'describeLDAPSSettings_type' - The type of LDAP security to enable. Currently only the value @Client@
-- is supported.
--
-- 'limit', 'describeLDAPSSettings_limit' - Specifies the number of items that should be displayed on one page.
--
-- 'directoryId', 'describeLDAPSSettings_directoryId' - The identifier of the directory.
newDescribeLDAPSSettings ::
  -- | 'directoryId'
  Core.Text ->
  DescribeLDAPSSettings
newDescribeLDAPSSettings pDirectoryId_ =
  DescribeLDAPSSettings'
    { nextToken = Core.Nothing,
      type' = Core.Nothing,
      limit = Core.Nothing,
      directoryId = pDirectoryId_
    }

-- | The type of next token used for pagination.
describeLDAPSSettings_nextToken :: Lens.Lens' DescribeLDAPSSettings (Core.Maybe Core.Text)
describeLDAPSSettings_nextToken = Lens.lens (\DescribeLDAPSSettings' {nextToken} -> nextToken) (\s@DescribeLDAPSSettings' {} a -> s {nextToken = a} :: DescribeLDAPSSettings)

-- | The type of LDAP security to enable. Currently only the value @Client@
-- is supported.
describeLDAPSSettings_type :: Lens.Lens' DescribeLDAPSSettings (Core.Maybe LDAPSType)
describeLDAPSSettings_type = Lens.lens (\DescribeLDAPSSettings' {type'} -> type') (\s@DescribeLDAPSSettings' {} a -> s {type' = a} :: DescribeLDAPSSettings)

-- | Specifies the number of items that should be displayed on one page.
describeLDAPSSettings_limit :: Lens.Lens' DescribeLDAPSSettings (Core.Maybe Core.Natural)
describeLDAPSSettings_limit = Lens.lens (\DescribeLDAPSSettings' {limit} -> limit) (\s@DescribeLDAPSSettings' {} a -> s {limit = a} :: DescribeLDAPSSettings)

-- | The identifier of the directory.
describeLDAPSSettings_directoryId :: Lens.Lens' DescribeLDAPSSettings Core.Text
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
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "LDAPSSettingsInfo" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeLDAPSSettings

instance Core.NFData DescribeLDAPSSettings

instance Core.ToHeaders DescribeLDAPSSettings where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.DescribeLDAPSSettings" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeLDAPSSettings where
  toJSON DescribeLDAPSSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("Type" Core..=) Core.<$> type',
            ("Limit" Core..=) Core.<$> limit,
            Core.Just ("DirectoryId" Core..= directoryId)
          ]
      )

instance Core.ToPath DescribeLDAPSSettings where
  toPath = Core.const "/"

instance Core.ToQuery DescribeLDAPSSettings where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeLDAPSSettingsResponse' smart constructor.
data DescribeLDAPSSettingsResponse = DescribeLDAPSSettingsResponse'
  { -- | The next token used to retrieve the LDAPS settings if the number of
    -- setting types exceeds page limit and there is another page.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about LDAP security for the specified directory, including
    -- status of enablement, state last updated date time, and the reason for
    -- the state.
    lDAPSSettingsInfo :: Core.Maybe [LDAPSSettingInfo],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeLDAPSSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeLDAPSSettingsResponse_nextToken' - The next token used to retrieve the LDAPS settings if the number of
-- setting types exceeds page limit and there is another page.
--
-- 'lDAPSSettingsInfo', 'describeLDAPSSettingsResponse_lDAPSSettingsInfo' - Information about LDAP security for the specified directory, including
-- status of enablement, state last updated date time, and the reason for
-- the state.
--
-- 'httpStatus', 'describeLDAPSSettingsResponse_httpStatus' - The response's http status code.
newDescribeLDAPSSettingsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeLDAPSSettingsResponse
newDescribeLDAPSSettingsResponse pHttpStatus_ =
  DescribeLDAPSSettingsResponse'
    { nextToken =
        Core.Nothing,
      lDAPSSettingsInfo = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The next token used to retrieve the LDAPS settings if the number of
-- setting types exceeds page limit and there is another page.
describeLDAPSSettingsResponse_nextToken :: Lens.Lens' DescribeLDAPSSettingsResponse (Core.Maybe Core.Text)
describeLDAPSSettingsResponse_nextToken = Lens.lens (\DescribeLDAPSSettingsResponse' {nextToken} -> nextToken) (\s@DescribeLDAPSSettingsResponse' {} a -> s {nextToken = a} :: DescribeLDAPSSettingsResponse)

-- | Information about LDAP security for the specified directory, including
-- status of enablement, state last updated date time, and the reason for
-- the state.
describeLDAPSSettingsResponse_lDAPSSettingsInfo :: Lens.Lens' DescribeLDAPSSettingsResponse (Core.Maybe [LDAPSSettingInfo])
describeLDAPSSettingsResponse_lDAPSSettingsInfo = Lens.lens (\DescribeLDAPSSettingsResponse' {lDAPSSettingsInfo} -> lDAPSSettingsInfo) (\s@DescribeLDAPSSettingsResponse' {} a -> s {lDAPSSettingsInfo = a} :: DescribeLDAPSSettingsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeLDAPSSettingsResponse_httpStatus :: Lens.Lens' DescribeLDAPSSettingsResponse Core.Int
describeLDAPSSettingsResponse_httpStatus = Lens.lens (\DescribeLDAPSSettingsResponse' {httpStatus} -> httpStatus) (\s@DescribeLDAPSSettingsResponse' {} a -> s {httpStatus = a} :: DescribeLDAPSSettingsResponse)

instance Core.NFData DescribeLDAPSSettingsResponse
