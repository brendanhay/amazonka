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
-- Module      : Amazonka.DirectoryService.DescribeSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the configurable settings for the specified
-- directory.
module Amazonka.DirectoryService.DescribeSettings
  ( -- * Creating a Request
    DescribeSettings (..),
    newDescribeSettings,

    -- * Request Lenses
    describeSettings_nextToken,
    describeSettings_status,
    describeSettings_directoryId,

    -- * Destructuring the Response
    DescribeSettingsResponse (..),
    newDescribeSettingsResponse,

    -- * Response Lenses
    describeSettingsResponse_directoryId,
    describeSettingsResponse_nextToken,
    describeSettingsResponse_settingEntries,
    describeSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeSettings' smart constructor.
data DescribeSettings = DescribeSettings'
  { -- | The @DescribeSettingsResult.NextToken@ value from a previous call to
    -- DescribeSettings. Pass null if this is the first call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The status of the directory settings for which to retrieve information.
    status :: Prelude.Maybe DirectoryConfigurationStatus,
    -- | The identifier of the directory for which to retrieve information.
    directoryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSettings_nextToken' - The @DescribeSettingsResult.NextToken@ value from a previous call to
-- DescribeSettings. Pass null if this is the first call.
--
-- 'status', 'describeSettings_status' - The status of the directory settings for which to retrieve information.
--
-- 'directoryId', 'describeSettings_directoryId' - The identifier of the directory for which to retrieve information.
newDescribeSettings ::
  -- | 'directoryId'
  Prelude.Text ->
  DescribeSettings
newDescribeSettings pDirectoryId_ =
  DescribeSettings'
    { nextToken = Prelude.Nothing,
      status = Prelude.Nothing,
      directoryId = pDirectoryId_
    }

-- | The @DescribeSettingsResult.NextToken@ value from a previous call to
-- DescribeSettings. Pass null if this is the first call.
describeSettings_nextToken :: Lens.Lens' DescribeSettings (Prelude.Maybe Prelude.Text)
describeSettings_nextToken = Lens.lens (\DescribeSettings' {nextToken} -> nextToken) (\s@DescribeSettings' {} a -> s {nextToken = a} :: DescribeSettings)

-- | The status of the directory settings for which to retrieve information.
describeSettings_status :: Lens.Lens' DescribeSettings (Prelude.Maybe DirectoryConfigurationStatus)
describeSettings_status = Lens.lens (\DescribeSettings' {status} -> status) (\s@DescribeSettings' {} a -> s {status = a} :: DescribeSettings)

-- | The identifier of the directory for which to retrieve information.
describeSettings_directoryId :: Lens.Lens' DescribeSettings Prelude.Text
describeSettings_directoryId = Lens.lens (\DescribeSettings' {directoryId} -> directoryId) (\s@DescribeSettings' {} a -> s {directoryId = a} :: DescribeSettings)

instance Core.AWSRequest DescribeSettings where
  type
    AWSResponse DescribeSettings =
      DescribeSettingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSettingsResponse'
            Prelude.<$> (x Data..?> "DirectoryId")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "SettingEntries" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSettings where
  hashWithSalt _salt DescribeSettings' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` directoryId

instance Prelude.NFData DescribeSettings where
  rnf DescribeSettings' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf directoryId

instance Data.ToHeaders DescribeSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.DescribeSettings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeSettings where
  toJSON DescribeSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Status" Data..=) Prelude.<$> status,
            Prelude.Just ("DirectoryId" Data..= directoryId)
          ]
      )

instance Data.ToPath DescribeSettings where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSettingsResponse' smart constructor.
data DescribeSettingsResponse = DescribeSettingsResponse'
  { -- | The identifier of the directory.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | If not null, token that indicates that more results are available. Pass
    -- this value for the @NextToken@ parameter in a subsequent call to
    -- @DescribeSettings@ to retrieve the next set of items.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of SettingEntry objects that were retrieved.
    --
    -- It is possible that this list contains less than the number of items
    -- specified in the @Limit@ member of the request. This occurs if there are
    -- less than the requested number of items left to retrieve, or if the
    -- limitations of the operation have been exceeded.
    settingEntries :: Prelude.Maybe [SettingEntry],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'describeSettingsResponse_directoryId' - The identifier of the directory.
--
-- 'nextToken', 'describeSettingsResponse_nextToken' - If not null, token that indicates that more results are available. Pass
-- this value for the @NextToken@ parameter in a subsequent call to
-- @DescribeSettings@ to retrieve the next set of items.
--
-- 'settingEntries', 'describeSettingsResponse_settingEntries' - The list of SettingEntry objects that were retrieved.
--
-- It is possible that this list contains less than the number of items
-- specified in the @Limit@ member of the request. This occurs if there are
-- less than the requested number of items left to retrieve, or if the
-- limitations of the operation have been exceeded.
--
-- 'httpStatus', 'describeSettingsResponse_httpStatus' - The response's http status code.
newDescribeSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSettingsResponse
newDescribeSettingsResponse pHttpStatus_ =
  DescribeSettingsResponse'
    { directoryId =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      settingEntries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the directory.
describeSettingsResponse_directoryId :: Lens.Lens' DescribeSettingsResponse (Prelude.Maybe Prelude.Text)
describeSettingsResponse_directoryId = Lens.lens (\DescribeSettingsResponse' {directoryId} -> directoryId) (\s@DescribeSettingsResponse' {} a -> s {directoryId = a} :: DescribeSettingsResponse)

-- | If not null, token that indicates that more results are available. Pass
-- this value for the @NextToken@ parameter in a subsequent call to
-- @DescribeSettings@ to retrieve the next set of items.
describeSettingsResponse_nextToken :: Lens.Lens' DescribeSettingsResponse (Prelude.Maybe Prelude.Text)
describeSettingsResponse_nextToken = Lens.lens (\DescribeSettingsResponse' {nextToken} -> nextToken) (\s@DescribeSettingsResponse' {} a -> s {nextToken = a} :: DescribeSettingsResponse)

-- | The list of SettingEntry objects that were retrieved.
--
-- It is possible that this list contains less than the number of items
-- specified in the @Limit@ member of the request. This occurs if there are
-- less than the requested number of items left to retrieve, or if the
-- limitations of the operation have been exceeded.
describeSettingsResponse_settingEntries :: Lens.Lens' DescribeSettingsResponse (Prelude.Maybe [SettingEntry])
describeSettingsResponse_settingEntries = Lens.lens (\DescribeSettingsResponse' {settingEntries} -> settingEntries) (\s@DescribeSettingsResponse' {} a -> s {settingEntries = a} :: DescribeSettingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeSettingsResponse_httpStatus :: Lens.Lens' DescribeSettingsResponse Prelude.Int
describeSettingsResponse_httpStatus = Lens.lens (\DescribeSettingsResponse' {httpStatus} -> httpStatus) (\s@DescribeSettingsResponse' {} a -> s {httpStatus = a} :: DescribeSettingsResponse)

instance Prelude.NFData DescribeSettingsResponse where
  rnf DescribeSettingsResponse' {..} =
    Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf settingEntries
      `Prelude.seq` Prelude.rnf httpStatus
