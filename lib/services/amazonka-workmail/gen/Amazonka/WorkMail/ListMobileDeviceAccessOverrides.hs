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
-- Module      : Amazonka.WorkMail.ListMobileDeviceAccessOverrides
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the mobile device access overrides for any given combination
-- of WorkMail organization, user, or device.
module Amazonka.WorkMail.ListMobileDeviceAccessOverrides
  ( -- * Creating a Request
    ListMobileDeviceAccessOverrides (..),
    newListMobileDeviceAccessOverrides,

    -- * Request Lenses
    listMobileDeviceAccessOverrides_userId,
    listMobileDeviceAccessOverrides_nextToken,
    listMobileDeviceAccessOverrides_deviceId,
    listMobileDeviceAccessOverrides_maxResults,
    listMobileDeviceAccessOverrides_organizationId,

    -- * Destructuring the Response
    ListMobileDeviceAccessOverridesResponse (..),
    newListMobileDeviceAccessOverridesResponse,

    -- * Response Lenses
    listMobileDeviceAccessOverridesResponse_overrides,
    listMobileDeviceAccessOverridesResponse_nextToken,
    listMobileDeviceAccessOverridesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newListMobileDeviceAccessOverrides' smart constructor.
data ListMobileDeviceAccessOverrides = ListMobileDeviceAccessOverrides'
  { -- | The WorkMail user under which you list the mobile device access
    -- overrides. Accepts the following types of user identities:
    --
    -- -   User ID: @12345678-1234-1234-1234-123456789012@ or
    --     @S-1-1-12-1234567890-123456789-123456789-1234@
    --
    -- -   Email address: @user\@domain.tld@
    --
    -- -   User name: @user@
    userId :: Prelude.Maybe Prelude.Text,
    -- | The token to use to retrieve the next page of results. The first call
    -- does not require a token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The mobile device to which the access override applies.
    deviceId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon WorkMail organization under which to list mobile device
    -- access overrides.
    organizationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMobileDeviceAccessOverrides' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userId', 'listMobileDeviceAccessOverrides_userId' - The WorkMail user under which you list the mobile device access
-- overrides. Accepts the following types of user identities:
--
-- -   User ID: @12345678-1234-1234-1234-123456789012@ or
--     @S-1-1-12-1234567890-123456789-123456789-1234@
--
-- -   Email address: @user\@domain.tld@
--
-- -   User name: @user@
--
-- 'nextToken', 'listMobileDeviceAccessOverrides_nextToken' - The token to use to retrieve the next page of results. The first call
-- does not require a token.
--
-- 'deviceId', 'listMobileDeviceAccessOverrides_deviceId' - The mobile device to which the access override applies.
--
-- 'maxResults', 'listMobileDeviceAccessOverrides_maxResults' - The maximum number of results to return in a single call.
--
-- 'organizationId', 'listMobileDeviceAccessOverrides_organizationId' - The Amazon WorkMail organization under which to list mobile device
-- access overrides.
newListMobileDeviceAccessOverrides ::
  -- | 'organizationId'
  Prelude.Text ->
  ListMobileDeviceAccessOverrides
newListMobileDeviceAccessOverrides pOrganizationId_ =
  ListMobileDeviceAccessOverrides'
    { userId =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      deviceId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      organizationId = pOrganizationId_
    }

-- | The WorkMail user under which you list the mobile device access
-- overrides. Accepts the following types of user identities:
--
-- -   User ID: @12345678-1234-1234-1234-123456789012@ or
--     @S-1-1-12-1234567890-123456789-123456789-1234@
--
-- -   Email address: @user\@domain.tld@
--
-- -   User name: @user@
listMobileDeviceAccessOverrides_userId :: Lens.Lens' ListMobileDeviceAccessOverrides (Prelude.Maybe Prelude.Text)
listMobileDeviceAccessOverrides_userId = Lens.lens (\ListMobileDeviceAccessOverrides' {userId} -> userId) (\s@ListMobileDeviceAccessOverrides' {} a -> s {userId = a} :: ListMobileDeviceAccessOverrides)

-- | The token to use to retrieve the next page of results. The first call
-- does not require a token.
listMobileDeviceAccessOverrides_nextToken :: Lens.Lens' ListMobileDeviceAccessOverrides (Prelude.Maybe Prelude.Text)
listMobileDeviceAccessOverrides_nextToken = Lens.lens (\ListMobileDeviceAccessOverrides' {nextToken} -> nextToken) (\s@ListMobileDeviceAccessOverrides' {} a -> s {nextToken = a} :: ListMobileDeviceAccessOverrides)

-- | The mobile device to which the access override applies.
listMobileDeviceAccessOverrides_deviceId :: Lens.Lens' ListMobileDeviceAccessOverrides (Prelude.Maybe Prelude.Text)
listMobileDeviceAccessOverrides_deviceId = Lens.lens (\ListMobileDeviceAccessOverrides' {deviceId} -> deviceId) (\s@ListMobileDeviceAccessOverrides' {} a -> s {deviceId = a} :: ListMobileDeviceAccessOverrides)

-- | The maximum number of results to return in a single call.
listMobileDeviceAccessOverrides_maxResults :: Lens.Lens' ListMobileDeviceAccessOverrides (Prelude.Maybe Prelude.Natural)
listMobileDeviceAccessOverrides_maxResults = Lens.lens (\ListMobileDeviceAccessOverrides' {maxResults} -> maxResults) (\s@ListMobileDeviceAccessOverrides' {} a -> s {maxResults = a} :: ListMobileDeviceAccessOverrides)

-- | The Amazon WorkMail organization under which to list mobile device
-- access overrides.
listMobileDeviceAccessOverrides_organizationId :: Lens.Lens' ListMobileDeviceAccessOverrides Prelude.Text
listMobileDeviceAccessOverrides_organizationId = Lens.lens (\ListMobileDeviceAccessOverrides' {organizationId} -> organizationId) (\s@ListMobileDeviceAccessOverrides' {} a -> s {organizationId = a} :: ListMobileDeviceAccessOverrides)

instance
  Core.AWSRequest
    ListMobileDeviceAccessOverrides
  where
  type
    AWSResponse ListMobileDeviceAccessOverrides =
      ListMobileDeviceAccessOverridesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMobileDeviceAccessOverridesResponse'
            Prelude.<$> (x Core..?> "Overrides" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListMobileDeviceAccessOverrides

instance
  Prelude.NFData
    ListMobileDeviceAccessOverrides

instance
  Core.ToHeaders
    ListMobileDeviceAccessOverrides
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkMailService.ListMobileDeviceAccessOverrides" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListMobileDeviceAccessOverrides where
  toJSON ListMobileDeviceAccessOverrides' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("UserId" Core..=) Prelude.<$> userId,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("DeviceId" Core..=) Prelude.<$> deviceId,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just
              ("OrganizationId" Core..= organizationId)
          ]
      )

instance Core.ToPath ListMobileDeviceAccessOverrides where
  toPath = Prelude.const "/"

instance Core.ToQuery ListMobileDeviceAccessOverrides where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListMobileDeviceAccessOverridesResponse' smart constructor.
data ListMobileDeviceAccessOverridesResponse = ListMobileDeviceAccessOverridesResponse'
  { -- | The list of mobile device access overrides that exist for the specified
    -- Amazon WorkMail organization and user.
    overrides :: Prelude.Maybe [MobileDeviceAccessOverride],
    -- | The token to use to retrieve the next page of results. The value is
    -- “null” when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMobileDeviceAccessOverridesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'overrides', 'listMobileDeviceAccessOverridesResponse_overrides' - The list of mobile device access overrides that exist for the specified
-- Amazon WorkMail organization and user.
--
-- 'nextToken', 'listMobileDeviceAccessOverridesResponse_nextToken' - The token to use to retrieve the next page of results. The value is
-- “null” when there are no more results to return.
--
-- 'httpStatus', 'listMobileDeviceAccessOverridesResponse_httpStatus' - The response's http status code.
newListMobileDeviceAccessOverridesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMobileDeviceAccessOverridesResponse
newListMobileDeviceAccessOverridesResponse
  pHttpStatus_ =
    ListMobileDeviceAccessOverridesResponse'
      { overrides =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The list of mobile device access overrides that exist for the specified
-- Amazon WorkMail organization and user.
listMobileDeviceAccessOverridesResponse_overrides :: Lens.Lens' ListMobileDeviceAccessOverridesResponse (Prelude.Maybe [MobileDeviceAccessOverride])
listMobileDeviceAccessOverridesResponse_overrides = Lens.lens (\ListMobileDeviceAccessOverridesResponse' {overrides} -> overrides) (\s@ListMobileDeviceAccessOverridesResponse' {} a -> s {overrides = a} :: ListMobileDeviceAccessOverridesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. The value is
-- “null” when there are no more results to return.
listMobileDeviceAccessOverridesResponse_nextToken :: Lens.Lens' ListMobileDeviceAccessOverridesResponse (Prelude.Maybe Prelude.Text)
listMobileDeviceAccessOverridesResponse_nextToken = Lens.lens (\ListMobileDeviceAccessOverridesResponse' {nextToken} -> nextToken) (\s@ListMobileDeviceAccessOverridesResponse' {} a -> s {nextToken = a} :: ListMobileDeviceAccessOverridesResponse)

-- | The response's http status code.
listMobileDeviceAccessOverridesResponse_httpStatus :: Lens.Lens' ListMobileDeviceAccessOverridesResponse Prelude.Int
listMobileDeviceAccessOverridesResponse_httpStatus = Lens.lens (\ListMobileDeviceAccessOverridesResponse' {httpStatus} -> httpStatus) (\s@ListMobileDeviceAccessOverridesResponse' {} a -> s {httpStatus = a} :: ListMobileDeviceAccessOverridesResponse)

instance
  Prelude.NFData
    ListMobileDeviceAccessOverridesResponse
