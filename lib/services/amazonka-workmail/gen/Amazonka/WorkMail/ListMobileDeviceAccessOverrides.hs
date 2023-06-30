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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    listMobileDeviceAccessOverrides_deviceId,
    listMobileDeviceAccessOverrides_maxResults,
    listMobileDeviceAccessOverrides_nextToken,
    listMobileDeviceAccessOverrides_userId,
    listMobileDeviceAccessOverrides_organizationId,

    -- * Destructuring the Response
    ListMobileDeviceAccessOverridesResponse (..),
    newListMobileDeviceAccessOverridesResponse,

    -- * Response Lenses
    listMobileDeviceAccessOverridesResponse_nextToken,
    listMobileDeviceAccessOverridesResponse_overrides,
    listMobileDeviceAccessOverridesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newListMobileDeviceAccessOverrides' smart constructor.
data ListMobileDeviceAccessOverrides = ListMobileDeviceAccessOverrides'
  { -- | The mobile device to which the access override applies.
    deviceId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to use to retrieve the next page of results. The first call
    -- does not require a token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The WorkMail user under which you list the mobile device access
    -- overrides. Accepts the following types of user identities:
    --
    -- -   User ID: @12345678-1234-1234-1234-123456789012@ or
    --     @S-1-1-12-1234567890-123456789-123456789-1234@
    --
    -- -   Email address: @user\@domain.tld@
    --
    -- -   User name: @user@
    userId :: Prelude.Maybe Prelude.Text,
    -- | The WorkMail organization under which to list mobile device access
    -- overrides.
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
-- 'deviceId', 'listMobileDeviceAccessOverrides_deviceId' - The mobile device to which the access override applies.
--
-- 'maxResults', 'listMobileDeviceAccessOverrides_maxResults' - The maximum number of results to return in a single call.
--
-- 'nextToken', 'listMobileDeviceAccessOverrides_nextToken' - The token to use to retrieve the next page of results. The first call
-- does not require a token.
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
-- 'organizationId', 'listMobileDeviceAccessOverrides_organizationId' - The WorkMail organization under which to list mobile device access
-- overrides.
newListMobileDeviceAccessOverrides ::
  -- | 'organizationId'
  Prelude.Text ->
  ListMobileDeviceAccessOverrides
newListMobileDeviceAccessOverrides pOrganizationId_ =
  ListMobileDeviceAccessOverrides'
    { deviceId =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      userId = Prelude.Nothing,
      organizationId = pOrganizationId_
    }

-- | The mobile device to which the access override applies.
listMobileDeviceAccessOverrides_deviceId :: Lens.Lens' ListMobileDeviceAccessOverrides (Prelude.Maybe Prelude.Text)
listMobileDeviceAccessOverrides_deviceId = Lens.lens (\ListMobileDeviceAccessOverrides' {deviceId} -> deviceId) (\s@ListMobileDeviceAccessOverrides' {} a -> s {deviceId = a} :: ListMobileDeviceAccessOverrides)

-- | The maximum number of results to return in a single call.
listMobileDeviceAccessOverrides_maxResults :: Lens.Lens' ListMobileDeviceAccessOverrides (Prelude.Maybe Prelude.Natural)
listMobileDeviceAccessOverrides_maxResults = Lens.lens (\ListMobileDeviceAccessOverrides' {maxResults} -> maxResults) (\s@ListMobileDeviceAccessOverrides' {} a -> s {maxResults = a} :: ListMobileDeviceAccessOverrides)

-- | The token to use to retrieve the next page of results. The first call
-- does not require a token.
listMobileDeviceAccessOverrides_nextToken :: Lens.Lens' ListMobileDeviceAccessOverrides (Prelude.Maybe Prelude.Text)
listMobileDeviceAccessOverrides_nextToken = Lens.lens (\ListMobileDeviceAccessOverrides' {nextToken} -> nextToken) (\s@ListMobileDeviceAccessOverrides' {} a -> s {nextToken = a} :: ListMobileDeviceAccessOverrides)

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

-- | The WorkMail organization under which to list mobile device access
-- overrides.
listMobileDeviceAccessOverrides_organizationId :: Lens.Lens' ListMobileDeviceAccessOverrides Prelude.Text
listMobileDeviceAccessOverrides_organizationId = Lens.lens (\ListMobileDeviceAccessOverrides' {organizationId} -> organizationId) (\s@ListMobileDeviceAccessOverrides' {} a -> s {organizationId = a} :: ListMobileDeviceAccessOverrides)

instance
  Core.AWSRequest
    ListMobileDeviceAccessOverrides
  where
  type
    AWSResponse ListMobileDeviceAccessOverrides =
      ListMobileDeviceAccessOverridesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMobileDeviceAccessOverridesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Overrides" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListMobileDeviceAccessOverrides
  where
  hashWithSalt
    _salt
    ListMobileDeviceAccessOverrides' {..} =
      _salt
        `Prelude.hashWithSalt` deviceId
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` userId
        `Prelude.hashWithSalt` organizationId

instance
  Prelude.NFData
    ListMobileDeviceAccessOverrides
  where
  rnf ListMobileDeviceAccessOverrides' {..} =
    Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf organizationId

instance
  Data.ToHeaders
    ListMobileDeviceAccessOverrides
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.ListMobileDeviceAccessOverrides" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListMobileDeviceAccessOverrides where
  toJSON ListMobileDeviceAccessOverrides' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeviceId" Data..=) Prelude.<$> deviceId,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("UserId" Data..=) Prelude.<$> userId,
            Prelude.Just
              ("OrganizationId" Data..= organizationId)
          ]
      )

instance Data.ToPath ListMobileDeviceAccessOverrides where
  toPath = Prelude.const "/"

instance Data.ToQuery ListMobileDeviceAccessOverrides where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListMobileDeviceAccessOverridesResponse' smart constructor.
data ListMobileDeviceAccessOverridesResponse = ListMobileDeviceAccessOverridesResponse'
  { -- | The token to use to retrieve the next page of results. The value is
    -- “null” when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of mobile device access overrides that exist for the specified
    -- WorkMail organization and user.
    overrides :: Prelude.Maybe [MobileDeviceAccessOverride],
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
-- 'nextToken', 'listMobileDeviceAccessOverridesResponse_nextToken' - The token to use to retrieve the next page of results. The value is
-- “null” when there are no more results to return.
--
-- 'overrides', 'listMobileDeviceAccessOverridesResponse_overrides' - The list of mobile device access overrides that exist for the specified
-- WorkMail organization and user.
--
-- 'httpStatus', 'listMobileDeviceAccessOverridesResponse_httpStatus' - The response's http status code.
newListMobileDeviceAccessOverridesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMobileDeviceAccessOverridesResponse
newListMobileDeviceAccessOverridesResponse
  pHttpStatus_ =
    ListMobileDeviceAccessOverridesResponse'
      { nextToken =
          Prelude.Nothing,
        overrides = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. The value is
-- “null” when there are no more results to return.
listMobileDeviceAccessOverridesResponse_nextToken :: Lens.Lens' ListMobileDeviceAccessOverridesResponse (Prelude.Maybe Prelude.Text)
listMobileDeviceAccessOverridesResponse_nextToken = Lens.lens (\ListMobileDeviceAccessOverridesResponse' {nextToken} -> nextToken) (\s@ListMobileDeviceAccessOverridesResponse' {} a -> s {nextToken = a} :: ListMobileDeviceAccessOverridesResponse)

-- | The list of mobile device access overrides that exist for the specified
-- WorkMail organization and user.
listMobileDeviceAccessOverridesResponse_overrides :: Lens.Lens' ListMobileDeviceAccessOverridesResponse (Prelude.Maybe [MobileDeviceAccessOverride])
listMobileDeviceAccessOverridesResponse_overrides = Lens.lens (\ListMobileDeviceAccessOverridesResponse' {overrides} -> overrides) (\s@ListMobileDeviceAccessOverridesResponse' {} a -> s {overrides = a} :: ListMobileDeviceAccessOverridesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listMobileDeviceAccessOverridesResponse_httpStatus :: Lens.Lens' ListMobileDeviceAccessOverridesResponse Prelude.Int
listMobileDeviceAccessOverridesResponse_httpStatus = Lens.lens (\ListMobileDeviceAccessOverridesResponse' {httpStatus} -> httpStatus) (\s@ListMobileDeviceAccessOverridesResponse' {} a -> s {httpStatus = a} :: ListMobileDeviceAccessOverridesResponse)

instance
  Prelude.NFData
    ListMobileDeviceAccessOverridesResponse
  where
  rnf ListMobileDeviceAccessOverridesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf overrides
      `Prelude.seq` Prelude.rnf httpStatus
