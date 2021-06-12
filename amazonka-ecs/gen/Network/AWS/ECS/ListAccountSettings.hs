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
-- Module      : Network.AWS.ECS.ListAccountSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the account settings for a specified principal.
--
-- This operation returns paginated results.
module Network.AWS.ECS.ListAccountSettings
  ( -- * Creating a Request
    ListAccountSettings (..),
    newListAccountSettings,

    -- * Request Lenses
    listAccountSettings_nextToken,
    listAccountSettings_maxResults,
    listAccountSettings_effectiveSettings,
    listAccountSettings_name,
    listAccountSettings_principalArn,
    listAccountSettings_value,

    -- * Destructuring the Response
    ListAccountSettingsResponse (..),
    newListAccountSettingsResponse,

    -- * Response Lenses
    listAccountSettingsResponse_nextToken,
    listAccountSettingsResponse_settings,
    listAccountSettingsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListAccountSettings' smart constructor.
data ListAccountSettings = ListAccountSettings'
  { -- | The @nextToken@ value returned from a @ListAccountSettings@ request
    -- indicating that more results are available to fulfill the request and
    -- further calls will be needed. If @maxResults@ was provided, it is
    -- possible the number of results to be fewer than @maxResults@.
    --
    -- This token should be treated as an opaque identifier that is only used
    -- to retrieve the next items in a list and not for other programmatic
    -- purposes.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of account setting results returned by
    -- @ListAccountSettings@ in paginated output. When this parameter is used,
    -- @ListAccountSettings@ only returns @maxResults@ results in a single page
    -- along with a @nextToken@ response element. The remaining results of the
    -- initial request can be seen by sending another @ListAccountSettings@
    -- request with the returned @nextToken@ value. This value can be between 1
    -- and 10. If this parameter is not used, then @ListAccountSettings@
    -- returns up to 10 results and a @nextToken@ value if applicable.
    maxResults :: Core.Maybe Core.Int,
    -- | Specifies whether to return the effective settings. If @true@, the
    -- account settings for the root user or the default setting for the
    -- @principalArn@ are returned. If @false@, the account settings for the
    -- @principalArn@ are returned if they are set. Otherwise, no account
    -- settings are returned.
    effectiveSettings :: Core.Maybe Core.Bool,
    -- | The name of the account setting you want to list the settings for.
    name :: Core.Maybe SettingName,
    -- | The ARN of the principal, which can be an IAM user, IAM role, or the
    -- root user. If this field is omitted, the account settings are listed
    -- only for the authenticated user.
    principalArn :: Core.Maybe Core.Text,
    -- | The value of the account settings with which to filter results. You must
    -- also specify an account setting name to use this parameter.
    value :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAccountSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAccountSettings_nextToken' - The @nextToken@ value returned from a @ListAccountSettings@ request
-- indicating that more results are available to fulfill the request and
-- further calls will be needed. If @maxResults@ was provided, it is
-- possible the number of results to be fewer than @maxResults@.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
--
-- 'maxResults', 'listAccountSettings_maxResults' - The maximum number of account setting results returned by
-- @ListAccountSettings@ in paginated output. When this parameter is used,
-- @ListAccountSettings@ only returns @maxResults@ results in a single page
-- along with a @nextToken@ response element. The remaining results of the
-- initial request can be seen by sending another @ListAccountSettings@
-- request with the returned @nextToken@ value. This value can be between 1
-- and 10. If this parameter is not used, then @ListAccountSettings@
-- returns up to 10 results and a @nextToken@ value if applicable.
--
-- 'effectiveSettings', 'listAccountSettings_effectiveSettings' - Specifies whether to return the effective settings. If @true@, the
-- account settings for the root user or the default setting for the
-- @principalArn@ are returned. If @false@, the account settings for the
-- @principalArn@ are returned if they are set. Otherwise, no account
-- settings are returned.
--
-- 'name', 'listAccountSettings_name' - The name of the account setting you want to list the settings for.
--
-- 'principalArn', 'listAccountSettings_principalArn' - The ARN of the principal, which can be an IAM user, IAM role, or the
-- root user. If this field is omitted, the account settings are listed
-- only for the authenticated user.
--
-- 'value', 'listAccountSettings_value' - The value of the account settings with which to filter results. You must
-- also specify an account setting name to use this parameter.
newListAccountSettings ::
  ListAccountSettings
newListAccountSettings =
  ListAccountSettings'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      effectiveSettings = Core.Nothing,
      name = Core.Nothing,
      principalArn = Core.Nothing,
      value = Core.Nothing
    }

-- | The @nextToken@ value returned from a @ListAccountSettings@ request
-- indicating that more results are available to fulfill the request and
-- further calls will be needed. If @maxResults@ was provided, it is
-- possible the number of results to be fewer than @maxResults@.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
listAccountSettings_nextToken :: Lens.Lens' ListAccountSettings (Core.Maybe Core.Text)
listAccountSettings_nextToken = Lens.lens (\ListAccountSettings' {nextToken} -> nextToken) (\s@ListAccountSettings' {} a -> s {nextToken = a} :: ListAccountSettings)

-- | The maximum number of account setting results returned by
-- @ListAccountSettings@ in paginated output. When this parameter is used,
-- @ListAccountSettings@ only returns @maxResults@ results in a single page
-- along with a @nextToken@ response element. The remaining results of the
-- initial request can be seen by sending another @ListAccountSettings@
-- request with the returned @nextToken@ value. This value can be between 1
-- and 10. If this parameter is not used, then @ListAccountSettings@
-- returns up to 10 results and a @nextToken@ value if applicable.
listAccountSettings_maxResults :: Lens.Lens' ListAccountSettings (Core.Maybe Core.Int)
listAccountSettings_maxResults = Lens.lens (\ListAccountSettings' {maxResults} -> maxResults) (\s@ListAccountSettings' {} a -> s {maxResults = a} :: ListAccountSettings)

-- | Specifies whether to return the effective settings. If @true@, the
-- account settings for the root user or the default setting for the
-- @principalArn@ are returned. If @false@, the account settings for the
-- @principalArn@ are returned if they are set. Otherwise, no account
-- settings are returned.
listAccountSettings_effectiveSettings :: Lens.Lens' ListAccountSettings (Core.Maybe Core.Bool)
listAccountSettings_effectiveSettings = Lens.lens (\ListAccountSettings' {effectiveSettings} -> effectiveSettings) (\s@ListAccountSettings' {} a -> s {effectiveSettings = a} :: ListAccountSettings)

-- | The name of the account setting you want to list the settings for.
listAccountSettings_name :: Lens.Lens' ListAccountSettings (Core.Maybe SettingName)
listAccountSettings_name = Lens.lens (\ListAccountSettings' {name} -> name) (\s@ListAccountSettings' {} a -> s {name = a} :: ListAccountSettings)

-- | The ARN of the principal, which can be an IAM user, IAM role, or the
-- root user. If this field is omitted, the account settings are listed
-- only for the authenticated user.
listAccountSettings_principalArn :: Lens.Lens' ListAccountSettings (Core.Maybe Core.Text)
listAccountSettings_principalArn = Lens.lens (\ListAccountSettings' {principalArn} -> principalArn) (\s@ListAccountSettings' {} a -> s {principalArn = a} :: ListAccountSettings)

-- | The value of the account settings with which to filter results. You must
-- also specify an account setting name to use this parameter.
listAccountSettings_value :: Lens.Lens' ListAccountSettings (Core.Maybe Core.Text)
listAccountSettings_value = Lens.lens (\ListAccountSettings' {value} -> value) (\s@ListAccountSettings' {} a -> s {value = a} :: ListAccountSettings)

instance Core.AWSPager ListAccountSettings where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAccountSettingsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listAccountSettingsResponse_settings
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listAccountSettings_nextToken
          Lens..~ rs
          Lens.^? listAccountSettingsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListAccountSettings where
  type
    AWSResponse ListAccountSettings =
      ListAccountSettingsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAccountSettingsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "settings" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListAccountSettings

instance Core.NFData ListAccountSettings

instance Core.ToHeaders ListAccountSettings where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.ListAccountSettings" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListAccountSettings where
  toJSON ListAccountSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("effectiveSettings" Core..=)
              Core.<$> effectiveSettings,
            ("name" Core..=) Core.<$> name,
            ("principalArn" Core..=) Core.<$> principalArn,
            ("value" Core..=) Core.<$> value
          ]
      )

instance Core.ToPath ListAccountSettings where
  toPath = Core.const "/"

instance Core.ToQuery ListAccountSettings where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListAccountSettingsResponse' smart constructor.
data ListAccountSettingsResponse = ListAccountSettingsResponse'
  { -- | The @nextToken@ value to include in a future @ListAccountSettings@
    -- request. When the results of a @ListAccountSettings@ request exceed
    -- @maxResults@, this value can be used to retrieve the next page of
    -- results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The account settings for the resource.
    settings :: Core.Maybe [Setting],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAccountSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAccountSettingsResponse_nextToken' - The @nextToken@ value to include in a future @ListAccountSettings@
-- request. When the results of a @ListAccountSettings@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
--
-- 'settings', 'listAccountSettingsResponse_settings' - The account settings for the resource.
--
-- 'httpStatus', 'listAccountSettingsResponse_httpStatus' - The response's http status code.
newListAccountSettingsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListAccountSettingsResponse
newListAccountSettingsResponse pHttpStatus_ =
  ListAccountSettingsResponse'
    { nextToken =
        Core.Nothing,
      settings = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ value to include in a future @ListAccountSettings@
-- request. When the results of a @ListAccountSettings@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
listAccountSettingsResponse_nextToken :: Lens.Lens' ListAccountSettingsResponse (Core.Maybe Core.Text)
listAccountSettingsResponse_nextToken = Lens.lens (\ListAccountSettingsResponse' {nextToken} -> nextToken) (\s@ListAccountSettingsResponse' {} a -> s {nextToken = a} :: ListAccountSettingsResponse)

-- | The account settings for the resource.
listAccountSettingsResponse_settings :: Lens.Lens' ListAccountSettingsResponse (Core.Maybe [Setting])
listAccountSettingsResponse_settings = Lens.lens (\ListAccountSettingsResponse' {settings} -> settings) (\s@ListAccountSettingsResponse' {} a -> s {settings = a} :: ListAccountSettingsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listAccountSettingsResponse_httpStatus :: Lens.Lens' ListAccountSettingsResponse Core.Int
listAccountSettingsResponse_httpStatus = Lens.lens (\ListAccountSettingsResponse' {httpStatus} -> httpStatus) (\s@ListAccountSettingsResponse' {} a -> s {httpStatus = a} :: ListAccountSettingsResponse)

instance Core.NFData ListAccountSettingsResponse
