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
-- Module      : Amazonka.ECS.ListAccountSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the account settings for a specified principal.
--
-- This operation returns paginated results.
module Amazonka.ECS.ListAccountSettings
  ( -- * Creating a Request
    ListAccountSettings (..),
    newListAccountSettings,

    -- * Request Lenses
    listAccountSettings_effectiveSettings,
    listAccountSettings_maxResults,
    listAccountSettings_name,
    listAccountSettings_nextToken,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAccountSettings' smart constructor.
data ListAccountSettings = ListAccountSettings'
  { -- | Determines whether to return the effective settings. If @true@, the
    -- account settings for the root user or the default setting for the
    -- @principalArn@ are returned. If @false@, the account settings for the
    -- @principalArn@ are returned if they\'re set. Otherwise, no account
    -- settings are returned.
    effectiveSettings :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of account setting results returned by
    -- @ListAccountSettings@ in paginated output. When this parameter is used,
    -- @ListAccountSettings@ only returns @maxResults@ results in a single page
    -- along with a @nextToken@ response element. The remaining results of the
    -- initial request can be seen by sending another @ListAccountSettings@
    -- request with the returned @nextToken@ value. This value can be between 1
    -- and 10. If this parameter isn\'t used, then @ListAccountSettings@
    -- returns up to 10 results and a @nextToken@ value if applicable.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The name of the account setting you want to list the settings for.
    name :: Prelude.Maybe SettingName,
    -- | The @nextToken@ value returned from a @ListAccountSettings@ request
    -- indicating that more results are available to fulfill the request and
    -- further calls will be needed. If @maxResults@ was provided, it\'s
    -- possible the number of results to be fewer than @maxResults@.
    --
    -- This token should be treated as an opaque identifier that is only used
    -- to retrieve the next items in a list and not for other programmatic
    -- purposes.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the principal, which can be an IAM user, IAM role, or the
    -- root user. If this field is omitted, the account settings are listed
    -- only for the authenticated user.
    --
    -- Federated users assume the account setting of the root user and can\'t
    -- have explicit account settings set for them.
    principalArn :: Prelude.Maybe Prelude.Text,
    -- | The value of the account settings to filter results with. You must also
    -- specify an account setting name to use this parameter.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccountSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'effectiveSettings', 'listAccountSettings_effectiveSettings' - Determines whether to return the effective settings. If @true@, the
-- account settings for the root user or the default setting for the
-- @principalArn@ are returned. If @false@, the account settings for the
-- @principalArn@ are returned if they\'re set. Otherwise, no account
-- settings are returned.
--
-- 'maxResults', 'listAccountSettings_maxResults' - The maximum number of account setting results returned by
-- @ListAccountSettings@ in paginated output. When this parameter is used,
-- @ListAccountSettings@ only returns @maxResults@ results in a single page
-- along with a @nextToken@ response element. The remaining results of the
-- initial request can be seen by sending another @ListAccountSettings@
-- request with the returned @nextToken@ value. This value can be between 1
-- and 10. If this parameter isn\'t used, then @ListAccountSettings@
-- returns up to 10 results and a @nextToken@ value if applicable.
--
-- 'name', 'listAccountSettings_name' - The name of the account setting you want to list the settings for.
--
-- 'nextToken', 'listAccountSettings_nextToken' - The @nextToken@ value returned from a @ListAccountSettings@ request
-- indicating that more results are available to fulfill the request and
-- further calls will be needed. If @maxResults@ was provided, it\'s
-- possible the number of results to be fewer than @maxResults@.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
--
-- 'principalArn', 'listAccountSettings_principalArn' - The ARN of the principal, which can be an IAM user, IAM role, or the
-- root user. If this field is omitted, the account settings are listed
-- only for the authenticated user.
--
-- Federated users assume the account setting of the root user and can\'t
-- have explicit account settings set for them.
--
-- 'value', 'listAccountSettings_value' - The value of the account settings to filter results with. You must also
-- specify an account setting name to use this parameter.
newListAccountSettings ::
  ListAccountSettings
newListAccountSettings =
  ListAccountSettings'
    { effectiveSettings =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      name = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      principalArn = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Determines whether to return the effective settings. If @true@, the
-- account settings for the root user or the default setting for the
-- @principalArn@ are returned. If @false@, the account settings for the
-- @principalArn@ are returned if they\'re set. Otherwise, no account
-- settings are returned.
listAccountSettings_effectiveSettings :: Lens.Lens' ListAccountSettings (Prelude.Maybe Prelude.Bool)
listAccountSettings_effectiveSettings = Lens.lens (\ListAccountSettings' {effectiveSettings} -> effectiveSettings) (\s@ListAccountSettings' {} a -> s {effectiveSettings = a} :: ListAccountSettings)

-- | The maximum number of account setting results returned by
-- @ListAccountSettings@ in paginated output. When this parameter is used,
-- @ListAccountSettings@ only returns @maxResults@ results in a single page
-- along with a @nextToken@ response element. The remaining results of the
-- initial request can be seen by sending another @ListAccountSettings@
-- request with the returned @nextToken@ value. This value can be between 1
-- and 10. If this parameter isn\'t used, then @ListAccountSettings@
-- returns up to 10 results and a @nextToken@ value if applicable.
listAccountSettings_maxResults :: Lens.Lens' ListAccountSettings (Prelude.Maybe Prelude.Int)
listAccountSettings_maxResults = Lens.lens (\ListAccountSettings' {maxResults} -> maxResults) (\s@ListAccountSettings' {} a -> s {maxResults = a} :: ListAccountSettings)

-- | The name of the account setting you want to list the settings for.
listAccountSettings_name :: Lens.Lens' ListAccountSettings (Prelude.Maybe SettingName)
listAccountSettings_name = Lens.lens (\ListAccountSettings' {name} -> name) (\s@ListAccountSettings' {} a -> s {name = a} :: ListAccountSettings)

-- | The @nextToken@ value returned from a @ListAccountSettings@ request
-- indicating that more results are available to fulfill the request and
-- further calls will be needed. If @maxResults@ was provided, it\'s
-- possible the number of results to be fewer than @maxResults@.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
listAccountSettings_nextToken :: Lens.Lens' ListAccountSettings (Prelude.Maybe Prelude.Text)
listAccountSettings_nextToken = Lens.lens (\ListAccountSettings' {nextToken} -> nextToken) (\s@ListAccountSettings' {} a -> s {nextToken = a} :: ListAccountSettings)

-- | The ARN of the principal, which can be an IAM user, IAM role, or the
-- root user. If this field is omitted, the account settings are listed
-- only for the authenticated user.
--
-- Federated users assume the account setting of the root user and can\'t
-- have explicit account settings set for them.
listAccountSettings_principalArn :: Lens.Lens' ListAccountSettings (Prelude.Maybe Prelude.Text)
listAccountSettings_principalArn = Lens.lens (\ListAccountSettings' {principalArn} -> principalArn) (\s@ListAccountSettings' {} a -> s {principalArn = a} :: ListAccountSettings)

-- | The value of the account settings to filter results with. You must also
-- specify an account setting name to use this parameter.
listAccountSettings_value :: Lens.Lens' ListAccountSettings (Prelude.Maybe Prelude.Text)
listAccountSettings_value = Lens.lens (\ListAccountSettings' {value} -> value) (\s@ListAccountSettings' {} a -> s {value = a} :: ListAccountSettings)

instance Core.AWSPager ListAccountSettings where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAccountSettingsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAccountSettingsResponse_settings
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAccountSettings_nextToken
          Lens..~ rs
          Lens.^? listAccountSettingsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListAccountSettings where
  type
    AWSResponse ListAccountSettings =
      ListAccountSettingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAccountSettingsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "settings" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAccountSettings where
  hashWithSalt _salt ListAccountSettings' {..} =
    _salt `Prelude.hashWithSalt` effectiveSettings
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` principalArn
      `Prelude.hashWithSalt` value

instance Prelude.NFData ListAccountSettings where
  rnf ListAccountSettings' {..} =
    Prelude.rnf effectiveSettings
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf principalArn
      `Prelude.seq` Prelude.rnf value

instance Data.ToHeaders ListAccountSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.ListAccountSettings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAccountSettings where
  toJSON ListAccountSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("effectiveSettings" Data..=)
              Prelude.<$> effectiveSettings,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("name" Data..=) Prelude.<$> name,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("principalArn" Data..=) Prelude.<$> principalArn,
            ("value" Data..=) Prelude.<$> value
          ]
      )

instance Data.ToPath ListAccountSettings where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAccountSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAccountSettingsResponse' smart constructor.
data ListAccountSettingsResponse = ListAccountSettingsResponse'
  { -- | The @nextToken@ value to include in a future @ListAccountSettings@
    -- request. When the results of a @ListAccountSettings@ request exceed
    -- @maxResults@, this value can be used to retrieve the next page of
    -- results. This value is @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The account settings for the resource.
    settings :: Prelude.Maybe [Setting],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListAccountSettingsResponse
newListAccountSettingsResponse pHttpStatus_ =
  ListAccountSettingsResponse'
    { nextToken =
        Prelude.Nothing,
      settings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ value to include in a future @ListAccountSettings@
-- request. When the results of a @ListAccountSettings@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
listAccountSettingsResponse_nextToken :: Lens.Lens' ListAccountSettingsResponse (Prelude.Maybe Prelude.Text)
listAccountSettingsResponse_nextToken = Lens.lens (\ListAccountSettingsResponse' {nextToken} -> nextToken) (\s@ListAccountSettingsResponse' {} a -> s {nextToken = a} :: ListAccountSettingsResponse)

-- | The account settings for the resource.
listAccountSettingsResponse_settings :: Lens.Lens' ListAccountSettingsResponse (Prelude.Maybe [Setting])
listAccountSettingsResponse_settings = Lens.lens (\ListAccountSettingsResponse' {settings} -> settings) (\s@ListAccountSettingsResponse' {} a -> s {settings = a} :: ListAccountSettingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAccountSettingsResponse_httpStatus :: Lens.Lens' ListAccountSettingsResponse Prelude.Int
listAccountSettingsResponse_httpStatus = Lens.lens (\ListAccountSettingsResponse' {httpStatus} -> httpStatus) (\s@ListAccountSettingsResponse' {} a -> s {httpStatus = a} :: ListAccountSettingsResponse)

instance Prelude.NFData ListAccountSettingsResponse where
  rnf ListAccountSettingsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf settings
      `Prelude.seq` Prelude.rnf httpStatus
