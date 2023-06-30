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
-- Module      : Amazonka.LicenseManager.ListUsageForLicenseConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all license usage records for a license configuration, displaying
-- license consumption details by resource at a selected point in time. Use
-- this action to audit the current license consumption for any license
-- inventory and configuration.
--
-- This operation returns paginated results.
module Amazonka.LicenseManager.ListUsageForLicenseConfiguration
  ( -- * Creating a Request
    ListUsageForLicenseConfiguration (..),
    newListUsageForLicenseConfiguration,

    -- * Request Lenses
    listUsageForLicenseConfiguration_filters,
    listUsageForLicenseConfiguration_maxResults,
    listUsageForLicenseConfiguration_nextToken,
    listUsageForLicenseConfiguration_licenseConfigurationArn,

    -- * Destructuring the Response
    ListUsageForLicenseConfigurationResponse (..),
    newListUsageForLicenseConfigurationResponse,

    -- * Response Lenses
    listUsageForLicenseConfigurationResponse_licenseConfigurationUsageList,
    listUsageForLicenseConfigurationResponse_nextToken,
    listUsageForLicenseConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListUsageForLicenseConfiguration' smart constructor.
data ListUsageForLicenseConfiguration = ListUsageForLicenseConfiguration'
  { -- | Filters to scope the results. The following filters and logical
    -- operators are supported:
    --
    -- -   @resourceArn@ - The ARN of the license configuration resource.
    --     Logical operators are @EQUALS@ | @NOT_EQUALS@.
    --
    -- -   @resourceType@ - The resource type (@EC2_INSTANCE@ | @EC2_HOST@ |
    --     @EC2_AMI@ | @SYSTEMS_MANAGER_MANAGED_INSTANCE@). Logical operators
    --     are @EQUALS@ | @NOT_EQUALS@.
    --
    -- -   @resourceAccount@ - The ID of the account that owns the resource.
    --     Logical operators are @EQUALS@ | @NOT_EQUALS@.
    filters :: Prelude.Maybe [Filter],
    -- | Maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the license configuration.
    licenseConfigurationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUsageForLicenseConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listUsageForLicenseConfiguration_filters' - Filters to scope the results. The following filters and logical
-- operators are supported:
--
-- -   @resourceArn@ - The ARN of the license configuration resource.
--     Logical operators are @EQUALS@ | @NOT_EQUALS@.
--
-- -   @resourceType@ - The resource type (@EC2_INSTANCE@ | @EC2_HOST@ |
--     @EC2_AMI@ | @SYSTEMS_MANAGER_MANAGED_INSTANCE@). Logical operators
--     are @EQUALS@ | @NOT_EQUALS@.
--
-- -   @resourceAccount@ - The ID of the account that owns the resource.
--     Logical operators are @EQUALS@ | @NOT_EQUALS@.
--
-- 'maxResults', 'listUsageForLicenseConfiguration_maxResults' - Maximum number of results to return in a single call.
--
-- 'nextToken', 'listUsageForLicenseConfiguration_nextToken' - Token for the next set of results.
--
-- 'licenseConfigurationArn', 'listUsageForLicenseConfiguration_licenseConfigurationArn' - Amazon Resource Name (ARN) of the license configuration.
newListUsageForLicenseConfiguration ::
  -- | 'licenseConfigurationArn'
  Prelude.Text ->
  ListUsageForLicenseConfiguration
newListUsageForLicenseConfiguration
  pLicenseConfigurationArn_ =
    ListUsageForLicenseConfiguration'
      { filters =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        licenseConfigurationArn =
          pLicenseConfigurationArn_
      }

-- | Filters to scope the results. The following filters and logical
-- operators are supported:
--
-- -   @resourceArn@ - The ARN of the license configuration resource.
--     Logical operators are @EQUALS@ | @NOT_EQUALS@.
--
-- -   @resourceType@ - The resource type (@EC2_INSTANCE@ | @EC2_HOST@ |
--     @EC2_AMI@ | @SYSTEMS_MANAGER_MANAGED_INSTANCE@). Logical operators
--     are @EQUALS@ | @NOT_EQUALS@.
--
-- -   @resourceAccount@ - The ID of the account that owns the resource.
--     Logical operators are @EQUALS@ | @NOT_EQUALS@.
listUsageForLicenseConfiguration_filters :: Lens.Lens' ListUsageForLicenseConfiguration (Prelude.Maybe [Filter])
listUsageForLicenseConfiguration_filters = Lens.lens (\ListUsageForLicenseConfiguration' {filters} -> filters) (\s@ListUsageForLicenseConfiguration' {} a -> s {filters = a} :: ListUsageForLicenseConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Maximum number of results to return in a single call.
listUsageForLicenseConfiguration_maxResults :: Lens.Lens' ListUsageForLicenseConfiguration (Prelude.Maybe Prelude.Int)
listUsageForLicenseConfiguration_maxResults = Lens.lens (\ListUsageForLicenseConfiguration' {maxResults} -> maxResults) (\s@ListUsageForLicenseConfiguration' {} a -> s {maxResults = a} :: ListUsageForLicenseConfiguration)

-- | Token for the next set of results.
listUsageForLicenseConfiguration_nextToken :: Lens.Lens' ListUsageForLicenseConfiguration (Prelude.Maybe Prelude.Text)
listUsageForLicenseConfiguration_nextToken = Lens.lens (\ListUsageForLicenseConfiguration' {nextToken} -> nextToken) (\s@ListUsageForLicenseConfiguration' {} a -> s {nextToken = a} :: ListUsageForLicenseConfiguration)

-- | Amazon Resource Name (ARN) of the license configuration.
listUsageForLicenseConfiguration_licenseConfigurationArn :: Lens.Lens' ListUsageForLicenseConfiguration Prelude.Text
listUsageForLicenseConfiguration_licenseConfigurationArn = Lens.lens (\ListUsageForLicenseConfiguration' {licenseConfigurationArn} -> licenseConfigurationArn) (\s@ListUsageForLicenseConfiguration' {} a -> s {licenseConfigurationArn = a} :: ListUsageForLicenseConfiguration)

instance
  Core.AWSPager
    ListUsageForLicenseConfiguration
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listUsageForLicenseConfigurationResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listUsageForLicenseConfigurationResponse_licenseConfigurationUsageList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listUsageForLicenseConfiguration_nextToken
          Lens..~ rs
          Lens.^? listUsageForLicenseConfigurationResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListUsageForLicenseConfiguration
  where
  type
    AWSResponse ListUsageForLicenseConfiguration =
      ListUsageForLicenseConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUsageForLicenseConfigurationResponse'
            Prelude.<$> ( x
                            Data..?> "LicenseConfigurationUsageList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListUsageForLicenseConfiguration
  where
  hashWithSalt
    _salt
    ListUsageForLicenseConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` licenseConfigurationArn

instance
  Prelude.NFData
    ListUsageForLicenseConfiguration
  where
  rnf ListUsageForLicenseConfiguration' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf licenseConfigurationArn

instance
  Data.ToHeaders
    ListUsageForLicenseConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLicenseManager.ListUsageForLicenseConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListUsageForLicenseConfiguration where
  toJSON ListUsageForLicenseConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ( "LicenseConfigurationArn"
                  Data..= licenseConfigurationArn
              )
          ]
      )

instance Data.ToPath ListUsageForLicenseConfiguration where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListUsageForLicenseConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListUsageForLicenseConfigurationResponse' smart constructor.
data ListUsageForLicenseConfigurationResponse = ListUsageForLicenseConfigurationResponse'
  { -- | Information about the license configurations.
    licenseConfigurationUsageList :: Prelude.Maybe [LicenseConfigurationUsage],
    -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUsageForLicenseConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'licenseConfigurationUsageList', 'listUsageForLicenseConfigurationResponse_licenseConfigurationUsageList' - Information about the license configurations.
--
-- 'nextToken', 'listUsageForLicenseConfigurationResponse_nextToken' - Token for the next set of results.
--
-- 'httpStatus', 'listUsageForLicenseConfigurationResponse_httpStatus' - The response's http status code.
newListUsageForLicenseConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListUsageForLicenseConfigurationResponse
newListUsageForLicenseConfigurationResponse
  pHttpStatus_ =
    ListUsageForLicenseConfigurationResponse'
      { licenseConfigurationUsageList =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the license configurations.
listUsageForLicenseConfigurationResponse_licenseConfigurationUsageList :: Lens.Lens' ListUsageForLicenseConfigurationResponse (Prelude.Maybe [LicenseConfigurationUsage])
listUsageForLicenseConfigurationResponse_licenseConfigurationUsageList = Lens.lens (\ListUsageForLicenseConfigurationResponse' {licenseConfigurationUsageList} -> licenseConfigurationUsageList) (\s@ListUsageForLicenseConfigurationResponse' {} a -> s {licenseConfigurationUsageList = a} :: ListUsageForLicenseConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | Token for the next set of results.
listUsageForLicenseConfigurationResponse_nextToken :: Lens.Lens' ListUsageForLicenseConfigurationResponse (Prelude.Maybe Prelude.Text)
listUsageForLicenseConfigurationResponse_nextToken = Lens.lens (\ListUsageForLicenseConfigurationResponse' {nextToken} -> nextToken) (\s@ListUsageForLicenseConfigurationResponse' {} a -> s {nextToken = a} :: ListUsageForLicenseConfigurationResponse)

-- | The response's http status code.
listUsageForLicenseConfigurationResponse_httpStatus :: Lens.Lens' ListUsageForLicenseConfigurationResponse Prelude.Int
listUsageForLicenseConfigurationResponse_httpStatus = Lens.lens (\ListUsageForLicenseConfigurationResponse' {httpStatus} -> httpStatus) (\s@ListUsageForLicenseConfigurationResponse' {} a -> s {httpStatus = a} :: ListUsageForLicenseConfigurationResponse)

instance
  Prelude.NFData
    ListUsageForLicenseConfigurationResponse
  where
  rnf ListUsageForLicenseConfigurationResponse' {..} =
    Prelude.rnf licenseConfigurationUsageList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
