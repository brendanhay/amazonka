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
-- Module      : Amazonka.Config.DescribeRetentionConfigurations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of one or more retention configurations. If the
-- retention configuration name is not specified, this action returns the
-- details for all the retention configurations for that account.
--
-- Currently, Config supports only one retention configuration per region
-- in your account.
--
-- This operation returns paginated results.
module Amazonka.Config.DescribeRetentionConfigurations
  ( -- * Creating a Request
    DescribeRetentionConfigurations (..),
    newDescribeRetentionConfigurations,

    -- * Request Lenses
    describeRetentionConfigurations_nextToken,
    describeRetentionConfigurations_retentionConfigurationNames,

    -- * Destructuring the Response
    DescribeRetentionConfigurationsResponse (..),
    newDescribeRetentionConfigurationsResponse,

    -- * Response Lenses
    describeRetentionConfigurationsResponse_nextToken,
    describeRetentionConfigurationsResponse_retentionConfigurations,
    describeRetentionConfigurationsResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeRetentionConfigurations' smart constructor.
data DescribeRetentionConfigurations = DescribeRetentionConfigurations'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of names of retention configurations for which you want details.
    -- If you do not specify a name, Config returns details for all the
    -- retention configurations for that account.
    --
    -- Currently, Config supports only one retention configuration per region
    -- in your account.
    retentionConfigurationNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRetentionConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeRetentionConfigurations_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'retentionConfigurationNames', 'describeRetentionConfigurations_retentionConfigurationNames' - A list of names of retention configurations for which you want details.
-- If you do not specify a name, Config returns details for all the
-- retention configurations for that account.
--
-- Currently, Config supports only one retention configuration per region
-- in your account.
newDescribeRetentionConfigurations ::
  DescribeRetentionConfigurations
newDescribeRetentionConfigurations =
  DescribeRetentionConfigurations'
    { nextToken =
        Prelude.Nothing,
      retentionConfigurationNames =
        Prelude.Nothing
    }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeRetentionConfigurations_nextToken :: Lens.Lens' DescribeRetentionConfigurations (Prelude.Maybe Prelude.Text)
describeRetentionConfigurations_nextToken = Lens.lens (\DescribeRetentionConfigurations' {nextToken} -> nextToken) (\s@DescribeRetentionConfigurations' {} a -> s {nextToken = a} :: DescribeRetentionConfigurations)

-- | A list of names of retention configurations for which you want details.
-- If you do not specify a name, Config returns details for all the
-- retention configurations for that account.
--
-- Currently, Config supports only one retention configuration per region
-- in your account.
describeRetentionConfigurations_retentionConfigurationNames :: Lens.Lens' DescribeRetentionConfigurations (Prelude.Maybe [Prelude.Text])
describeRetentionConfigurations_retentionConfigurationNames = Lens.lens (\DescribeRetentionConfigurations' {retentionConfigurationNames} -> retentionConfigurationNames) (\s@DescribeRetentionConfigurations' {} a -> s {retentionConfigurationNames = a} :: DescribeRetentionConfigurations) Prelude.. Lens.mapping Lens.coerced

instance
  Core.AWSPager
    DescribeRetentionConfigurations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeRetentionConfigurationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeRetentionConfigurationsResponse_retentionConfigurations
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeRetentionConfigurations_nextToken
          Lens..~ rs
          Lens.^? describeRetentionConfigurationsResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeRetentionConfigurations
  where
  type
    AWSResponse DescribeRetentionConfigurations =
      DescribeRetentionConfigurationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRetentionConfigurationsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "RetentionConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeRetentionConfigurations
  where
  hashWithSalt
    _salt
    DescribeRetentionConfigurations' {..} =
      _salt
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` retentionConfigurationNames

instance
  Prelude.NFData
    DescribeRetentionConfigurations
  where
  rnf DescribeRetentionConfigurations' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf retentionConfigurationNames

instance
  Data.ToHeaders
    DescribeRetentionConfigurations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.DescribeRetentionConfigurations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeRetentionConfigurations where
  toJSON DescribeRetentionConfigurations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("RetentionConfigurationNames" Data..=)
              Prelude.<$> retentionConfigurationNames
          ]
      )

instance Data.ToPath DescribeRetentionConfigurations where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeRetentionConfigurations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRetentionConfigurationsResponse' smart constructor.
data DescribeRetentionConfigurationsResponse = DescribeRetentionConfigurationsResponse'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns a retention configuration object.
    retentionConfigurations :: Prelude.Maybe [RetentionConfiguration],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRetentionConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeRetentionConfigurationsResponse_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'retentionConfigurations', 'describeRetentionConfigurationsResponse_retentionConfigurations' - Returns a retention configuration object.
--
-- 'httpStatus', 'describeRetentionConfigurationsResponse_httpStatus' - The response's http status code.
newDescribeRetentionConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRetentionConfigurationsResponse
newDescribeRetentionConfigurationsResponse
  pHttpStatus_ =
    DescribeRetentionConfigurationsResponse'
      { nextToken =
          Prelude.Nothing,
        retentionConfigurations =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeRetentionConfigurationsResponse_nextToken :: Lens.Lens' DescribeRetentionConfigurationsResponse (Prelude.Maybe Prelude.Text)
describeRetentionConfigurationsResponse_nextToken = Lens.lens (\DescribeRetentionConfigurationsResponse' {nextToken} -> nextToken) (\s@DescribeRetentionConfigurationsResponse' {} a -> s {nextToken = a} :: DescribeRetentionConfigurationsResponse)

-- | Returns a retention configuration object.
describeRetentionConfigurationsResponse_retentionConfigurations :: Lens.Lens' DescribeRetentionConfigurationsResponse (Prelude.Maybe [RetentionConfiguration])
describeRetentionConfigurationsResponse_retentionConfigurations = Lens.lens (\DescribeRetentionConfigurationsResponse' {retentionConfigurations} -> retentionConfigurations) (\s@DescribeRetentionConfigurationsResponse' {} a -> s {retentionConfigurations = a} :: DescribeRetentionConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeRetentionConfigurationsResponse_httpStatus :: Lens.Lens' DescribeRetentionConfigurationsResponse Prelude.Int
describeRetentionConfigurationsResponse_httpStatus = Lens.lens (\DescribeRetentionConfigurationsResponse' {httpStatus} -> httpStatus) (\s@DescribeRetentionConfigurationsResponse' {} a -> s {httpStatus = a} :: DescribeRetentionConfigurationsResponse)

instance
  Prelude.NFData
    DescribeRetentionConfigurationsResponse
  where
  rnf DescribeRetentionConfigurationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf retentionConfigurations
      `Prelude.seq` Prelude.rnf httpStatus
