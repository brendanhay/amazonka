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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    describeRetentionConfigurations_retentionConfigurationNames,
    describeRetentionConfigurations_nextToken,

    -- * Destructuring the Response
    DescribeRetentionConfigurationsResponse (..),
    newDescribeRetentionConfigurationsResponse,

    -- * Response Lenses
    describeRetentionConfigurationsResponse_retentionConfigurations,
    describeRetentionConfigurationsResponse_nextToken,
    describeRetentionConfigurationsResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeRetentionConfigurations' smart constructor.
data DescribeRetentionConfigurations = DescribeRetentionConfigurations'
  { -- | A list of names of retention configurations for which you want details.
    -- If you do not specify a name, Config returns details for all the
    -- retention configurations for that account.
    --
    -- Currently, Config supports only one retention configuration per region
    -- in your account.
    retentionConfigurationNames :: Prelude.Maybe [Prelude.Text],
    -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'retentionConfigurationNames', 'describeRetentionConfigurations_retentionConfigurationNames' - A list of names of retention configurations for which you want details.
-- If you do not specify a name, Config returns details for all the
-- retention configurations for that account.
--
-- Currently, Config supports only one retention configuration per region
-- in your account.
--
-- 'nextToken', 'describeRetentionConfigurations_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
newDescribeRetentionConfigurations ::
  DescribeRetentionConfigurations
newDescribeRetentionConfigurations =
  DescribeRetentionConfigurations'
    { retentionConfigurationNames =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | A list of names of retention configurations for which you want details.
-- If you do not specify a name, Config returns details for all the
-- retention configurations for that account.
--
-- Currently, Config supports only one retention configuration per region
-- in your account.
describeRetentionConfigurations_retentionConfigurationNames :: Lens.Lens' DescribeRetentionConfigurations (Prelude.Maybe [Prelude.Text])
describeRetentionConfigurations_retentionConfigurationNames = Lens.lens (\DescribeRetentionConfigurations' {retentionConfigurationNames} -> retentionConfigurationNames) (\s@DescribeRetentionConfigurations' {} a -> s {retentionConfigurationNames = a} :: DescribeRetentionConfigurations) Prelude.. Lens.mapping Lens.coerced

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeRetentionConfigurations_nextToken :: Lens.Lens' DescribeRetentionConfigurations (Prelude.Maybe Prelude.Text)
describeRetentionConfigurations_nextToken = Lens.lens (\DescribeRetentionConfigurations' {nextToken} -> nextToken) (\s@DescribeRetentionConfigurations' {} a -> s {nextToken = a} :: DescribeRetentionConfigurations)

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
      Prelude.Just Prelude.$
        rq
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRetentionConfigurationsResponse'
            Prelude.<$> ( x Core..?> "RetentionConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextToken")
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
        `Prelude.hashWithSalt` retentionConfigurationNames
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    DescribeRetentionConfigurations
  where
  rnf DescribeRetentionConfigurations' {..} =
    Prelude.rnf retentionConfigurationNames
      `Prelude.seq` Prelude.rnf nextToken

instance
  Core.ToHeaders
    DescribeRetentionConfigurations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DescribeRetentionConfigurations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeRetentionConfigurations where
  toJSON DescribeRetentionConfigurations' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RetentionConfigurationNames" Core..=)
              Prelude.<$> retentionConfigurationNames,
            ("NextToken" Core..=) Prelude.<$> nextToken
          ]
      )

instance Core.ToPath DescribeRetentionConfigurations where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeRetentionConfigurations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRetentionConfigurationsResponse' smart constructor.
data DescribeRetentionConfigurationsResponse = DescribeRetentionConfigurationsResponse'
  { -- | Returns a retention configuration object.
    retentionConfigurations :: Prelude.Maybe [RetentionConfiguration],
    -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'retentionConfigurations', 'describeRetentionConfigurationsResponse_retentionConfigurations' - Returns a retention configuration object.
--
-- 'nextToken', 'describeRetentionConfigurationsResponse_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'httpStatus', 'describeRetentionConfigurationsResponse_httpStatus' - The response's http status code.
newDescribeRetentionConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRetentionConfigurationsResponse
newDescribeRetentionConfigurationsResponse
  pHttpStatus_ =
    DescribeRetentionConfigurationsResponse'
      { retentionConfigurations =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Returns a retention configuration object.
describeRetentionConfigurationsResponse_retentionConfigurations :: Lens.Lens' DescribeRetentionConfigurationsResponse (Prelude.Maybe [RetentionConfiguration])
describeRetentionConfigurationsResponse_retentionConfigurations = Lens.lens (\DescribeRetentionConfigurationsResponse' {retentionConfigurations} -> retentionConfigurations) (\s@DescribeRetentionConfigurationsResponse' {} a -> s {retentionConfigurations = a} :: DescribeRetentionConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeRetentionConfigurationsResponse_nextToken :: Lens.Lens' DescribeRetentionConfigurationsResponse (Prelude.Maybe Prelude.Text)
describeRetentionConfigurationsResponse_nextToken = Lens.lens (\DescribeRetentionConfigurationsResponse' {nextToken} -> nextToken) (\s@DescribeRetentionConfigurationsResponse' {} a -> s {nextToken = a} :: DescribeRetentionConfigurationsResponse)

-- | The response's http status code.
describeRetentionConfigurationsResponse_httpStatus :: Lens.Lens' DescribeRetentionConfigurationsResponse Prelude.Int
describeRetentionConfigurationsResponse_httpStatus = Lens.lens (\DescribeRetentionConfigurationsResponse' {httpStatus} -> httpStatus) (\s@DescribeRetentionConfigurationsResponse' {} a -> s {httpStatus = a} :: DescribeRetentionConfigurationsResponse)

instance
  Prelude.NFData
    DescribeRetentionConfigurationsResponse
  where
  rnf DescribeRetentionConfigurationsResponse' {..} =
    Prelude.rnf retentionConfigurations
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
