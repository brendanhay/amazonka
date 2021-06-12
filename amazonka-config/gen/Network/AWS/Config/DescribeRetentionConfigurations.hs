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
-- Module      : Network.AWS.Config.DescribeRetentionConfigurations
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
-- Currently, AWS Config supports only one retention configuration per
-- region in your account.
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeRetentionConfigurations
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

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeRetentionConfigurations' smart constructor.
data DescribeRetentionConfigurations = DescribeRetentionConfigurations'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of names of retention configurations for which you want details.
    -- If you do not specify a name, AWS Config returns details for all the
    -- retention configurations for that account.
    --
    -- Currently, AWS Config supports only one retention configuration per
    -- region in your account.
    retentionConfigurationNames :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- If you do not specify a name, AWS Config returns details for all the
-- retention configurations for that account.
--
-- Currently, AWS Config supports only one retention configuration per
-- region in your account.
newDescribeRetentionConfigurations ::
  DescribeRetentionConfigurations
newDescribeRetentionConfigurations =
  DescribeRetentionConfigurations'
    { nextToken =
        Core.Nothing,
      retentionConfigurationNames = Core.Nothing
    }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeRetentionConfigurations_nextToken :: Lens.Lens' DescribeRetentionConfigurations (Core.Maybe Core.Text)
describeRetentionConfigurations_nextToken = Lens.lens (\DescribeRetentionConfigurations' {nextToken} -> nextToken) (\s@DescribeRetentionConfigurations' {} a -> s {nextToken = a} :: DescribeRetentionConfigurations)

-- | A list of names of retention configurations for which you want details.
-- If you do not specify a name, AWS Config returns details for all the
-- retention configurations for that account.
--
-- Currently, AWS Config supports only one retention configuration per
-- region in your account.
describeRetentionConfigurations_retentionConfigurationNames :: Lens.Lens' DescribeRetentionConfigurations (Core.Maybe [Core.Text])
describeRetentionConfigurations_retentionConfigurationNames = Lens.lens (\DescribeRetentionConfigurations' {retentionConfigurationNames} -> retentionConfigurationNames) (\s@DescribeRetentionConfigurations' {} a -> s {retentionConfigurationNames = a} :: DescribeRetentionConfigurations) Core.. Lens.mapping Lens._Coerce

instance
  Core.AWSPager
    DescribeRetentionConfigurations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeRetentionConfigurationsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeRetentionConfigurationsResponse_retentionConfigurations
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeRetentionConfigurations_nextToken
          Lens..~ rs
          Lens.^? describeRetentionConfigurationsResponse_nextToken
            Core.. Lens._Just

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
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "RetentionConfigurations"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeRetentionConfigurations

instance Core.NFData DescribeRetentionConfigurations

instance
  Core.ToHeaders
    DescribeRetentionConfigurations
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DescribeRetentionConfigurations" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeRetentionConfigurations where
  toJSON DescribeRetentionConfigurations' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("RetentionConfigurationNames" Core..=)
              Core.<$> retentionConfigurationNames
          ]
      )

instance Core.ToPath DescribeRetentionConfigurations where
  toPath = Core.const "/"

instance Core.ToQuery DescribeRetentionConfigurations where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeRetentionConfigurationsResponse' smart constructor.
data DescribeRetentionConfigurationsResponse = DescribeRetentionConfigurationsResponse'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | Returns a retention configuration object.
    retentionConfigurations :: Core.Maybe [RetentionConfiguration],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeRetentionConfigurationsResponse
newDescribeRetentionConfigurationsResponse
  pHttpStatus_ =
    DescribeRetentionConfigurationsResponse'
      { nextToken =
          Core.Nothing,
        retentionConfigurations =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeRetentionConfigurationsResponse_nextToken :: Lens.Lens' DescribeRetentionConfigurationsResponse (Core.Maybe Core.Text)
describeRetentionConfigurationsResponse_nextToken = Lens.lens (\DescribeRetentionConfigurationsResponse' {nextToken} -> nextToken) (\s@DescribeRetentionConfigurationsResponse' {} a -> s {nextToken = a} :: DescribeRetentionConfigurationsResponse)

-- | Returns a retention configuration object.
describeRetentionConfigurationsResponse_retentionConfigurations :: Lens.Lens' DescribeRetentionConfigurationsResponse (Core.Maybe [RetentionConfiguration])
describeRetentionConfigurationsResponse_retentionConfigurations = Lens.lens (\DescribeRetentionConfigurationsResponse' {retentionConfigurations} -> retentionConfigurations) (\s@DescribeRetentionConfigurationsResponse' {} a -> s {retentionConfigurations = a} :: DescribeRetentionConfigurationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeRetentionConfigurationsResponse_httpStatus :: Lens.Lens' DescribeRetentionConfigurationsResponse Core.Int
describeRetentionConfigurationsResponse_httpStatus = Lens.lens (\DescribeRetentionConfigurationsResponse' {httpStatus} -> httpStatus) (\s@DescribeRetentionConfigurationsResponse' {} a -> s {httpStatus = a} :: DescribeRetentionConfigurationsResponse)

instance
  Core.NFData
    DescribeRetentionConfigurationsResponse
