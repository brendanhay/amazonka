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
-- Module      : Network.AWS.AutoScaling.DescribeLaunchConfigurations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more launch configurations.
--
-- This operation returns paginated results.
module Network.AWS.AutoScaling.DescribeLaunchConfigurations
  ( -- * Creating a Request
    DescribeLaunchConfigurations (..),
    newDescribeLaunchConfigurations,

    -- * Request Lenses
    describeLaunchConfigurations_nextToken,
    describeLaunchConfigurations_launchConfigurationNames,
    describeLaunchConfigurations_maxRecords,

    -- * Destructuring the Response
    DescribeLaunchConfigurationsResponse (..),
    newDescribeLaunchConfigurationsResponse,

    -- * Response Lenses
    describeLaunchConfigurationsResponse_nextToken,
    describeLaunchConfigurationsResponse_httpStatus,
    describeLaunchConfigurationsResponse_launchConfigurations,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeLaunchConfigurations' smart constructor.
data DescribeLaunchConfigurations = DescribeLaunchConfigurations'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | The launch configuration names. If you omit this parameter, all launch
    -- configurations are described.
    launchConfigurationNames :: Core.Maybe [Core.Text],
    -- | The maximum number of items to return with this call. The default value
    -- is @50@ and the maximum value is @100@.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeLaunchConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeLaunchConfigurations_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'launchConfigurationNames', 'describeLaunchConfigurations_launchConfigurationNames' - The launch configuration names. If you omit this parameter, all launch
-- configurations are described.
--
-- 'maxRecords', 'describeLaunchConfigurations_maxRecords' - The maximum number of items to return with this call. The default value
-- is @50@ and the maximum value is @100@.
newDescribeLaunchConfigurations ::
  DescribeLaunchConfigurations
newDescribeLaunchConfigurations =
  DescribeLaunchConfigurations'
    { nextToken =
        Core.Nothing,
      launchConfigurationNames = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeLaunchConfigurations_nextToken :: Lens.Lens' DescribeLaunchConfigurations (Core.Maybe Core.Text)
describeLaunchConfigurations_nextToken = Lens.lens (\DescribeLaunchConfigurations' {nextToken} -> nextToken) (\s@DescribeLaunchConfigurations' {} a -> s {nextToken = a} :: DescribeLaunchConfigurations)

-- | The launch configuration names. If you omit this parameter, all launch
-- configurations are described.
describeLaunchConfigurations_launchConfigurationNames :: Lens.Lens' DescribeLaunchConfigurations (Core.Maybe [Core.Text])
describeLaunchConfigurations_launchConfigurationNames = Lens.lens (\DescribeLaunchConfigurations' {launchConfigurationNames} -> launchConfigurationNames) (\s@DescribeLaunchConfigurations' {} a -> s {launchConfigurationNames = a} :: DescribeLaunchConfigurations) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of items to return with this call. The default value
-- is @50@ and the maximum value is @100@.
describeLaunchConfigurations_maxRecords :: Lens.Lens' DescribeLaunchConfigurations (Core.Maybe Core.Int)
describeLaunchConfigurations_maxRecords = Lens.lens (\DescribeLaunchConfigurations' {maxRecords} -> maxRecords) (\s@DescribeLaunchConfigurations' {} a -> s {maxRecords = a} :: DescribeLaunchConfigurations)

instance Core.AWSPager DescribeLaunchConfigurations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeLaunchConfigurationsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^. describeLaunchConfigurationsResponse_launchConfigurations
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeLaunchConfigurations_nextToken
          Lens..~ rs
          Lens.^? describeLaunchConfigurationsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeLaunchConfigurations where
  type
    AWSResponse DescribeLaunchConfigurations =
      DescribeLaunchConfigurationsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeLaunchConfigurationsResult"
      ( \s h x ->
          DescribeLaunchConfigurationsResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..@? "LaunchConfigurations"
                         Core..!@ Core.mempty
                         Core.>>= Core.parseXMLList "member"
                     )
      )

instance Core.Hashable DescribeLaunchConfigurations

instance Core.NFData DescribeLaunchConfigurations

instance Core.ToHeaders DescribeLaunchConfigurations where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeLaunchConfigurations where
  toPath = Core.const "/"

instance Core.ToQuery DescribeLaunchConfigurations where
  toQuery DescribeLaunchConfigurations' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeLaunchConfigurations" :: Core.ByteString),
        "Version" Core.=: ("2011-01-01" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "LaunchConfigurationNames"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> launchConfigurationNames
            ),
        "MaxRecords" Core.=: maxRecords
      ]

-- | /See:/ 'newDescribeLaunchConfigurationsResponse' smart constructor.
data DescribeLaunchConfigurationsResponse = DescribeLaunchConfigurationsResponse'
  { -- | A string that indicates that the response contains more items than can
    -- be returned in a single response. To receive additional items, specify
    -- this string for the @NextToken@ value when requesting the next set of
    -- items. This value is null when there are no more items to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The launch configurations.
    launchConfigurations :: [LaunchConfiguration]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeLaunchConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeLaunchConfigurationsResponse_nextToken' - A string that indicates that the response contains more items than can
-- be returned in a single response. To receive additional items, specify
-- this string for the @NextToken@ value when requesting the next set of
-- items. This value is null when there are no more items to return.
--
-- 'httpStatus', 'describeLaunchConfigurationsResponse_httpStatus' - The response's http status code.
--
-- 'launchConfigurations', 'describeLaunchConfigurationsResponse_launchConfigurations' - The launch configurations.
newDescribeLaunchConfigurationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeLaunchConfigurationsResponse
newDescribeLaunchConfigurationsResponse pHttpStatus_ =
  DescribeLaunchConfigurationsResponse'
    { nextToken =
        Core.Nothing,
      httpStatus = pHttpStatus_,
      launchConfigurations = Core.mempty
    }

-- | A string that indicates that the response contains more items than can
-- be returned in a single response. To receive additional items, specify
-- this string for the @NextToken@ value when requesting the next set of
-- items. This value is null when there are no more items to return.
describeLaunchConfigurationsResponse_nextToken :: Lens.Lens' DescribeLaunchConfigurationsResponse (Core.Maybe Core.Text)
describeLaunchConfigurationsResponse_nextToken = Lens.lens (\DescribeLaunchConfigurationsResponse' {nextToken} -> nextToken) (\s@DescribeLaunchConfigurationsResponse' {} a -> s {nextToken = a} :: DescribeLaunchConfigurationsResponse)

-- | The response's http status code.
describeLaunchConfigurationsResponse_httpStatus :: Lens.Lens' DescribeLaunchConfigurationsResponse Core.Int
describeLaunchConfigurationsResponse_httpStatus = Lens.lens (\DescribeLaunchConfigurationsResponse' {httpStatus} -> httpStatus) (\s@DescribeLaunchConfigurationsResponse' {} a -> s {httpStatus = a} :: DescribeLaunchConfigurationsResponse)

-- | The launch configurations.
describeLaunchConfigurationsResponse_launchConfigurations :: Lens.Lens' DescribeLaunchConfigurationsResponse [LaunchConfiguration]
describeLaunchConfigurationsResponse_launchConfigurations = Lens.lens (\DescribeLaunchConfigurationsResponse' {launchConfigurations} -> launchConfigurations) (\s@DescribeLaunchConfigurationsResponse' {} a -> s {launchConfigurations = a} :: DescribeLaunchConfigurationsResponse) Core.. Lens._Coerce

instance
  Core.NFData
    DescribeLaunchConfigurationsResponse
