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
-- Module      : Amazonka.AutoScaling.DescribeLaunchConfigurations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the launch configurations in the account and
-- Region.
--
-- This operation returns paginated results.
module Amazonka.AutoScaling.DescribeLaunchConfigurations
  ( -- * Creating a Request
    DescribeLaunchConfigurations (..),
    newDescribeLaunchConfigurations,

    -- * Request Lenses
    describeLaunchConfigurations_launchConfigurationNames,
    describeLaunchConfigurations_maxRecords,
    describeLaunchConfigurations_nextToken,

    -- * Destructuring the Response
    DescribeLaunchConfigurationsResponse (..),
    newDescribeLaunchConfigurationsResponse,

    -- * Response Lenses
    describeLaunchConfigurationsResponse_nextToken,
    describeLaunchConfigurationsResponse_httpStatus,
    describeLaunchConfigurationsResponse_launchConfigurations,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeLaunchConfigurations' smart constructor.
data DescribeLaunchConfigurations = DescribeLaunchConfigurations'
  { -- | The launch configuration names. If you omit this property, all launch
    -- configurations are described.
    --
    -- Array Members: Maximum number of 50 items.
    launchConfigurationNames :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of items to return with this call. The default value
    -- is @50@ and the maximum value is @100@.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLaunchConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchConfigurationNames', 'describeLaunchConfigurations_launchConfigurationNames' - The launch configuration names. If you omit this property, all launch
-- configurations are described.
--
-- Array Members: Maximum number of 50 items.
--
-- 'maxRecords', 'describeLaunchConfigurations_maxRecords' - The maximum number of items to return with this call. The default value
-- is @50@ and the maximum value is @100@.
--
-- 'nextToken', 'describeLaunchConfigurations_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
newDescribeLaunchConfigurations ::
  DescribeLaunchConfigurations
newDescribeLaunchConfigurations =
  DescribeLaunchConfigurations'
    { launchConfigurationNames =
        Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The launch configuration names. If you omit this property, all launch
-- configurations are described.
--
-- Array Members: Maximum number of 50 items.
describeLaunchConfigurations_launchConfigurationNames :: Lens.Lens' DescribeLaunchConfigurations (Prelude.Maybe [Prelude.Text])
describeLaunchConfigurations_launchConfigurationNames = Lens.lens (\DescribeLaunchConfigurations' {launchConfigurationNames} -> launchConfigurationNames) (\s@DescribeLaunchConfigurations' {} a -> s {launchConfigurationNames = a} :: DescribeLaunchConfigurations) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return with this call. The default value
-- is @50@ and the maximum value is @100@.
describeLaunchConfigurations_maxRecords :: Lens.Lens' DescribeLaunchConfigurations (Prelude.Maybe Prelude.Int)
describeLaunchConfigurations_maxRecords = Lens.lens (\DescribeLaunchConfigurations' {maxRecords} -> maxRecords) (\s@DescribeLaunchConfigurations' {} a -> s {maxRecords = a} :: DescribeLaunchConfigurations)

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeLaunchConfigurations_nextToken :: Lens.Lens' DescribeLaunchConfigurations (Prelude.Maybe Prelude.Text)
describeLaunchConfigurations_nextToken = Lens.lens (\DescribeLaunchConfigurations' {nextToken} -> nextToken) (\s@DescribeLaunchConfigurations' {} a -> s {nextToken = a} :: DescribeLaunchConfigurations)

instance Core.AWSPager DescribeLaunchConfigurations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeLaunchConfigurationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. describeLaunchConfigurationsResponse_launchConfigurations
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeLaunchConfigurations_nextToken
              Lens..~ rs
              Lens.^? describeLaunchConfigurationsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest DescribeLaunchConfigurations where
  type
    AWSResponse DescribeLaunchConfigurations =
      DescribeLaunchConfigurationsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeLaunchConfigurationsResult"
      ( \s h x ->
          DescribeLaunchConfigurationsResponse'
            Prelude.<$> (x Data..@? "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..@? "LaunchConfigurations"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Data.parseXMLList "member"
                        )
      )

instance
  Prelude.Hashable
    DescribeLaunchConfigurations
  where
  hashWithSalt _salt DescribeLaunchConfigurations' {..} =
    _salt
      `Prelude.hashWithSalt` launchConfigurationNames
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeLaunchConfigurations where
  rnf DescribeLaunchConfigurations' {..} =
    Prelude.rnf launchConfigurationNames `Prelude.seq`
      Prelude.rnf maxRecords `Prelude.seq`
        Prelude.rnf nextToken

instance Data.ToHeaders DescribeLaunchConfigurations where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeLaunchConfigurations where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeLaunchConfigurations where
  toQuery DescribeLaunchConfigurations' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeLaunchConfigurations" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2011-01-01" :: Prelude.ByteString),
        "LaunchConfigurationNames"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> launchConfigurationNames
            ),
        "MaxRecords" Data.=: maxRecords,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeLaunchConfigurationsResponse' smart constructor.
data DescribeLaunchConfigurationsResponse = DescribeLaunchConfigurationsResponse'
  { -- | A string that indicates that the response contains more items than can
    -- be returned in a single response. To receive additional items, specify
    -- this string for the @NextToken@ value when requesting the next set of
    -- items. This value is null when there are no more items to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The launch configurations.
    launchConfigurations :: [LaunchConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeLaunchConfigurationsResponse
newDescribeLaunchConfigurationsResponse pHttpStatus_ =
  DescribeLaunchConfigurationsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      launchConfigurations = Prelude.mempty
    }

-- | A string that indicates that the response contains more items than can
-- be returned in a single response. To receive additional items, specify
-- this string for the @NextToken@ value when requesting the next set of
-- items. This value is null when there are no more items to return.
describeLaunchConfigurationsResponse_nextToken :: Lens.Lens' DescribeLaunchConfigurationsResponse (Prelude.Maybe Prelude.Text)
describeLaunchConfigurationsResponse_nextToken = Lens.lens (\DescribeLaunchConfigurationsResponse' {nextToken} -> nextToken) (\s@DescribeLaunchConfigurationsResponse' {} a -> s {nextToken = a} :: DescribeLaunchConfigurationsResponse)

-- | The response's http status code.
describeLaunchConfigurationsResponse_httpStatus :: Lens.Lens' DescribeLaunchConfigurationsResponse Prelude.Int
describeLaunchConfigurationsResponse_httpStatus = Lens.lens (\DescribeLaunchConfigurationsResponse' {httpStatus} -> httpStatus) (\s@DescribeLaunchConfigurationsResponse' {} a -> s {httpStatus = a} :: DescribeLaunchConfigurationsResponse)

-- | The launch configurations.
describeLaunchConfigurationsResponse_launchConfigurations :: Lens.Lens' DescribeLaunchConfigurationsResponse [LaunchConfiguration]
describeLaunchConfigurationsResponse_launchConfigurations = Lens.lens (\DescribeLaunchConfigurationsResponse' {launchConfigurations} -> launchConfigurations) (\s@DescribeLaunchConfigurationsResponse' {} a -> s {launchConfigurations = a} :: DescribeLaunchConfigurationsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    DescribeLaunchConfigurationsResponse
  where
  rnf DescribeLaunchConfigurationsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf launchConfigurations
