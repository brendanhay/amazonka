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
-- Module      : Network.AWS.MQ.DescribeBrokerEngineTypes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe available engine types and versions.
module Network.AWS.MQ.DescribeBrokerEngineTypes
  ( -- * Creating a Request
    DescribeBrokerEngineTypes (..),
    newDescribeBrokerEngineTypes,

    -- * Request Lenses
    describeBrokerEngineTypes_nextToken,
    describeBrokerEngineTypes_engineType,
    describeBrokerEngineTypes_maxResults,

    -- * Destructuring the Response
    DescribeBrokerEngineTypesResponse (..),
    newDescribeBrokerEngineTypesResponse,

    -- * Response Lenses
    describeBrokerEngineTypesResponse_nextToken,
    describeBrokerEngineTypesResponse_brokerEngineTypes,
    describeBrokerEngineTypesResponse_maxResults,
    describeBrokerEngineTypesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeBrokerEngineTypes' smart constructor.
data DescribeBrokerEngineTypes = DescribeBrokerEngineTypes'
  { -- | The token that specifies the next page of results Amazon MQ should
    -- return. To request the first page, leave nextToken empty.
    nextToken :: Core.Maybe Core.Text,
    -- | Filter response by engine type.
    engineType :: Core.Maybe Core.Text,
    -- | The maximum number of engine types that Amazon MQ can return per page
    -- (20 by default). This value must be an integer from 5 to 100.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeBrokerEngineTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeBrokerEngineTypes_nextToken' - The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
--
-- 'engineType', 'describeBrokerEngineTypes_engineType' - Filter response by engine type.
--
-- 'maxResults', 'describeBrokerEngineTypes_maxResults' - The maximum number of engine types that Amazon MQ can return per page
-- (20 by default). This value must be an integer from 5 to 100.
newDescribeBrokerEngineTypes ::
  DescribeBrokerEngineTypes
newDescribeBrokerEngineTypes =
  DescribeBrokerEngineTypes'
    { nextToken =
        Core.Nothing,
      engineType = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
describeBrokerEngineTypes_nextToken :: Lens.Lens' DescribeBrokerEngineTypes (Core.Maybe Core.Text)
describeBrokerEngineTypes_nextToken = Lens.lens (\DescribeBrokerEngineTypes' {nextToken} -> nextToken) (\s@DescribeBrokerEngineTypes' {} a -> s {nextToken = a} :: DescribeBrokerEngineTypes)

-- | Filter response by engine type.
describeBrokerEngineTypes_engineType :: Lens.Lens' DescribeBrokerEngineTypes (Core.Maybe Core.Text)
describeBrokerEngineTypes_engineType = Lens.lens (\DescribeBrokerEngineTypes' {engineType} -> engineType) (\s@DescribeBrokerEngineTypes' {} a -> s {engineType = a} :: DescribeBrokerEngineTypes)

-- | The maximum number of engine types that Amazon MQ can return per page
-- (20 by default). This value must be an integer from 5 to 100.
describeBrokerEngineTypes_maxResults :: Lens.Lens' DescribeBrokerEngineTypes (Core.Maybe Core.Natural)
describeBrokerEngineTypes_maxResults = Lens.lens (\DescribeBrokerEngineTypes' {maxResults} -> maxResults) (\s@DescribeBrokerEngineTypes' {} a -> s {maxResults = a} :: DescribeBrokerEngineTypes)

instance Core.AWSRequest DescribeBrokerEngineTypes where
  type
    AWSResponse DescribeBrokerEngineTypes =
      DescribeBrokerEngineTypesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBrokerEngineTypesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "brokerEngineTypes" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "maxResults")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeBrokerEngineTypes

instance Core.NFData DescribeBrokerEngineTypes

instance Core.ToHeaders DescribeBrokerEngineTypes where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DescribeBrokerEngineTypes where
  toPath = Core.const "/v1/broker-engine-types"

instance Core.ToQuery DescribeBrokerEngineTypes where
  toQuery DescribeBrokerEngineTypes' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "engineType" Core.=: engineType,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newDescribeBrokerEngineTypesResponse' smart constructor.
data DescribeBrokerEngineTypesResponse = DescribeBrokerEngineTypesResponse'
  { -- | The token that specifies the next page of results Amazon MQ should
    -- return. To request the first page, leave nextToken empty.
    nextToken :: Core.Maybe Core.Text,
    -- | List of available engine types and versions.
    brokerEngineTypes :: Core.Maybe [BrokerEngineType],
    -- | Required. The maximum number of engine types that can be returned per
    -- page (20 by default). This value must be an integer from 5 to 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeBrokerEngineTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeBrokerEngineTypesResponse_nextToken' - The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
--
-- 'brokerEngineTypes', 'describeBrokerEngineTypesResponse_brokerEngineTypes' - List of available engine types and versions.
--
-- 'maxResults', 'describeBrokerEngineTypesResponse_maxResults' - Required. The maximum number of engine types that can be returned per
-- page (20 by default). This value must be an integer from 5 to 100.
--
-- 'httpStatus', 'describeBrokerEngineTypesResponse_httpStatus' - The response's http status code.
newDescribeBrokerEngineTypesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeBrokerEngineTypesResponse
newDescribeBrokerEngineTypesResponse pHttpStatus_ =
  DescribeBrokerEngineTypesResponse'
    { nextToken =
        Core.Nothing,
      brokerEngineTypes = Core.Nothing,
      maxResults = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
describeBrokerEngineTypesResponse_nextToken :: Lens.Lens' DescribeBrokerEngineTypesResponse (Core.Maybe Core.Text)
describeBrokerEngineTypesResponse_nextToken = Lens.lens (\DescribeBrokerEngineTypesResponse' {nextToken} -> nextToken) (\s@DescribeBrokerEngineTypesResponse' {} a -> s {nextToken = a} :: DescribeBrokerEngineTypesResponse)

-- | List of available engine types and versions.
describeBrokerEngineTypesResponse_brokerEngineTypes :: Lens.Lens' DescribeBrokerEngineTypesResponse (Core.Maybe [BrokerEngineType])
describeBrokerEngineTypesResponse_brokerEngineTypes = Lens.lens (\DescribeBrokerEngineTypesResponse' {brokerEngineTypes} -> brokerEngineTypes) (\s@DescribeBrokerEngineTypesResponse' {} a -> s {brokerEngineTypes = a} :: DescribeBrokerEngineTypesResponse) Core.. Lens.mapping Lens._Coerce

-- | Required. The maximum number of engine types that can be returned per
-- page (20 by default). This value must be an integer from 5 to 100.
describeBrokerEngineTypesResponse_maxResults :: Lens.Lens' DescribeBrokerEngineTypesResponse (Core.Maybe Core.Natural)
describeBrokerEngineTypesResponse_maxResults = Lens.lens (\DescribeBrokerEngineTypesResponse' {maxResults} -> maxResults) (\s@DescribeBrokerEngineTypesResponse' {} a -> s {maxResults = a} :: DescribeBrokerEngineTypesResponse)

-- | The response's http status code.
describeBrokerEngineTypesResponse_httpStatus :: Lens.Lens' DescribeBrokerEngineTypesResponse Core.Int
describeBrokerEngineTypesResponse_httpStatus = Lens.lens (\DescribeBrokerEngineTypesResponse' {httpStatus} -> httpStatus) (\s@DescribeBrokerEngineTypesResponse' {} a -> s {httpStatus = a} :: DescribeBrokerEngineTypesResponse)

instance
  Core.NFData
    DescribeBrokerEngineTypesResponse
