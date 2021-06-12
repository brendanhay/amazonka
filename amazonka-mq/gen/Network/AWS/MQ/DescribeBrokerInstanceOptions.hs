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
-- Module      : Network.AWS.MQ.DescribeBrokerInstanceOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe available broker instance options.
module Network.AWS.MQ.DescribeBrokerInstanceOptions
  ( -- * Creating a Request
    DescribeBrokerInstanceOptions (..),
    newDescribeBrokerInstanceOptions,

    -- * Request Lenses
    describeBrokerInstanceOptions_nextToken,
    describeBrokerInstanceOptions_storageType,
    describeBrokerInstanceOptions_engineType,
    describeBrokerInstanceOptions_maxResults,
    describeBrokerInstanceOptions_hostInstanceType,

    -- * Destructuring the Response
    DescribeBrokerInstanceOptionsResponse (..),
    newDescribeBrokerInstanceOptionsResponse,

    -- * Response Lenses
    describeBrokerInstanceOptionsResponse_nextToken,
    describeBrokerInstanceOptionsResponse_maxResults,
    describeBrokerInstanceOptionsResponse_brokerInstanceOptions,
    describeBrokerInstanceOptionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeBrokerInstanceOptions' smart constructor.
data DescribeBrokerInstanceOptions = DescribeBrokerInstanceOptions'
  { -- | The token that specifies the next page of results Amazon MQ should
    -- return. To request the first page, leave nextToken empty.
    nextToken :: Core.Maybe Core.Text,
    -- | Filter response by storage type.
    storageType :: Core.Maybe Core.Text,
    -- | Filter response by engine type.
    engineType :: Core.Maybe Core.Text,
    -- | The maximum number of instance options that Amazon MQ can return per
    -- page (20 by default). This value must be an integer from 5 to 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | Filter response by host instance type.
    hostInstanceType :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeBrokerInstanceOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeBrokerInstanceOptions_nextToken' - The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
--
-- 'storageType', 'describeBrokerInstanceOptions_storageType' - Filter response by storage type.
--
-- 'engineType', 'describeBrokerInstanceOptions_engineType' - Filter response by engine type.
--
-- 'maxResults', 'describeBrokerInstanceOptions_maxResults' - The maximum number of instance options that Amazon MQ can return per
-- page (20 by default). This value must be an integer from 5 to 100.
--
-- 'hostInstanceType', 'describeBrokerInstanceOptions_hostInstanceType' - Filter response by host instance type.
newDescribeBrokerInstanceOptions ::
  DescribeBrokerInstanceOptions
newDescribeBrokerInstanceOptions =
  DescribeBrokerInstanceOptions'
    { nextToken =
        Core.Nothing,
      storageType = Core.Nothing,
      engineType = Core.Nothing,
      maxResults = Core.Nothing,
      hostInstanceType = Core.Nothing
    }

-- | The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
describeBrokerInstanceOptions_nextToken :: Lens.Lens' DescribeBrokerInstanceOptions (Core.Maybe Core.Text)
describeBrokerInstanceOptions_nextToken = Lens.lens (\DescribeBrokerInstanceOptions' {nextToken} -> nextToken) (\s@DescribeBrokerInstanceOptions' {} a -> s {nextToken = a} :: DescribeBrokerInstanceOptions)

-- | Filter response by storage type.
describeBrokerInstanceOptions_storageType :: Lens.Lens' DescribeBrokerInstanceOptions (Core.Maybe Core.Text)
describeBrokerInstanceOptions_storageType = Lens.lens (\DescribeBrokerInstanceOptions' {storageType} -> storageType) (\s@DescribeBrokerInstanceOptions' {} a -> s {storageType = a} :: DescribeBrokerInstanceOptions)

-- | Filter response by engine type.
describeBrokerInstanceOptions_engineType :: Lens.Lens' DescribeBrokerInstanceOptions (Core.Maybe Core.Text)
describeBrokerInstanceOptions_engineType = Lens.lens (\DescribeBrokerInstanceOptions' {engineType} -> engineType) (\s@DescribeBrokerInstanceOptions' {} a -> s {engineType = a} :: DescribeBrokerInstanceOptions)

-- | The maximum number of instance options that Amazon MQ can return per
-- page (20 by default). This value must be an integer from 5 to 100.
describeBrokerInstanceOptions_maxResults :: Lens.Lens' DescribeBrokerInstanceOptions (Core.Maybe Core.Natural)
describeBrokerInstanceOptions_maxResults = Lens.lens (\DescribeBrokerInstanceOptions' {maxResults} -> maxResults) (\s@DescribeBrokerInstanceOptions' {} a -> s {maxResults = a} :: DescribeBrokerInstanceOptions)

-- | Filter response by host instance type.
describeBrokerInstanceOptions_hostInstanceType :: Lens.Lens' DescribeBrokerInstanceOptions (Core.Maybe Core.Text)
describeBrokerInstanceOptions_hostInstanceType = Lens.lens (\DescribeBrokerInstanceOptions' {hostInstanceType} -> hostInstanceType) (\s@DescribeBrokerInstanceOptions' {} a -> s {hostInstanceType = a} :: DescribeBrokerInstanceOptions)

instance
  Core.AWSRequest
    DescribeBrokerInstanceOptions
  where
  type
    AWSResponse DescribeBrokerInstanceOptions =
      DescribeBrokerInstanceOptionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBrokerInstanceOptionsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "maxResults")
            Core.<*> ( x Core..?> "brokerInstanceOptions"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeBrokerInstanceOptions

instance Core.NFData DescribeBrokerInstanceOptions

instance Core.ToHeaders DescribeBrokerInstanceOptions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DescribeBrokerInstanceOptions where
  toPath = Core.const "/v1/broker-instance-options"

instance Core.ToQuery DescribeBrokerInstanceOptions where
  toQuery DescribeBrokerInstanceOptions' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "storageType" Core.=: storageType,
        "engineType" Core.=: engineType,
        "maxResults" Core.=: maxResults,
        "hostInstanceType" Core.=: hostInstanceType
      ]

-- | /See:/ 'newDescribeBrokerInstanceOptionsResponse' smart constructor.
data DescribeBrokerInstanceOptionsResponse = DescribeBrokerInstanceOptionsResponse'
  { -- | The token that specifies the next page of results Amazon MQ should
    -- return. To request the first page, leave nextToken empty.
    nextToken :: Core.Maybe Core.Text,
    -- | Required. The maximum number of instance options that can be returned
    -- per page (20 by default). This value must be an integer from 5 to 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | List of available broker instance options.
    brokerInstanceOptions :: Core.Maybe [BrokerInstanceOption],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeBrokerInstanceOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeBrokerInstanceOptionsResponse_nextToken' - The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
--
-- 'maxResults', 'describeBrokerInstanceOptionsResponse_maxResults' - Required. The maximum number of instance options that can be returned
-- per page (20 by default). This value must be an integer from 5 to 100.
--
-- 'brokerInstanceOptions', 'describeBrokerInstanceOptionsResponse_brokerInstanceOptions' - List of available broker instance options.
--
-- 'httpStatus', 'describeBrokerInstanceOptionsResponse_httpStatus' - The response's http status code.
newDescribeBrokerInstanceOptionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeBrokerInstanceOptionsResponse
newDescribeBrokerInstanceOptionsResponse pHttpStatus_ =
  DescribeBrokerInstanceOptionsResponse'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      brokerInstanceOptions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
describeBrokerInstanceOptionsResponse_nextToken :: Lens.Lens' DescribeBrokerInstanceOptionsResponse (Core.Maybe Core.Text)
describeBrokerInstanceOptionsResponse_nextToken = Lens.lens (\DescribeBrokerInstanceOptionsResponse' {nextToken} -> nextToken) (\s@DescribeBrokerInstanceOptionsResponse' {} a -> s {nextToken = a} :: DescribeBrokerInstanceOptionsResponse)

-- | Required. The maximum number of instance options that can be returned
-- per page (20 by default). This value must be an integer from 5 to 100.
describeBrokerInstanceOptionsResponse_maxResults :: Lens.Lens' DescribeBrokerInstanceOptionsResponse (Core.Maybe Core.Natural)
describeBrokerInstanceOptionsResponse_maxResults = Lens.lens (\DescribeBrokerInstanceOptionsResponse' {maxResults} -> maxResults) (\s@DescribeBrokerInstanceOptionsResponse' {} a -> s {maxResults = a} :: DescribeBrokerInstanceOptionsResponse)

-- | List of available broker instance options.
describeBrokerInstanceOptionsResponse_brokerInstanceOptions :: Lens.Lens' DescribeBrokerInstanceOptionsResponse (Core.Maybe [BrokerInstanceOption])
describeBrokerInstanceOptionsResponse_brokerInstanceOptions = Lens.lens (\DescribeBrokerInstanceOptionsResponse' {brokerInstanceOptions} -> brokerInstanceOptions) (\s@DescribeBrokerInstanceOptionsResponse' {} a -> s {brokerInstanceOptions = a} :: DescribeBrokerInstanceOptionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeBrokerInstanceOptionsResponse_httpStatus :: Lens.Lens' DescribeBrokerInstanceOptionsResponse Core.Int
describeBrokerInstanceOptionsResponse_httpStatus = Lens.lens (\DescribeBrokerInstanceOptionsResponse' {httpStatus} -> httpStatus) (\s@DescribeBrokerInstanceOptionsResponse' {} a -> s {httpStatus = a} :: DescribeBrokerInstanceOptionsResponse)

instance
  Core.NFData
    DescribeBrokerInstanceOptionsResponse
