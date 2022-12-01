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
-- Module      : Amazonka.MQ.DescribeBrokerEngineTypes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe available engine types and versions.
module Amazonka.MQ.DescribeBrokerEngineTypes
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MQ.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeBrokerEngineTypes' smart constructor.
data DescribeBrokerEngineTypes = DescribeBrokerEngineTypes'
  { -- | The token that specifies the next page of results Amazon MQ should
    -- return. To request the first page, leave nextToken empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filter response by engine type.
    engineType :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of brokers that Amazon MQ can return per page (20 by
    -- default). This value must be an integer from 5 to 100.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'maxResults', 'describeBrokerEngineTypes_maxResults' - The maximum number of brokers that Amazon MQ can return per page (20 by
-- default). This value must be an integer from 5 to 100.
newDescribeBrokerEngineTypes ::
  DescribeBrokerEngineTypes
newDescribeBrokerEngineTypes =
  DescribeBrokerEngineTypes'
    { nextToken =
        Prelude.Nothing,
      engineType = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
describeBrokerEngineTypes_nextToken :: Lens.Lens' DescribeBrokerEngineTypes (Prelude.Maybe Prelude.Text)
describeBrokerEngineTypes_nextToken = Lens.lens (\DescribeBrokerEngineTypes' {nextToken} -> nextToken) (\s@DescribeBrokerEngineTypes' {} a -> s {nextToken = a} :: DescribeBrokerEngineTypes)

-- | Filter response by engine type.
describeBrokerEngineTypes_engineType :: Lens.Lens' DescribeBrokerEngineTypes (Prelude.Maybe Prelude.Text)
describeBrokerEngineTypes_engineType = Lens.lens (\DescribeBrokerEngineTypes' {engineType} -> engineType) (\s@DescribeBrokerEngineTypes' {} a -> s {engineType = a} :: DescribeBrokerEngineTypes)

-- | The maximum number of brokers that Amazon MQ can return per page (20 by
-- default). This value must be an integer from 5 to 100.
describeBrokerEngineTypes_maxResults :: Lens.Lens' DescribeBrokerEngineTypes (Prelude.Maybe Prelude.Natural)
describeBrokerEngineTypes_maxResults = Lens.lens (\DescribeBrokerEngineTypes' {maxResults} -> maxResults) (\s@DescribeBrokerEngineTypes' {} a -> s {maxResults = a} :: DescribeBrokerEngineTypes)

instance Core.AWSRequest DescribeBrokerEngineTypes where
  type
    AWSResponse DescribeBrokerEngineTypes =
      DescribeBrokerEngineTypesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBrokerEngineTypesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "brokerEngineTypes"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "maxResults")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeBrokerEngineTypes where
  hashWithSalt _salt DescribeBrokerEngineTypes' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` engineType
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeBrokerEngineTypes where
  rnf DescribeBrokerEngineTypes' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf engineType
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders DescribeBrokerEngineTypes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeBrokerEngineTypes where
  toPath = Prelude.const "/v1/broker-engine-types"

instance Core.ToQuery DescribeBrokerEngineTypes where
  toQuery DescribeBrokerEngineTypes' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "engineType" Core.=: engineType,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newDescribeBrokerEngineTypesResponse' smart constructor.
data DescribeBrokerEngineTypesResponse = DescribeBrokerEngineTypesResponse'
  { -- | The token that specifies the next page of results Amazon MQ should
    -- return. To request the first page, leave nextToken empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of available engine types and versions.
    brokerEngineTypes :: Prelude.Maybe [BrokerEngineType],
    -- | Required. The maximum number of engine types that can be returned per
    -- page (20 by default). This value must be an integer from 5 to 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeBrokerEngineTypesResponse
newDescribeBrokerEngineTypesResponse pHttpStatus_ =
  DescribeBrokerEngineTypesResponse'
    { nextToken =
        Prelude.Nothing,
      brokerEngineTypes = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
describeBrokerEngineTypesResponse_nextToken :: Lens.Lens' DescribeBrokerEngineTypesResponse (Prelude.Maybe Prelude.Text)
describeBrokerEngineTypesResponse_nextToken = Lens.lens (\DescribeBrokerEngineTypesResponse' {nextToken} -> nextToken) (\s@DescribeBrokerEngineTypesResponse' {} a -> s {nextToken = a} :: DescribeBrokerEngineTypesResponse)

-- | List of available engine types and versions.
describeBrokerEngineTypesResponse_brokerEngineTypes :: Lens.Lens' DescribeBrokerEngineTypesResponse (Prelude.Maybe [BrokerEngineType])
describeBrokerEngineTypesResponse_brokerEngineTypes = Lens.lens (\DescribeBrokerEngineTypesResponse' {brokerEngineTypes} -> brokerEngineTypes) (\s@DescribeBrokerEngineTypesResponse' {} a -> s {brokerEngineTypes = a} :: DescribeBrokerEngineTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Required. The maximum number of engine types that can be returned per
-- page (20 by default). This value must be an integer from 5 to 100.
describeBrokerEngineTypesResponse_maxResults :: Lens.Lens' DescribeBrokerEngineTypesResponse (Prelude.Maybe Prelude.Natural)
describeBrokerEngineTypesResponse_maxResults = Lens.lens (\DescribeBrokerEngineTypesResponse' {maxResults} -> maxResults) (\s@DescribeBrokerEngineTypesResponse' {} a -> s {maxResults = a} :: DescribeBrokerEngineTypesResponse)

-- | The response's http status code.
describeBrokerEngineTypesResponse_httpStatus :: Lens.Lens' DescribeBrokerEngineTypesResponse Prelude.Int
describeBrokerEngineTypesResponse_httpStatus = Lens.lens (\DescribeBrokerEngineTypesResponse' {httpStatus} -> httpStatus) (\s@DescribeBrokerEngineTypesResponse' {} a -> s {httpStatus = a} :: DescribeBrokerEngineTypesResponse)

instance
  Prelude.NFData
    DescribeBrokerEngineTypesResponse
  where
  rnf DescribeBrokerEngineTypesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf brokerEngineTypes
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf httpStatus
