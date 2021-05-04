{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeBrokerInstanceOptions' smart constructor.
data DescribeBrokerInstanceOptions = DescribeBrokerInstanceOptions'
  { -- | The token that specifies the next page of results Amazon MQ should
    -- return. To request the first page, leave nextToken empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filter response by storage type.
    storageType :: Prelude.Maybe Prelude.Text,
    -- | Filter response by engine type.
    engineType :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of instance options that Amazon MQ can return per
    -- page (20 by default). This value must be an integer from 5 to 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Filter response by host instance type.
    hostInstanceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      storageType = Prelude.Nothing,
      engineType = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      hostInstanceType = Prelude.Nothing
    }

-- | The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
describeBrokerInstanceOptions_nextToken :: Lens.Lens' DescribeBrokerInstanceOptions (Prelude.Maybe Prelude.Text)
describeBrokerInstanceOptions_nextToken = Lens.lens (\DescribeBrokerInstanceOptions' {nextToken} -> nextToken) (\s@DescribeBrokerInstanceOptions' {} a -> s {nextToken = a} :: DescribeBrokerInstanceOptions)

-- | Filter response by storage type.
describeBrokerInstanceOptions_storageType :: Lens.Lens' DescribeBrokerInstanceOptions (Prelude.Maybe Prelude.Text)
describeBrokerInstanceOptions_storageType = Lens.lens (\DescribeBrokerInstanceOptions' {storageType} -> storageType) (\s@DescribeBrokerInstanceOptions' {} a -> s {storageType = a} :: DescribeBrokerInstanceOptions)

-- | Filter response by engine type.
describeBrokerInstanceOptions_engineType :: Lens.Lens' DescribeBrokerInstanceOptions (Prelude.Maybe Prelude.Text)
describeBrokerInstanceOptions_engineType = Lens.lens (\DescribeBrokerInstanceOptions' {engineType} -> engineType) (\s@DescribeBrokerInstanceOptions' {} a -> s {engineType = a} :: DescribeBrokerInstanceOptions)

-- | The maximum number of instance options that Amazon MQ can return per
-- page (20 by default). This value must be an integer from 5 to 100.
describeBrokerInstanceOptions_maxResults :: Lens.Lens' DescribeBrokerInstanceOptions (Prelude.Maybe Prelude.Natural)
describeBrokerInstanceOptions_maxResults = Lens.lens (\DescribeBrokerInstanceOptions' {maxResults} -> maxResults) (\s@DescribeBrokerInstanceOptions' {} a -> s {maxResults = a} :: DescribeBrokerInstanceOptions)

-- | Filter response by host instance type.
describeBrokerInstanceOptions_hostInstanceType :: Lens.Lens' DescribeBrokerInstanceOptions (Prelude.Maybe Prelude.Text)
describeBrokerInstanceOptions_hostInstanceType = Lens.lens (\DescribeBrokerInstanceOptions' {hostInstanceType} -> hostInstanceType) (\s@DescribeBrokerInstanceOptions' {} a -> s {hostInstanceType = a} :: DescribeBrokerInstanceOptions)

instance
  Prelude.AWSRequest
    DescribeBrokerInstanceOptions
  where
  type
    Rs DescribeBrokerInstanceOptions =
      DescribeBrokerInstanceOptionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBrokerInstanceOptionsResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> (x Prelude..?> "maxResults")
            Prelude.<*> ( x Prelude..?> "brokerInstanceOptions"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeBrokerInstanceOptions

instance Prelude.NFData DescribeBrokerInstanceOptions

instance
  Prelude.ToHeaders
    DescribeBrokerInstanceOptions
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DescribeBrokerInstanceOptions where
  toPath = Prelude.const "/v1/broker-instance-options"

instance
  Prelude.ToQuery
    DescribeBrokerInstanceOptions
  where
  toQuery DescribeBrokerInstanceOptions' {..} =
    Prelude.mconcat
      [ "nextToken" Prelude.=: nextToken,
        "storageType" Prelude.=: storageType,
        "engineType" Prelude.=: engineType,
        "maxResults" Prelude.=: maxResults,
        "hostInstanceType" Prelude.=: hostInstanceType
      ]

-- | /See:/ 'newDescribeBrokerInstanceOptionsResponse' smart constructor.
data DescribeBrokerInstanceOptionsResponse = DescribeBrokerInstanceOptionsResponse'
  { -- | The token that specifies the next page of results Amazon MQ should
    -- return. To request the first page, leave nextToken empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Required. The maximum number of instance options that can be returned
    -- per page (20 by default). This value must be an integer from 5 to 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | List of available broker instance options.
    brokerInstanceOptions :: Prelude.Maybe [BrokerInstanceOption],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeBrokerInstanceOptionsResponse
newDescribeBrokerInstanceOptionsResponse pHttpStatus_ =
  DescribeBrokerInstanceOptionsResponse'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      brokerInstanceOptions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
describeBrokerInstanceOptionsResponse_nextToken :: Lens.Lens' DescribeBrokerInstanceOptionsResponse (Prelude.Maybe Prelude.Text)
describeBrokerInstanceOptionsResponse_nextToken = Lens.lens (\DescribeBrokerInstanceOptionsResponse' {nextToken} -> nextToken) (\s@DescribeBrokerInstanceOptionsResponse' {} a -> s {nextToken = a} :: DescribeBrokerInstanceOptionsResponse)

-- | Required. The maximum number of instance options that can be returned
-- per page (20 by default). This value must be an integer from 5 to 100.
describeBrokerInstanceOptionsResponse_maxResults :: Lens.Lens' DescribeBrokerInstanceOptionsResponse (Prelude.Maybe Prelude.Natural)
describeBrokerInstanceOptionsResponse_maxResults = Lens.lens (\DescribeBrokerInstanceOptionsResponse' {maxResults} -> maxResults) (\s@DescribeBrokerInstanceOptionsResponse' {} a -> s {maxResults = a} :: DescribeBrokerInstanceOptionsResponse)

-- | List of available broker instance options.
describeBrokerInstanceOptionsResponse_brokerInstanceOptions :: Lens.Lens' DescribeBrokerInstanceOptionsResponse (Prelude.Maybe [BrokerInstanceOption])
describeBrokerInstanceOptionsResponse_brokerInstanceOptions = Lens.lens (\DescribeBrokerInstanceOptionsResponse' {brokerInstanceOptions} -> brokerInstanceOptions) (\s@DescribeBrokerInstanceOptionsResponse' {} a -> s {brokerInstanceOptions = a} :: DescribeBrokerInstanceOptionsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeBrokerInstanceOptionsResponse_httpStatus :: Lens.Lens' DescribeBrokerInstanceOptionsResponse Prelude.Int
describeBrokerInstanceOptionsResponse_httpStatus = Lens.lens (\DescribeBrokerInstanceOptionsResponse' {httpStatus} -> httpStatus) (\s@DescribeBrokerInstanceOptionsResponse' {} a -> s {httpStatus = a} :: DescribeBrokerInstanceOptionsResponse)

instance
  Prelude.NFData
    DescribeBrokerInstanceOptionsResponse
