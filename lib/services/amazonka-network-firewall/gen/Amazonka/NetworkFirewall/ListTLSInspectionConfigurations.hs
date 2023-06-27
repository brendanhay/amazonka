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
-- Module      : Amazonka.NetworkFirewall.ListTLSInspectionConfigurations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the metadata for the TLS inspection configurations that you
-- have defined. Depending on your setting for max results and the number
-- of TLS inspection configurations, a single call might not return the
-- full list.
--
-- This operation returns paginated results.
module Amazonka.NetworkFirewall.ListTLSInspectionConfigurations
  ( -- * Creating a Request
    ListTLSInspectionConfigurations (..),
    newListTLSInspectionConfigurations,

    -- * Request Lenses
    listTLSInspectionConfigurations_maxResults,
    listTLSInspectionConfigurations_nextToken,

    -- * Destructuring the Response
    ListTLSInspectionConfigurationsResponse (..),
    newListTLSInspectionConfigurationsResponse,

    -- * Response Lenses
    listTLSInspectionConfigurationsResponse_nextToken,
    listTLSInspectionConfigurationsResponse_tLSInspectionConfigurations,
    listTLSInspectionConfigurationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTLSInspectionConfigurations' smart constructor.
data ListTLSInspectionConfigurations = ListTLSInspectionConfigurations'
  { -- | The maximum number of objects that you want Network Firewall to return
    -- for this request. If more objects are available, in the response,
    -- Network Firewall provides a @NextToken@ value that you can use in a
    -- subsequent call to get the next batch of objects.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | When you request a list of objects with a @MaxResults@ setting, if the
    -- number of objects that are still available for retrieval exceeds the
    -- maximum you requested, Network Firewall returns a @NextToken@ value in
    -- the response. To retrieve the next batch of objects, use the token
    -- returned from the prior request in your next request.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTLSInspectionConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listTLSInspectionConfigurations_maxResults' - The maximum number of objects that you want Network Firewall to return
-- for this request. If more objects are available, in the response,
-- Network Firewall provides a @NextToken@ value that you can use in a
-- subsequent call to get the next batch of objects.
--
-- 'nextToken', 'listTLSInspectionConfigurations_nextToken' - When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Network Firewall returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
newListTLSInspectionConfigurations ::
  ListTLSInspectionConfigurations
newListTLSInspectionConfigurations =
  ListTLSInspectionConfigurations'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of objects that you want Network Firewall to return
-- for this request. If more objects are available, in the response,
-- Network Firewall provides a @NextToken@ value that you can use in a
-- subsequent call to get the next batch of objects.
listTLSInspectionConfigurations_maxResults :: Lens.Lens' ListTLSInspectionConfigurations (Prelude.Maybe Prelude.Natural)
listTLSInspectionConfigurations_maxResults = Lens.lens (\ListTLSInspectionConfigurations' {maxResults} -> maxResults) (\s@ListTLSInspectionConfigurations' {} a -> s {maxResults = a} :: ListTLSInspectionConfigurations)

-- | When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Network Firewall returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
listTLSInspectionConfigurations_nextToken :: Lens.Lens' ListTLSInspectionConfigurations (Prelude.Maybe Prelude.Text)
listTLSInspectionConfigurations_nextToken = Lens.lens (\ListTLSInspectionConfigurations' {nextToken} -> nextToken) (\s@ListTLSInspectionConfigurations' {} a -> s {nextToken = a} :: ListTLSInspectionConfigurations)

instance
  Core.AWSPager
    ListTLSInspectionConfigurations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTLSInspectionConfigurationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTLSInspectionConfigurationsResponse_tLSInspectionConfigurations
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listTLSInspectionConfigurations_nextToken
          Lens..~ rs
          Lens.^? listTLSInspectionConfigurationsResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListTLSInspectionConfigurations
  where
  type
    AWSResponse ListTLSInspectionConfigurations =
      ListTLSInspectionConfigurationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTLSInspectionConfigurationsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "TLSInspectionConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListTLSInspectionConfigurations
  where
  hashWithSalt
    _salt
    ListTLSInspectionConfigurations' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    ListTLSInspectionConfigurations
  where
  rnf ListTLSInspectionConfigurations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    ListTLSInspectionConfigurations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "NetworkFirewall_20201112.ListTLSInspectionConfigurations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTLSInspectionConfigurations where
  toJSON ListTLSInspectionConfigurations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListTLSInspectionConfigurations where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTLSInspectionConfigurations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTLSInspectionConfigurationsResponse' smart constructor.
data ListTLSInspectionConfigurationsResponse = ListTLSInspectionConfigurationsResponse'
  { -- | When you request a list of objects with a @MaxResults@ setting, if the
    -- number of objects that are still available for retrieval exceeds the
    -- maximum you requested, Network Firewall returns a @NextToken@ value in
    -- the response. To retrieve the next batch of objects, use the token
    -- returned from the prior request in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The TLS inspection configuration metadata objects that you\'ve defined.
    -- Depending on your setting for max results and the number of TLS
    -- inspection configurations, this might not be the full list.
    tLSInspectionConfigurations :: Prelude.Maybe [TLSInspectionConfigurationMetadata],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTLSInspectionConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTLSInspectionConfigurationsResponse_nextToken' - When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Network Firewall returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
--
-- 'tLSInspectionConfigurations', 'listTLSInspectionConfigurationsResponse_tLSInspectionConfigurations' - The TLS inspection configuration metadata objects that you\'ve defined.
-- Depending on your setting for max results and the number of TLS
-- inspection configurations, this might not be the full list.
--
-- 'httpStatus', 'listTLSInspectionConfigurationsResponse_httpStatus' - The response's http status code.
newListTLSInspectionConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTLSInspectionConfigurationsResponse
newListTLSInspectionConfigurationsResponse
  pHttpStatus_ =
    ListTLSInspectionConfigurationsResponse'
      { nextToken =
          Prelude.Nothing,
        tLSInspectionConfigurations =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Network Firewall returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
listTLSInspectionConfigurationsResponse_nextToken :: Lens.Lens' ListTLSInspectionConfigurationsResponse (Prelude.Maybe Prelude.Text)
listTLSInspectionConfigurationsResponse_nextToken = Lens.lens (\ListTLSInspectionConfigurationsResponse' {nextToken} -> nextToken) (\s@ListTLSInspectionConfigurationsResponse' {} a -> s {nextToken = a} :: ListTLSInspectionConfigurationsResponse)

-- | The TLS inspection configuration metadata objects that you\'ve defined.
-- Depending on your setting for max results and the number of TLS
-- inspection configurations, this might not be the full list.
listTLSInspectionConfigurationsResponse_tLSInspectionConfigurations :: Lens.Lens' ListTLSInspectionConfigurationsResponse (Prelude.Maybe [TLSInspectionConfigurationMetadata])
listTLSInspectionConfigurationsResponse_tLSInspectionConfigurations = Lens.lens (\ListTLSInspectionConfigurationsResponse' {tLSInspectionConfigurations} -> tLSInspectionConfigurations) (\s@ListTLSInspectionConfigurationsResponse' {} a -> s {tLSInspectionConfigurations = a} :: ListTLSInspectionConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTLSInspectionConfigurationsResponse_httpStatus :: Lens.Lens' ListTLSInspectionConfigurationsResponse Prelude.Int
listTLSInspectionConfigurationsResponse_httpStatus = Lens.lens (\ListTLSInspectionConfigurationsResponse' {httpStatus} -> httpStatus) (\s@ListTLSInspectionConfigurationsResponse' {} a -> s {httpStatus = a} :: ListTLSInspectionConfigurationsResponse)

instance
  Prelude.NFData
    ListTLSInspectionConfigurationsResponse
  where
  rnf ListTLSInspectionConfigurationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf tLSInspectionConfigurations
      `Prelude.seq` Prelude.rnf httpStatus
