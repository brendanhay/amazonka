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
-- Module      : Amazonka.PinpointSmsVoiceV2.DescribeConfigurationSets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified configuration sets or all in your account.
--
-- If you specify configuration set names, the output includes information
-- for only the specified configuration sets. If you specify filters, the
-- output includes information for only those configuration sets that meet
-- the filter criteria. If you don\'t specify configuration set names or
-- filters, the output includes information for all configuration sets.
--
-- If you specify a configuration set name that isn\'t valid, an error is
-- returned.
--
-- This operation returns paginated results.
module Amazonka.PinpointSmsVoiceV2.DescribeConfigurationSets
  ( -- * Creating a Request
    DescribeConfigurationSets (..),
    newDescribeConfigurationSets,

    -- * Request Lenses
    describeConfigurationSets_configurationSetNames,
    describeConfigurationSets_filters,
    describeConfigurationSets_maxResults,
    describeConfigurationSets_nextToken,

    -- * Destructuring the Response
    DescribeConfigurationSetsResponse (..),
    newDescribeConfigurationSetsResponse,

    -- * Response Lenses
    describeConfigurationSetsResponse_configurationSets,
    describeConfigurationSetsResponse_nextToken,
    describeConfigurationSetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeConfigurationSets' smart constructor.
data DescribeConfigurationSets = DescribeConfigurationSets'
  { -- | An array of strings. Each element can be either a ConfigurationSetName
    -- or ConfigurationSetArn.
    configurationSetNames :: Prelude.Maybe [Prelude.Text],
    -- | An array of filters to apply to the results that are returned.
    filters :: Prelude.Maybe [ConfigurationSetFilter],
    -- | The maximum number of results to return per each request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to be used for the next set of paginated results. You don\'t
    -- need to supply a value for this field in the initial request.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConfigurationSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSetNames', 'describeConfigurationSets_configurationSetNames' - An array of strings. Each element can be either a ConfigurationSetName
-- or ConfigurationSetArn.
--
-- 'filters', 'describeConfigurationSets_filters' - An array of filters to apply to the results that are returned.
--
-- 'maxResults', 'describeConfigurationSets_maxResults' - The maximum number of results to return per each request.
--
-- 'nextToken', 'describeConfigurationSets_nextToken' - The token to be used for the next set of paginated results. You don\'t
-- need to supply a value for this field in the initial request.
newDescribeConfigurationSets ::
  DescribeConfigurationSets
newDescribeConfigurationSets =
  DescribeConfigurationSets'
    { configurationSetNames =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | An array of strings. Each element can be either a ConfigurationSetName
-- or ConfigurationSetArn.
describeConfigurationSets_configurationSetNames :: Lens.Lens' DescribeConfigurationSets (Prelude.Maybe [Prelude.Text])
describeConfigurationSets_configurationSetNames = Lens.lens (\DescribeConfigurationSets' {configurationSetNames} -> configurationSetNames) (\s@DescribeConfigurationSets' {} a -> s {configurationSetNames = a} :: DescribeConfigurationSets) Prelude.. Lens.mapping Lens.coerced

-- | An array of filters to apply to the results that are returned.
describeConfigurationSets_filters :: Lens.Lens' DescribeConfigurationSets (Prelude.Maybe [ConfigurationSetFilter])
describeConfigurationSets_filters = Lens.lens (\DescribeConfigurationSets' {filters} -> filters) (\s@DescribeConfigurationSets' {} a -> s {filters = a} :: DescribeConfigurationSets) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return per each request.
describeConfigurationSets_maxResults :: Lens.Lens' DescribeConfigurationSets (Prelude.Maybe Prelude.Natural)
describeConfigurationSets_maxResults = Lens.lens (\DescribeConfigurationSets' {maxResults} -> maxResults) (\s@DescribeConfigurationSets' {} a -> s {maxResults = a} :: DescribeConfigurationSets)

-- | The token to be used for the next set of paginated results. You don\'t
-- need to supply a value for this field in the initial request.
describeConfigurationSets_nextToken :: Lens.Lens' DescribeConfigurationSets (Prelude.Maybe Prelude.Text)
describeConfigurationSets_nextToken = Lens.lens (\DescribeConfigurationSets' {nextToken} -> nextToken) (\s@DescribeConfigurationSets' {} a -> s {nextToken = a} :: DescribeConfigurationSets)

instance Core.AWSPager DescribeConfigurationSets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeConfigurationSetsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeConfigurationSetsResponse_configurationSets
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeConfigurationSets_nextToken
          Lens..~ rs
          Lens.^? describeConfigurationSetsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeConfigurationSets where
  type
    AWSResponse DescribeConfigurationSets =
      DescribeConfigurationSetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConfigurationSetsResponse'
            Prelude.<$> ( x
                            Data..?> "ConfigurationSets"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeConfigurationSets where
  hashWithSalt _salt DescribeConfigurationSets' {..} =
    _salt
      `Prelude.hashWithSalt` configurationSetNames
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeConfigurationSets where
  rnf DescribeConfigurationSets' {..} =
    Prelude.rnf configurationSetNames
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeConfigurationSets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PinpointSMSVoiceV2.DescribeConfigurationSets" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeConfigurationSets where
  toJSON DescribeConfigurationSets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConfigurationSetNames" Data..=)
              Prelude.<$> configurationSetNames,
            ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeConfigurationSets where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeConfigurationSets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeConfigurationSetsResponse' smart constructor.
data DescribeConfigurationSetsResponse = DescribeConfigurationSetsResponse'
  { -- | An array of ConfigurationSets objects.
    configurationSets :: Prelude.Maybe [ConfigurationSetInformation],
    -- | The token to be used for the next set of paginated results. If this
    -- field is empty then there are no more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConfigurationSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSets', 'describeConfigurationSetsResponse_configurationSets' - An array of ConfigurationSets objects.
--
-- 'nextToken', 'describeConfigurationSetsResponse_nextToken' - The token to be used for the next set of paginated results. If this
-- field is empty then there are no more results.
--
-- 'httpStatus', 'describeConfigurationSetsResponse_httpStatus' - The response's http status code.
newDescribeConfigurationSetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeConfigurationSetsResponse
newDescribeConfigurationSetsResponse pHttpStatus_ =
  DescribeConfigurationSetsResponse'
    { configurationSets =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of ConfigurationSets objects.
describeConfigurationSetsResponse_configurationSets :: Lens.Lens' DescribeConfigurationSetsResponse (Prelude.Maybe [ConfigurationSetInformation])
describeConfigurationSetsResponse_configurationSets = Lens.lens (\DescribeConfigurationSetsResponse' {configurationSets} -> configurationSets) (\s@DescribeConfigurationSetsResponse' {} a -> s {configurationSets = a} :: DescribeConfigurationSetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to be used for the next set of paginated results. If this
-- field is empty then there are no more results.
describeConfigurationSetsResponse_nextToken :: Lens.Lens' DescribeConfigurationSetsResponse (Prelude.Maybe Prelude.Text)
describeConfigurationSetsResponse_nextToken = Lens.lens (\DescribeConfigurationSetsResponse' {nextToken} -> nextToken) (\s@DescribeConfigurationSetsResponse' {} a -> s {nextToken = a} :: DescribeConfigurationSetsResponse)

-- | The response's http status code.
describeConfigurationSetsResponse_httpStatus :: Lens.Lens' DescribeConfigurationSetsResponse Prelude.Int
describeConfigurationSetsResponse_httpStatus = Lens.lens (\DescribeConfigurationSetsResponse' {httpStatus} -> httpStatus) (\s@DescribeConfigurationSetsResponse' {} a -> s {httpStatus = a} :: DescribeConfigurationSetsResponse)

instance
  Prelude.NFData
    DescribeConfigurationSetsResponse
  where
  rnf DescribeConfigurationSetsResponse' {..} =
    Prelude.rnf configurationSets
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
