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
-- Module      : Amazonka.Config.DescribeConfigRules
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about your Config rules.
--
-- This operation returns paginated results.
module Amazonka.Config.DescribeConfigRules
  ( -- * Creating a Request
    DescribeConfigRules (..),
    newDescribeConfigRules,

    -- * Request Lenses
    describeConfigRules_configRuleNames,
    describeConfigRules_filters,
    describeConfigRules_nextToken,

    -- * Destructuring the Response
    DescribeConfigRulesResponse (..),
    newDescribeConfigRulesResponse,

    -- * Response Lenses
    describeConfigRulesResponse_configRules,
    describeConfigRulesResponse_nextToken,
    describeConfigRulesResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDescribeConfigRules' smart constructor.
data DescribeConfigRules = DescribeConfigRules'
  { -- | The names of the Config rules for which you want details. If you do not
    -- specify any names, Config returns details for all your rules.
    configRuleNames :: Prelude.Maybe [Prelude.Text],
    -- | Returns a list of Detecive or Proactive Config rules. By default, this
    -- API returns an unfiltered list.
    filters :: Prelude.Maybe DescribeConfigRulesFilters,
    -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConfigRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configRuleNames', 'describeConfigRules_configRuleNames' - The names of the Config rules for which you want details. If you do not
-- specify any names, Config returns details for all your rules.
--
-- 'filters', 'describeConfigRules_filters' - Returns a list of Detecive or Proactive Config rules. By default, this
-- API returns an unfiltered list.
--
-- 'nextToken', 'describeConfigRules_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
newDescribeConfigRules ::
  DescribeConfigRules
newDescribeConfigRules =
  DescribeConfigRules'
    { configRuleNames =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The names of the Config rules for which you want details. If you do not
-- specify any names, Config returns details for all your rules.
describeConfigRules_configRuleNames :: Lens.Lens' DescribeConfigRules (Prelude.Maybe [Prelude.Text])
describeConfigRules_configRuleNames = Lens.lens (\DescribeConfigRules' {configRuleNames} -> configRuleNames) (\s@DescribeConfigRules' {} a -> s {configRuleNames = a} :: DescribeConfigRules) Prelude.. Lens.mapping Lens.coerced

-- | Returns a list of Detecive or Proactive Config rules. By default, this
-- API returns an unfiltered list.
describeConfigRules_filters :: Lens.Lens' DescribeConfigRules (Prelude.Maybe DescribeConfigRulesFilters)
describeConfigRules_filters = Lens.lens (\DescribeConfigRules' {filters} -> filters) (\s@DescribeConfigRules' {} a -> s {filters = a} :: DescribeConfigRules)

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeConfigRules_nextToken :: Lens.Lens' DescribeConfigRules (Prelude.Maybe Prelude.Text)
describeConfigRules_nextToken = Lens.lens (\DescribeConfigRules' {nextToken} -> nextToken) (\s@DescribeConfigRules' {} a -> s {nextToken = a} :: DescribeConfigRules)

instance Core.AWSPager DescribeConfigRules where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeConfigRulesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeConfigRulesResponse_configRules
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeConfigRules_nextToken
          Lens..~ rs
          Lens.^? describeConfigRulesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeConfigRules where
  type
    AWSResponse DescribeConfigRules =
      DescribeConfigRulesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConfigRulesResponse'
            Prelude.<$> (x Data..?> "ConfigRules" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeConfigRules where
  hashWithSalt _salt DescribeConfigRules' {..} =
    _salt
      `Prelude.hashWithSalt` configRuleNames
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeConfigRules where
  rnf DescribeConfigRules' {..} =
    Prelude.rnf configRuleNames
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeConfigRules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.DescribeConfigRules" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeConfigRules where
  toJSON DescribeConfigRules' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConfigRuleNames" Data..=)
              Prelude.<$> configRuleNames,
            ("Filters" Data..=) Prelude.<$> filters,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeConfigRules where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeConfigRules where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDescribeConfigRulesResponse' smart constructor.
data DescribeConfigRulesResponse = DescribeConfigRulesResponse'
  { -- | The details about your Config rules.
    configRules :: Prelude.Maybe [ConfigRule],
    -- | The string that you use in a subsequent request to get the next page of
    -- results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConfigRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configRules', 'describeConfigRulesResponse_configRules' - The details about your Config rules.
--
-- 'nextToken', 'describeConfigRulesResponse_nextToken' - The string that you use in a subsequent request to get the next page of
-- results in a paginated response.
--
-- 'httpStatus', 'describeConfigRulesResponse_httpStatus' - The response's http status code.
newDescribeConfigRulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeConfigRulesResponse
newDescribeConfigRulesResponse pHttpStatus_ =
  DescribeConfigRulesResponse'
    { configRules =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details about your Config rules.
describeConfigRulesResponse_configRules :: Lens.Lens' DescribeConfigRulesResponse (Prelude.Maybe [ConfigRule])
describeConfigRulesResponse_configRules = Lens.lens (\DescribeConfigRulesResponse' {configRules} -> configRules) (\s@DescribeConfigRulesResponse' {} a -> s {configRules = a} :: DescribeConfigRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The string that you use in a subsequent request to get the next page of
-- results in a paginated response.
describeConfigRulesResponse_nextToken :: Lens.Lens' DescribeConfigRulesResponse (Prelude.Maybe Prelude.Text)
describeConfigRulesResponse_nextToken = Lens.lens (\DescribeConfigRulesResponse' {nextToken} -> nextToken) (\s@DescribeConfigRulesResponse' {} a -> s {nextToken = a} :: DescribeConfigRulesResponse)

-- | The response's http status code.
describeConfigRulesResponse_httpStatus :: Lens.Lens' DescribeConfigRulesResponse Prelude.Int
describeConfigRulesResponse_httpStatus = Lens.lens (\DescribeConfigRulesResponse' {httpStatus} -> httpStatus) (\s@DescribeConfigRulesResponse' {} a -> s {httpStatus = a} :: DescribeConfigRulesResponse)

instance Prelude.NFData DescribeConfigRulesResponse where
  rnf DescribeConfigRulesResponse' {..} =
    Prelude.rnf configRules
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
