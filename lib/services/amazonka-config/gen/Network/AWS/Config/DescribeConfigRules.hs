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
-- Module      : Network.AWS.Config.DescribeConfigRules
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about your Config rules.
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeConfigRules
  ( -- * Creating a Request
    DescribeConfigRules (..),
    newDescribeConfigRules,

    -- * Request Lenses
    describeConfigRules_configRuleNames,
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

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeConfigRules' smart constructor.
data DescribeConfigRules = DescribeConfigRules'
  { -- | The names of the Config rules for which you want details. If you do not
    -- specify any names, Config returns details for all your rules.
    configRuleNames :: Prelude.Maybe [Prelude.Text],
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
-- 'nextToken', 'describeConfigRules_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
newDescribeConfigRules ::
  DescribeConfigRules
newDescribeConfigRules =
  DescribeConfigRules'
    { configRuleNames =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The names of the Config rules for which you want details. If you do not
-- specify any names, Config returns details for all your rules.
describeConfigRules_configRuleNames :: Lens.Lens' DescribeConfigRules (Prelude.Maybe [Prelude.Text])
describeConfigRules_configRuleNames = Lens.lens (\DescribeConfigRules' {configRuleNames} -> configRuleNames) (\s@DescribeConfigRules' {} a -> s {configRuleNames = a} :: DescribeConfigRules) Prelude.. Lens.mapping Lens.coerced

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
      Prelude.Just Prelude.$
        rq
          Prelude.& describeConfigRules_nextToken
          Lens..~ rs
          Lens.^? describeConfigRulesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeConfigRules where
  type
    AWSResponse DescribeConfigRules =
      DescribeConfigRulesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConfigRulesResponse'
            Prelude.<$> (x Core..?> "ConfigRules" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeConfigRules

instance Prelude.NFData DescribeConfigRules

instance Core.ToHeaders DescribeConfigRules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DescribeConfigRules" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeConfigRules where
  toJSON DescribeConfigRules' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ConfigRuleNames" Core..=)
              Prelude.<$> configRuleNames,
            ("NextToken" Core..=) Prelude.<$> nextToken
          ]
      )

instance Core.ToPath DescribeConfigRules where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeConfigRules where
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

instance Prelude.NFData DescribeConfigRulesResponse
