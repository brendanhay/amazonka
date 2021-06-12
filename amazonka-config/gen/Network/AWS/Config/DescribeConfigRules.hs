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
-- Returns details about your AWS Config rules.
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeConfigRules
  ( -- * Creating a Request
    DescribeConfigRules (..),
    newDescribeConfigRules,

    -- * Request Lenses
    describeConfigRules_nextToken,
    describeConfigRules_configRuleNames,

    -- * Destructuring the Response
    DescribeConfigRulesResponse (..),
    newDescribeConfigRulesResponse,

    -- * Response Lenses
    describeConfigRulesResponse_nextToken,
    describeConfigRulesResponse_configRules,
    describeConfigRulesResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeConfigRules' smart constructor.
data DescribeConfigRules = DescribeConfigRules'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | The names of the AWS Config rules for which you want details. If you do
    -- not specify any names, AWS Config returns details for all your rules.
    configRuleNames :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeConfigRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeConfigRules_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'configRuleNames', 'describeConfigRules_configRuleNames' - The names of the AWS Config rules for which you want details. If you do
-- not specify any names, AWS Config returns details for all your rules.
newDescribeConfigRules ::
  DescribeConfigRules
newDescribeConfigRules =
  DescribeConfigRules'
    { nextToken = Core.Nothing,
      configRuleNames = Core.Nothing
    }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeConfigRules_nextToken :: Lens.Lens' DescribeConfigRules (Core.Maybe Core.Text)
describeConfigRules_nextToken = Lens.lens (\DescribeConfigRules' {nextToken} -> nextToken) (\s@DescribeConfigRules' {} a -> s {nextToken = a} :: DescribeConfigRules)

-- | The names of the AWS Config rules for which you want details. If you do
-- not specify any names, AWS Config returns details for all your rules.
describeConfigRules_configRuleNames :: Lens.Lens' DescribeConfigRules (Core.Maybe [Core.Text])
describeConfigRules_configRuleNames = Lens.lens (\DescribeConfigRules' {configRuleNames} -> configRuleNames) (\s@DescribeConfigRules' {} a -> s {configRuleNames = a} :: DescribeConfigRules) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeConfigRules where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeConfigRulesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeConfigRulesResponse_configRules
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeConfigRules_nextToken
          Lens..~ rs
          Lens.^? describeConfigRulesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeConfigRules where
  type
    AWSResponse DescribeConfigRules =
      DescribeConfigRulesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConfigRulesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "ConfigRules" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeConfigRules

instance Core.NFData DescribeConfigRules

instance Core.ToHeaders DescribeConfigRules where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DescribeConfigRules" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeConfigRules where
  toJSON DescribeConfigRules' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("ConfigRuleNames" Core..=)
              Core.<$> configRuleNames
          ]
      )

instance Core.ToPath DescribeConfigRules where
  toPath = Core.const "/"

instance Core.ToQuery DescribeConfigRules where
  toQuery = Core.const Core.mempty

-- |
--
-- /See:/ 'newDescribeConfigRulesResponse' smart constructor.
data DescribeConfigRulesResponse = DescribeConfigRulesResponse'
  { -- | The string that you use in a subsequent request to get the next page of
    -- results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | The details about your AWS Config rules.
    configRules :: Core.Maybe [ConfigRule],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeConfigRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeConfigRulesResponse_nextToken' - The string that you use in a subsequent request to get the next page of
-- results in a paginated response.
--
-- 'configRules', 'describeConfigRulesResponse_configRules' - The details about your AWS Config rules.
--
-- 'httpStatus', 'describeConfigRulesResponse_httpStatus' - The response's http status code.
newDescribeConfigRulesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeConfigRulesResponse
newDescribeConfigRulesResponse pHttpStatus_ =
  DescribeConfigRulesResponse'
    { nextToken =
        Core.Nothing,
      configRules = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The string that you use in a subsequent request to get the next page of
-- results in a paginated response.
describeConfigRulesResponse_nextToken :: Lens.Lens' DescribeConfigRulesResponse (Core.Maybe Core.Text)
describeConfigRulesResponse_nextToken = Lens.lens (\DescribeConfigRulesResponse' {nextToken} -> nextToken) (\s@DescribeConfigRulesResponse' {} a -> s {nextToken = a} :: DescribeConfigRulesResponse)

-- | The details about your AWS Config rules.
describeConfigRulesResponse_configRules :: Lens.Lens' DescribeConfigRulesResponse (Core.Maybe [ConfigRule])
describeConfigRulesResponse_configRules = Lens.lens (\DescribeConfigRulesResponse' {configRules} -> configRules) (\s@DescribeConfigRulesResponse' {} a -> s {configRules = a} :: DescribeConfigRulesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeConfigRulesResponse_httpStatus :: Lens.Lens' DescribeConfigRulesResponse Core.Int
describeConfigRulesResponse_httpStatus = Lens.lens (\DescribeConfigRulesResponse' {httpStatus} -> httpStatus) (\s@DescribeConfigRulesResponse' {} a -> s {httpStatus = a} :: DescribeConfigRulesResponse)

instance Core.NFData DescribeConfigRulesResponse
