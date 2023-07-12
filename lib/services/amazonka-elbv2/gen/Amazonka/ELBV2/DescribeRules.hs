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
-- Module      : Amazonka.ELBV2.DescribeRules
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified rules or the rules for the specified listener.
-- You must specify either a listener or one or more rules.
--
-- This operation returns paginated results.
module Amazonka.ELBV2.DescribeRules
  ( -- * Creating a Request
    DescribeRules (..),
    newDescribeRules,

    -- * Request Lenses
    describeRules_listenerArn,
    describeRules_marker,
    describeRules_pageSize,
    describeRules_ruleArns,

    -- * Destructuring the Response
    DescribeRulesResponse (..),
    newDescribeRulesResponse,

    -- * Response Lenses
    describeRulesResponse_nextMarker,
    describeRulesResponse_rules,
    describeRulesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELBV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeRules' smart constructor.
data DescribeRules = DescribeRules'
  { -- | The Amazon Resource Name (ARN) of the listener.
    listenerArn :: Prelude.Maybe Prelude.Text,
    -- | The marker for the next set of results. (You received this marker from a
    -- previous call.)
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Names (ARN) of the rules.
    ruleArns :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listenerArn', 'describeRules_listenerArn' - The Amazon Resource Name (ARN) of the listener.
--
-- 'marker', 'describeRules_marker' - The marker for the next set of results. (You received this marker from a
-- previous call.)
--
-- 'pageSize', 'describeRules_pageSize' - The maximum number of results to return with this call.
--
-- 'ruleArns', 'describeRules_ruleArns' - The Amazon Resource Names (ARN) of the rules.
newDescribeRules ::
  DescribeRules
newDescribeRules =
  DescribeRules'
    { listenerArn = Prelude.Nothing,
      marker = Prelude.Nothing,
      pageSize = Prelude.Nothing,
      ruleArns = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the listener.
describeRules_listenerArn :: Lens.Lens' DescribeRules (Prelude.Maybe Prelude.Text)
describeRules_listenerArn = Lens.lens (\DescribeRules' {listenerArn} -> listenerArn) (\s@DescribeRules' {} a -> s {listenerArn = a} :: DescribeRules)

-- | The marker for the next set of results. (You received this marker from a
-- previous call.)
describeRules_marker :: Lens.Lens' DescribeRules (Prelude.Maybe Prelude.Text)
describeRules_marker = Lens.lens (\DescribeRules' {marker} -> marker) (\s@DescribeRules' {} a -> s {marker = a} :: DescribeRules)

-- | The maximum number of results to return with this call.
describeRules_pageSize :: Lens.Lens' DescribeRules (Prelude.Maybe Prelude.Natural)
describeRules_pageSize = Lens.lens (\DescribeRules' {pageSize} -> pageSize) (\s@DescribeRules' {} a -> s {pageSize = a} :: DescribeRules)

-- | The Amazon Resource Names (ARN) of the rules.
describeRules_ruleArns :: Lens.Lens' DescribeRules (Prelude.Maybe [Prelude.Text])
describeRules_ruleArns = Lens.lens (\DescribeRules' {ruleArns} -> ruleArns) (\s@DescribeRules' {} a -> s {ruleArns = a} :: DescribeRules) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager DescribeRules where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeRulesResponse_nextMarker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeRulesResponse_rules
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeRules_marker
          Lens..~ rs
          Lens.^? describeRulesResponse_nextMarker
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeRules where
  type
    AWSResponse DescribeRules =
      DescribeRulesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeRulesResult"
      ( \s h x ->
          DescribeRulesResponse'
            Prelude.<$> (x Data..@? "NextMarker")
            Prelude.<*> ( x
                            Data..@? "Rules"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeRules where
  hashWithSalt _salt DescribeRules' {..} =
    _salt
      `Prelude.hashWithSalt` listenerArn
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` ruleArns

instance Prelude.NFData DescribeRules where
  rnf DescribeRules' {..} =
    Prelude.rnf listenerArn
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf ruleArns

instance Data.ToHeaders DescribeRules where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeRules where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeRules where
  toQuery DescribeRules' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeRules" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-12-01" :: Prelude.ByteString),
        "ListenerArn" Data.=: listenerArn,
        "Marker" Data.=: marker,
        "PageSize" Data.=: pageSize,
        "RuleArns"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> ruleArns)
      ]

-- | /See:/ 'newDescribeRulesResponse' smart constructor.
data DescribeRulesResponse = DescribeRulesResponse'
  { -- | If there are additional results, this is the marker for the next set of
    -- results. Otherwise, this is null.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | Information about the rules.
    rules :: Prelude.Maybe [Rule],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'describeRulesResponse_nextMarker' - If there are additional results, this is the marker for the next set of
-- results. Otherwise, this is null.
--
-- 'rules', 'describeRulesResponse_rules' - Information about the rules.
--
-- 'httpStatus', 'describeRulesResponse_httpStatus' - The response's http status code.
newDescribeRulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRulesResponse
newDescribeRulesResponse pHttpStatus_ =
  DescribeRulesResponse'
    { nextMarker =
        Prelude.Nothing,
      rules = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the marker for the next set of
-- results. Otherwise, this is null.
describeRulesResponse_nextMarker :: Lens.Lens' DescribeRulesResponse (Prelude.Maybe Prelude.Text)
describeRulesResponse_nextMarker = Lens.lens (\DescribeRulesResponse' {nextMarker} -> nextMarker) (\s@DescribeRulesResponse' {} a -> s {nextMarker = a} :: DescribeRulesResponse)

-- | Information about the rules.
describeRulesResponse_rules :: Lens.Lens' DescribeRulesResponse (Prelude.Maybe [Rule])
describeRulesResponse_rules = Lens.lens (\DescribeRulesResponse' {rules} -> rules) (\s@DescribeRulesResponse' {} a -> s {rules = a} :: DescribeRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeRulesResponse_httpStatus :: Lens.Lens' DescribeRulesResponse Prelude.Int
describeRulesResponse_httpStatus = Lens.lens (\DescribeRulesResponse' {httpStatus} -> httpStatus) (\s@DescribeRulesResponse' {} a -> s {httpStatus = a} :: DescribeRulesResponse)

instance Prelude.NFData DescribeRulesResponse where
  rnf DescribeRulesResponse' {..} =
    Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf rules
      `Prelude.seq` Prelude.rnf httpStatus
