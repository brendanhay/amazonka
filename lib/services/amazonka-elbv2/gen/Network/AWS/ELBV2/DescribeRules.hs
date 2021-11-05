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
-- Module      : Network.AWS.ELBV2.DescribeRules
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified rules or the rules for the specified listener.
-- You must specify either a listener or one or more rules.
--
-- This operation returns paginated results.
module Network.AWS.ELBV2.DescribeRules
  ( -- * Creating a Request
    DescribeRules (..),
    newDescribeRules,

    -- * Request Lenses
    describeRules_listenerArn,
    describeRules_marker,
    describeRules_ruleArns,
    describeRules_pageSize,

    -- * Destructuring the Response
    DescribeRulesResponse (..),
    newDescribeRulesResponse,

    -- * Response Lenses
    describeRulesResponse_rules,
    describeRulesResponse_nextMarker,
    describeRulesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELBV2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeRules' smart constructor.
data DescribeRules = DescribeRules'
  { -- | The Amazon Resource Name (ARN) of the listener.
    listenerArn :: Prelude.Maybe Prelude.Text,
    -- | The marker for the next set of results. (You received this marker from a
    -- previous call.)
    marker :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Names (ARN) of the rules.
    ruleArns :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural
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
-- 'ruleArns', 'describeRules_ruleArns' - The Amazon Resource Names (ARN) of the rules.
--
-- 'pageSize', 'describeRules_pageSize' - The maximum number of results to return with this call.
newDescribeRules ::
  DescribeRules
newDescribeRules =
  DescribeRules'
    { listenerArn = Prelude.Nothing,
      marker = Prelude.Nothing,
      ruleArns = Prelude.Nothing,
      pageSize = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the listener.
describeRules_listenerArn :: Lens.Lens' DescribeRules (Prelude.Maybe Prelude.Text)
describeRules_listenerArn = Lens.lens (\DescribeRules' {listenerArn} -> listenerArn) (\s@DescribeRules' {} a -> s {listenerArn = a} :: DescribeRules)

-- | The marker for the next set of results. (You received this marker from a
-- previous call.)
describeRules_marker :: Lens.Lens' DescribeRules (Prelude.Maybe Prelude.Text)
describeRules_marker = Lens.lens (\DescribeRules' {marker} -> marker) (\s@DescribeRules' {} a -> s {marker = a} :: DescribeRules)

-- | The Amazon Resource Names (ARN) of the rules.
describeRules_ruleArns :: Lens.Lens' DescribeRules (Prelude.Maybe [Prelude.Text])
describeRules_ruleArns = Lens.lens (\DescribeRules' {ruleArns} -> ruleArns) (\s@DescribeRules' {} a -> s {ruleArns = a} :: DescribeRules) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with this call.
describeRules_pageSize :: Lens.Lens' DescribeRules (Prelude.Maybe Prelude.Natural)
describeRules_pageSize = Lens.lens (\DescribeRules' {pageSize} -> pageSize) (\s@DescribeRules' {} a -> s {pageSize = a} :: DescribeRules)

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
            Lens.^? describeRulesResponse_rules Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeRules_marker
          Lens..~ rs
          Lens.^? describeRulesResponse_nextMarker Prelude.. Lens._Just

instance Core.AWSRequest DescribeRules where
  type
    AWSResponse DescribeRules =
      DescribeRulesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeRulesResult"
      ( \s h x ->
          DescribeRulesResponse'
            Prelude.<$> ( x Core..@? "Rules" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (x Core..@? "NextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeRules

instance Prelude.NFData DescribeRules

instance Core.ToHeaders DescribeRules where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeRules where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeRules where
  toQuery DescribeRules' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeRules" :: Prelude.ByteString),
        "Version"
          Core.=: ("2015-12-01" :: Prelude.ByteString),
        "ListenerArn" Core.=: listenerArn,
        "Marker" Core.=: marker,
        "RuleArns"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> ruleArns),
        "PageSize" Core.=: pageSize
      ]

-- | /See:/ 'newDescribeRulesResponse' smart constructor.
data DescribeRulesResponse = DescribeRulesResponse'
  { -- | Information about the rules.
    rules :: Prelude.Maybe [Rule],
    -- | If there are additional results, this is the marker for the next set of
    -- results. Otherwise, this is null.
    nextMarker :: Prelude.Maybe Prelude.Text,
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
-- 'rules', 'describeRulesResponse_rules' - Information about the rules.
--
-- 'nextMarker', 'describeRulesResponse_nextMarker' - If there are additional results, this is the marker for the next set of
-- results. Otherwise, this is null.
--
-- 'httpStatus', 'describeRulesResponse_httpStatus' - The response's http status code.
newDescribeRulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRulesResponse
newDescribeRulesResponse pHttpStatus_ =
  DescribeRulesResponse'
    { rules = Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the rules.
describeRulesResponse_rules :: Lens.Lens' DescribeRulesResponse (Prelude.Maybe [Rule])
describeRulesResponse_rules = Lens.lens (\DescribeRulesResponse' {rules} -> rules) (\s@DescribeRulesResponse' {} a -> s {rules = a} :: DescribeRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | If there are additional results, this is the marker for the next set of
-- results. Otherwise, this is null.
describeRulesResponse_nextMarker :: Lens.Lens' DescribeRulesResponse (Prelude.Maybe Prelude.Text)
describeRulesResponse_nextMarker = Lens.lens (\DescribeRulesResponse' {nextMarker} -> nextMarker) (\s@DescribeRulesResponse' {} a -> s {nextMarker = a} :: DescribeRulesResponse)

-- | The response's http status code.
describeRulesResponse_httpStatus :: Lens.Lens' DescribeRulesResponse Prelude.Int
describeRulesResponse_httpStatus = Lens.lens (\DescribeRulesResponse' {httpStatus} -> httpStatus) (\s@DescribeRulesResponse' {} a -> s {httpStatus = a} :: DescribeRulesResponse)

instance Prelude.NFData DescribeRulesResponse
