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
-- Module      : Network.AWS.ELBv2.DescribeRules
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
module Network.AWS.ELBv2.DescribeRules
  ( -- * Creating a Request
    DescribeRules (..),
    newDescribeRules,

    -- * Request Lenses
    describeRules_pageSize,
    describeRules_listenerArn,
    describeRules_ruleArns,
    describeRules_marker,

    -- * Destructuring the Response
    DescribeRulesResponse (..),
    newDescribeRulesResponse,

    -- * Response Lenses
    describeRulesResponse_nextMarker,
    describeRulesResponse_rules,
    describeRulesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeRules' smart constructor.
data DescribeRules = DescribeRules'
  { -- | The maximum number of results to return with this call.
    pageSize :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Name (ARN) of the listener.
    listenerArn :: Core.Maybe Core.Text,
    -- | The Amazon Resource Names (ARN) of the rules.
    ruleArns :: Core.Maybe [Core.Text],
    -- | The marker for the next set of results. (You received this marker from a
    -- previous call.)
    marker :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'describeRules_pageSize' - The maximum number of results to return with this call.
--
-- 'listenerArn', 'describeRules_listenerArn' - The Amazon Resource Name (ARN) of the listener.
--
-- 'ruleArns', 'describeRules_ruleArns' - The Amazon Resource Names (ARN) of the rules.
--
-- 'marker', 'describeRules_marker' - The marker for the next set of results. (You received this marker from a
-- previous call.)
newDescribeRules ::
  DescribeRules
newDescribeRules =
  DescribeRules'
    { pageSize = Core.Nothing,
      listenerArn = Core.Nothing,
      ruleArns = Core.Nothing,
      marker = Core.Nothing
    }

-- | The maximum number of results to return with this call.
describeRules_pageSize :: Lens.Lens' DescribeRules (Core.Maybe Core.Natural)
describeRules_pageSize = Lens.lens (\DescribeRules' {pageSize} -> pageSize) (\s@DescribeRules' {} a -> s {pageSize = a} :: DescribeRules)

-- | The Amazon Resource Name (ARN) of the listener.
describeRules_listenerArn :: Lens.Lens' DescribeRules (Core.Maybe Core.Text)
describeRules_listenerArn = Lens.lens (\DescribeRules' {listenerArn} -> listenerArn) (\s@DescribeRules' {} a -> s {listenerArn = a} :: DescribeRules)

-- | The Amazon Resource Names (ARN) of the rules.
describeRules_ruleArns :: Lens.Lens' DescribeRules (Core.Maybe [Core.Text])
describeRules_ruleArns = Lens.lens (\DescribeRules' {ruleArns} -> ruleArns) (\s@DescribeRules' {} a -> s {ruleArns = a} :: DescribeRules) Core.. Lens.mapping Lens._Coerce

-- | The marker for the next set of results. (You received this marker from a
-- previous call.)
describeRules_marker :: Lens.Lens' DescribeRules (Core.Maybe Core.Text)
describeRules_marker = Lens.lens (\DescribeRules' {marker} -> marker) (\s@DescribeRules' {} a -> s {marker = a} :: DescribeRules)

instance Core.AWSPager DescribeRules where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeRulesResponse_nextMarker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeRulesResponse_rules Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeRules_marker
          Lens..~ rs
          Lens.^? describeRulesResponse_nextMarker Core.. Lens._Just

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
            Core.<$> (x Core..@? "NextMarker")
            Core.<*> ( x Core..@? "Rules" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeRules

instance Core.NFData DescribeRules

instance Core.ToHeaders DescribeRules where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeRules where
  toPath = Core.const "/"

instance Core.ToQuery DescribeRules where
  toQuery DescribeRules' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeRules" :: Core.ByteString),
        "Version" Core.=: ("2015-12-01" :: Core.ByteString),
        "PageSize" Core.=: pageSize,
        "ListenerArn" Core.=: listenerArn,
        "RuleArns"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> ruleArns),
        "Marker" Core.=: marker
      ]

-- | /See:/ 'newDescribeRulesResponse' smart constructor.
data DescribeRulesResponse = DescribeRulesResponse'
  { -- | If there are additional results, this is the marker for the next set of
    -- results. Otherwise, this is null.
    nextMarker :: Core.Maybe Core.Text,
    -- | Information about the rules.
    rules :: Core.Maybe [Rule],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeRulesResponse
newDescribeRulesResponse pHttpStatus_ =
  DescribeRulesResponse'
    { nextMarker = Core.Nothing,
      rules = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the marker for the next set of
-- results. Otherwise, this is null.
describeRulesResponse_nextMarker :: Lens.Lens' DescribeRulesResponse (Core.Maybe Core.Text)
describeRulesResponse_nextMarker = Lens.lens (\DescribeRulesResponse' {nextMarker} -> nextMarker) (\s@DescribeRulesResponse' {} a -> s {nextMarker = a} :: DescribeRulesResponse)

-- | Information about the rules.
describeRulesResponse_rules :: Lens.Lens' DescribeRulesResponse (Core.Maybe [Rule])
describeRulesResponse_rules = Lens.lens (\DescribeRulesResponse' {rules} -> rules) (\s@DescribeRulesResponse' {} a -> s {rules = a} :: DescribeRulesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeRulesResponse_httpStatus :: Lens.Lens' DescribeRulesResponse Core.Int
describeRulesResponse_httpStatus = Lens.lens (\DescribeRulesResponse' {httpStatus} -> httpStatus) (\s@DescribeRulesResponse' {} a -> s {httpStatus = a} :: DescribeRulesResponse)

instance Core.NFData DescribeRulesResponse
