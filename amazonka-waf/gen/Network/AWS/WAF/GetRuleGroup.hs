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
-- Module      : Network.AWS.WAF.GetRuleGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- Returns the RuleGroup that is specified by the @RuleGroupId@ that you
-- included in the @GetRuleGroup@ request.
--
-- To view the rules in a rule group, use ListActivatedRulesInRuleGroup.
module Network.AWS.WAF.GetRuleGroup
  ( -- * Creating a Request
    GetRuleGroup (..),
    newGetRuleGroup,

    -- * Request Lenses
    getRuleGroup_ruleGroupId,

    -- * Destructuring the Response
    GetRuleGroupResponse (..),
    newGetRuleGroupResponse,

    -- * Response Lenses
    getRuleGroupResponse_ruleGroup,
    getRuleGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | /See:/ 'newGetRuleGroup' smart constructor.
data GetRuleGroup = GetRuleGroup'
  { -- | The @RuleGroupId@ of the RuleGroup that you want to get. @RuleGroupId@
    -- is returned by CreateRuleGroup and by ListRuleGroups.
    ruleGroupId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetRuleGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleGroupId', 'getRuleGroup_ruleGroupId' - The @RuleGroupId@ of the RuleGroup that you want to get. @RuleGroupId@
-- is returned by CreateRuleGroup and by ListRuleGroups.
newGetRuleGroup ::
  -- | 'ruleGroupId'
  Core.Text ->
  GetRuleGroup
newGetRuleGroup pRuleGroupId_ =
  GetRuleGroup' {ruleGroupId = pRuleGroupId_}

-- | The @RuleGroupId@ of the RuleGroup that you want to get. @RuleGroupId@
-- is returned by CreateRuleGroup and by ListRuleGroups.
getRuleGroup_ruleGroupId :: Lens.Lens' GetRuleGroup Core.Text
getRuleGroup_ruleGroupId = Lens.lens (\GetRuleGroup' {ruleGroupId} -> ruleGroupId) (\s@GetRuleGroup' {} a -> s {ruleGroupId = a} :: GetRuleGroup)

instance Core.AWSRequest GetRuleGroup where
  type AWSResponse GetRuleGroup = GetRuleGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRuleGroupResponse'
            Core.<$> (x Core..?> "RuleGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetRuleGroup

instance Core.NFData GetRuleGroup

instance Core.ToHeaders GetRuleGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSWAF_20150824.GetRuleGroup" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetRuleGroup where
  toJSON GetRuleGroup' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("RuleGroupId" Core..= ruleGroupId)]
      )

instance Core.ToPath GetRuleGroup where
  toPath = Core.const "/"

instance Core.ToQuery GetRuleGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetRuleGroupResponse' smart constructor.
data GetRuleGroupResponse = GetRuleGroupResponse'
  { -- | Information about the RuleGroup that you specified in the @GetRuleGroup@
    -- request.
    ruleGroup :: Core.Maybe RuleGroup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetRuleGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleGroup', 'getRuleGroupResponse_ruleGroup' - Information about the RuleGroup that you specified in the @GetRuleGroup@
-- request.
--
-- 'httpStatus', 'getRuleGroupResponse_httpStatus' - The response's http status code.
newGetRuleGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetRuleGroupResponse
newGetRuleGroupResponse pHttpStatus_ =
  GetRuleGroupResponse'
    { ruleGroup = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the RuleGroup that you specified in the @GetRuleGroup@
-- request.
getRuleGroupResponse_ruleGroup :: Lens.Lens' GetRuleGroupResponse (Core.Maybe RuleGroup)
getRuleGroupResponse_ruleGroup = Lens.lens (\GetRuleGroupResponse' {ruleGroup} -> ruleGroup) (\s@GetRuleGroupResponse' {} a -> s {ruleGroup = a} :: GetRuleGroupResponse)

-- | The response's http status code.
getRuleGroupResponse_httpStatus :: Lens.Lens' GetRuleGroupResponse Core.Int
getRuleGroupResponse_httpStatus = Lens.lens (\GetRuleGroupResponse' {httpStatus} -> httpStatus) (\s@GetRuleGroupResponse' {} a -> s {httpStatus = a} :: GetRuleGroupResponse)

instance Core.NFData GetRuleGroupResponse
