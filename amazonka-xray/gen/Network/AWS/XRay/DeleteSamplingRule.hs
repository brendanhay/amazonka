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
-- Module      : Network.AWS.XRay.DeleteSamplingRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a sampling rule.
module Network.AWS.XRay.DeleteSamplingRule
  ( -- * Creating a Request
    DeleteSamplingRule (..),
    newDeleteSamplingRule,

    -- * Request Lenses
    deleteSamplingRule_ruleName,
    deleteSamplingRule_ruleARN,

    -- * Destructuring the Response
    DeleteSamplingRuleResponse (..),
    newDeleteSamplingRuleResponse,

    -- * Response Lenses
    deleteSamplingRuleResponse_samplingRuleRecord,
    deleteSamplingRuleResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.XRay.Types

-- | /See:/ 'newDeleteSamplingRule' smart constructor.
data DeleteSamplingRule = DeleteSamplingRule'
  { -- | The name of the sampling rule. Specify a rule by either name or ARN, but
    -- not both.
    ruleName :: Core.Maybe Core.Text,
    -- | The ARN of the sampling rule. Specify a rule by either name or ARN, but
    -- not both.
    ruleARN :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSamplingRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleName', 'deleteSamplingRule_ruleName' - The name of the sampling rule. Specify a rule by either name or ARN, but
-- not both.
--
-- 'ruleARN', 'deleteSamplingRule_ruleARN' - The ARN of the sampling rule. Specify a rule by either name or ARN, but
-- not both.
newDeleteSamplingRule ::
  DeleteSamplingRule
newDeleteSamplingRule =
  DeleteSamplingRule'
    { ruleName = Core.Nothing,
      ruleARN = Core.Nothing
    }

-- | The name of the sampling rule. Specify a rule by either name or ARN, but
-- not both.
deleteSamplingRule_ruleName :: Lens.Lens' DeleteSamplingRule (Core.Maybe Core.Text)
deleteSamplingRule_ruleName = Lens.lens (\DeleteSamplingRule' {ruleName} -> ruleName) (\s@DeleteSamplingRule' {} a -> s {ruleName = a} :: DeleteSamplingRule)

-- | The ARN of the sampling rule. Specify a rule by either name or ARN, but
-- not both.
deleteSamplingRule_ruleARN :: Lens.Lens' DeleteSamplingRule (Core.Maybe Core.Text)
deleteSamplingRule_ruleARN = Lens.lens (\DeleteSamplingRule' {ruleARN} -> ruleARN) (\s@DeleteSamplingRule' {} a -> s {ruleARN = a} :: DeleteSamplingRule)

instance Core.AWSRequest DeleteSamplingRule where
  type
    AWSResponse DeleteSamplingRule =
      DeleteSamplingRuleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSamplingRuleResponse'
            Core.<$> (x Core..?> "SamplingRuleRecord")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteSamplingRule

instance Core.NFData DeleteSamplingRule

instance Core.ToHeaders DeleteSamplingRule where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON DeleteSamplingRule where
  toJSON DeleteSamplingRule' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RuleName" Core..=) Core.<$> ruleName,
            ("RuleARN" Core..=) Core.<$> ruleARN
          ]
      )

instance Core.ToPath DeleteSamplingRule where
  toPath = Core.const "/DeleteSamplingRule"

instance Core.ToQuery DeleteSamplingRule where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteSamplingRuleResponse' smart constructor.
data DeleteSamplingRuleResponse = DeleteSamplingRuleResponse'
  { -- | The deleted rule definition and metadata.
    samplingRuleRecord :: Core.Maybe SamplingRuleRecord,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSamplingRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'samplingRuleRecord', 'deleteSamplingRuleResponse_samplingRuleRecord' - The deleted rule definition and metadata.
--
-- 'httpStatus', 'deleteSamplingRuleResponse_httpStatus' - The response's http status code.
newDeleteSamplingRuleResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteSamplingRuleResponse
newDeleteSamplingRuleResponse pHttpStatus_ =
  DeleteSamplingRuleResponse'
    { samplingRuleRecord =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The deleted rule definition and metadata.
deleteSamplingRuleResponse_samplingRuleRecord :: Lens.Lens' DeleteSamplingRuleResponse (Core.Maybe SamplingRuleRecord)
deleteSamplingRuleResponse_samplingRuleRecord = Lens.lens (\DeleteSamplingRuleResponse' {samplingRuleRecord} -> samplingRuleRecord) (\s@DeleteSamplingRuleResponse' {} a -> s {samplingRuleRecord = a} :: DeleteSamplingRuleResponse)

-- | The response's http status code.
deleteSamplingRuleResponse_httpStatus :: Lens.Lens' DeleteSamplingRuleResponse Core.Int
deleteSamplingRuleResponse_httpStatus = Lens.lens (\DeleteSamplingRuleResponse' {httpStatus} -> httpStatus) (\s@DeleteSamplingRuleResponse' {} a -> s {httpStatus = a} :: DeleteSamplingRuleResponse)

instance Core.NFData DeleteSamplingRuleResponse
