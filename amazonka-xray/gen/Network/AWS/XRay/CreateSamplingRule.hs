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
-- Module      : Network.AWS.XRay.CreateSamplingRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a rule to control sampling behavior for instrumented
-- applications. Services retrieve rules with GetSamplingRules, and
-- evaluate each rule in ascending order of /priority/ for each request. If
-- a rule matches, the service records a trace, borrowing it from the
-- reservoir size. After 10 seconds, the service reports back to X-Ray with
-- GetSamplingTargets to get updated versions of each in-use rule. The
-- updated rule contains a trace quota that the service can use instead of
-- borrowing from the reservoir.
module Network.AWS.XRay.CreateSamplingRule
  ( -- * Creating a Request
    CreateSamplingRule (..),
    newCreateSamplingRule,

    -- * Request Lenses
    createSamplingRule_tags,
    createSamplingRule_samplingRule,

    -- * Destructuring the Response
    CreateSamplingRuleResponse (..),
    newCreateSamplingRuleResponse,

    -- * Response Lenses
    createSamplingRuleResponse_samplingRuleRecord,
    createSamplingRuleResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.XRay.Types

-- | /See:/ 'newCreateSamplingRule' smart constructor.
data CreateSamplingRule = CreateSamplingRule'
  { -- | A map that contains one or more tag keys and tag values to attach to an
    -- X-Ray sampling rule. For more information about ways to use tags, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources>
    -- in the /AWS General Reference/.
    --
    -- The following restrictions apply to tags:
    --
    -- -   Maximum number of user-applied tags per resource: 50
    --
    -- -   Maximum tag key length: 128 Unicode characters
    --
    -- -   Maximum tag value length: 256 Unicode characters
    --
    -- -   Valid values for key and value: a-z, A-Z, 0-9, space, and the
    --     following characters: _ . : \/ = + - and \@
    --
    -- -   Tag keys and values are case sensitive.
    --
    -- -   Don\'t use @aws:@ as a prefix for keys; it\'s reserved for AWS use.
    tags :: Core.Maybe [Tag],
    -- | The rule definition.
    samplingRule :: SamplingRule
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateSamplingRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createSamplingRule_tags' - A map that contains one or more tag keys and tag values to attach to an
-- X-Ray sampling rule. For more information about ways to use tags, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources>
-- in the /AWS General Reference/.
--
-- The following restrictions apply to tags:
--
-- -   Maximum number of user-applied tags per resource: 50
--
-- -   Maximum tag key length: 128 Unicode characters
--
-- -   Maximum tag value length: 256 Unicode characters
--
-- -   Valid values for key and value: a-z, A-Z, 0-9, space, and the
--     following characters: _ . : \/ = + - and \@
--
-- -   Tag keys and values are case sensitive.
--
-- -   Don\'t use @aws:@ as a prefix for keys; it\'s reserved for AWS use.
--
-- 'samplingRule', 'createSamplingRule_samplingRule' - The rule definition.
newCreateSamplingRule ::
  -- | 'samplingRule'
  SamplingRule ->
  CreateSamplingRule
newCreateSamplingRule pSamplingRule_ =
  CreateSamplingRule'
    { tags = Core.Nothing,
      samplingRule = pSamplingRule_
    }

-- | A map that contains one or more tag keys and tag values to attach to an
-- X-Ray sampling rule. For more information about ways to use tags, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources>
-- in the /AWS General Reference/.
--
-- The following restrictions apply to tags:
--
-- -   Maximum number of user-applied tags per resource: 50
--
-- -   Maximum tag key length: 128 Unicode characters
--
-- -   Maximum tag value length: 256 Unicode characters
--
-- -   Valid values for key and value: a-z, A-Z, 0-9, space, and the
--     following characters: _ . : \/ = + - and \@
--
-- -   Tag keys and values are case sensitive.
--
-- -   Don\'t use @aws:@ as a prefix for keys; it\'s reserved for AWS use.
createSamplingRule_tags :: Lens.Lens' CreateSamplingRule (Core.Maybe [Tag])
createSamplingRule_tags = Lens.lens (\CreateSamplingRule' {tags} -> tags) (\s@CreateSamplingRule' {} a -> s {tags = a} :: CreateSamplingRule) Core.. Lens.mapping Lens._Coerce

-- | The rule definition.
createSamplingRule_samplingRule :: Lens.Lens' CreateSamplingRule SamplingRule
createSamplingRule_samplingRule = Lens.lens (\CreateSamplingRule' {samplingRule} -> samplingRule) (\s@CreateSamplingRule' {} a -> s {samplingRule = a} :: CreateSamplingRule)

instance Core.AWSRequest CreateSamplingRule where
  type
    AWSResponse CreateSamplingRule =
      CreateSamplingRuleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSamplingRuleResponse'
            Core.<$> (x Core..?> "SamplingRuleRecord")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateSamplingRule

instance Core.NFData CreateSamplingRule

instance Core.ToHeaders CreateSamplingRule where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON CreateSamplingRule where
  toJSON CreateSamplingRule' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Tags" Core..=) Core.<$> tags,
            Core.Just ("SamplingRule" Core..= samplingRule)
          ]
      )

instance Core.ToPath CreateSamplingRule where
  toPath = Core.const "/CreateSamplingRule"

instance Core.ToQuery CreateSamplingRule where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateSamplingRuleResponse' smart constructor.
data CreateSamplingRuleResponse = CreateSamplingRuleResponse'
  { -- | The saved rule definition and metadata.
    samplingRuleRecord :: Core.Maybe SamplingRuleRecord,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateSamplingRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'samplingRuleRecord', 'createSamplingRuleResponse_samplingRuleRecord' - The saved rule definition and metadata.
--
-- 'httpStatus', 'createSamplingRuleResponse_httpStatus' - The response's http status code.
newCreateSamplingRuleResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateSamplingRuleResponse
newCreateSamplingRuleResponse pHttpStatus_ =
  CreateSamplingRuleResponse'
    { samplingRuleRecord =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The saved rule definition and metadata.
createSamplingRuleResponse_samplingRuleRecord :: Lens.Lens' CreateSamplingRuleResponse (Core.Maybe SamplingRuleRecord)
createSamplingRuleResponse_samplingRuleRecord = Lens.lens (\CreateSamplingRuleResponse' {samplingRuleRecord} -> samplingRuleRecord) (\s@CreateSamplingRuleResponse' {} a -> s {samplingRuleRecord = a} :: CreateSamplingRuleResponse)

-- | The response's http status code.
createSamplingRuleResponse_httpStatus :: Lens.Lens' CreateSamplingRuleResponse Core.Int
createSamplingRuleResponse_httpStatus = Lens.lens (\CreateSamplingRuleResponse' {httpStatus} -> httpStatus) (\s@CreateSamplingRuleResponse' {} a -> s {httpStatus = a} :: CreateSamplingRuleResponse)

instance Core.NFData CreateSamplingRuleResponse
