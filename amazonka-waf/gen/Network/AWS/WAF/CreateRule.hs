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
-- Module      : Network.AWS.WAF.CreateRule
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
-- Creates a @Rule@, which contains the @IPSet@ objects, @ByteMatchSet@
-- objects, and other predicates that identify the requests that you want
-- to block. If you add more than one predicate to a @Rule@, a request must
-- match all of the specifications to be allowed or blocked. For example,
-- suppose that you add the following to a @Rule@:
--
-- -   An @IPSet@ that matches the IP address @192.0.2.44\/32@
--
-- -   A @ByteMatchSet@ that matches @BadBot@ in the @User-Agent@ header
--
-- You then add the @Rule@ to a @WebACL@ and specify that you want to
-- blocks requests that satisfy the @Rule@. For a request to be blocked, it
-- must come from the IP address 192.0.2.44 /and/ the @User-Agent@ header
-- in the request must contain the value @BadBot@.
--
-- To create and configure a @Rule@, perform the following steps:
--
-- 1.  Create and update the predicates that you want to include in the
--     @Rule@. For more information, see CreateByteMatchSet, CreateIPSet,
--     and CreateSqlInjectionMatchSet.
--
-- 2.  Use GetChangeToken to get the change token that you provide in the
--     @ChangeToken@ parameter of a @CreateRule@ request.
--
-- 3.  Submit a @CreateRule@ request.
--
-- 4.  Use @GetChangeToken@ to get the change token that you provide in the
--     @ChangeToken@ parameter of an UpdateRule request.
--
-- 5.  Submit an @UpdateRule@ request to specify the predicates that you
--     want to include in the @Rule@.
--
-- 6.  Create and update a @WebACL@ that contains the @Rule@. For more
--     information, see CreateWebACL.
--
-- For more information about how to use the AWS WAF API to allow or block
-- HTTP requests, see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide>.
module Network.AWS.WAF.CreateRule
  ( -- * Creating a Request
    CreateRule (..),
    newCreateRule,

    -- * Request Lenses
    createRule_tags,
    createRule_name,
    createRule_metricName,
    createRule_changeToken,

    -- * Destructuring the Response
    CreateRuleResponse (..),
    newCreateRuleResponse,

    -- * Response Lenses
    createRuleResponse_rule,
    createRuleResponse_changeToken,
    createRuleResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | /See:/ 'newCreateRule' smart constructor.
data CreateRule = CreateRule'
  { tags :: Core.Maybe (Core.NonEmpty Tag),
    -- | A friendly name or description of the Rule. You can\'t change the name
    -- of a @Rule@ after you create it.
    name :: Core.Text,
    -- | A friendly name or description for the metrics for this @Rule@. The name
    -- can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum
    -- length 128 and minimum length one. It can\'t contain whitespace or
    -- metric names reserved for AWS WAF, including \"All\" and
    -- \"Default_Action.\" You can\'t change the name of the metric after you
    -- create the @Rule@.
    metricName :: Core.Text,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createRule_tags' -
--
-- 'name', 'createRule_name' - A friendly name or description of the Rule. You can\'t change the name
-- of a @Rule@ after you create it.
--
-- 'metricName', 'createRule_metricName' - A friendly name or description for the metrics for this @Rule@. The name
-- can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum
-- length 128 and minimum length one. It can\'t contain whitespace or
-- metric names reserved for AWS WAF, including \"All\" and
-- \"Default_Action.\" You can\'t change the name of the metric after you
-- create the @Rule@.
--
-- 'changeToken', 'createRule_changeToken' - The value returned by the most recent call to GetChangeToken.
newCreateRule ::
  -- | 'name'
  Core.Text ->
  -- | 'metricName'
  Core.Text ->
  -- | 'changeToken'
  Core.Text ->
  CreateRule
newCreateRule pName_ pMetricName_ pChangeToken_ =
  CreateRule'
    { tags = Core.Nothing,
      name = pName_,
      metricName = pMetricName_,
      changeToken = pChangeToken_
    }

-- |
createRule_tags :: Lens.Lens' CreateRule (Core.Maybe (Core.NonEmpty Tag))
createRule_tags = Lens.lens (\CreateRule' {tags} -> tags) (\s@CreateRule' {} a -> s {tags = a} :: CreateRule) Core.. Lens.mapping Lens._Coerce

-- | A friendly name or description of the Rule. You can\'t change the name
-- of a @Rule@ after you create it.
createRule_name :: Lens.Lens' CreateRule Core.Text
createRule_name = Lens.lens (\CreateRule' {name} -> name) (\s@CreateRule' {} a -> s {name = a} :: CreateRule)

-- | A friendly name or description for the metrics for this @Rule@. The name
-- can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum
-- length 128 and minimum length one. It can\'t contain whitespace or
-- metric names reserved for AWS WAF, including \"All\" and
-- \"Default_Action.\" You can\'t change the name of the metric after you
-- create the @Rule@.
createRule_metricName :: Lens.Lens' CreateRule Core.Text
createRule_metricName = Lens.lens (\CreateRule' {metricName} -> metricName) (\s@CreateRule' {} a -> s {metricName = a} :: CreateRule)

-- | The value returned by the most recent call to GetChangeToken.
createRule_changeToken :: Lens.Lens' CreateRule Core.Text
createRule_changeToken = Lens.lens (\CreateRule' {changeToken} -> changeToken) (\s@CreateRule' {} a -> s {changeToken = a} :: CreateRule)

instance Core.AWSRequest CreateRule where
  type AWSResponse CreateRule = CreateRuleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRuleResponse'
            Core.<$> (x Core..?> "Rule")
            Core.<*> (x Core..?> "ChangeToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateRule

instance Core.NFData CreateRule

instance Core.ToHeaders CreateRule where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSWAF_20150824.CreateRule" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateRule where
  toJSON CreateRule' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Tags" Core..=) Core.<$> tags,
            Core.Just ("Name" Core..= name),
            Core.Just ("MetricName" Core..= metricName),
            Core.Just ("ChangeToken" Core..= changeToken)
          ]
      )

instance Core.ToPath CreateRule where
  toPath = Core.const "/"

instance Core.ToQuery CreateRule where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateRuleResponse' smart constructor.
data CreateRuleResponse = CreateRuleResponse'
  { -- | The Rule returned in the @CreateRule@ response.
    rule :: Core.Maybe Rule,
    -- | The @ChangeToken@ that you used to submit the @CreateRule@ request. You
    -- can also use this value to query the status of the request. For more
    -- information, see GetChangeTokenStatus.
    changeToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rule', 'createRuleResponse_rule' - The Rule returned in the @CreateRule@ response.
--
-- 'changeToken', 'createRuleResponse_changeToken' - The @ChangeToken@ that you used to submit the @CreateRule@ request. You
-- can also use this value to query the status of the request. For more
-- information, see GetChangeTokenStatus.
--
-- 'httpStatus', 'createRuleResponse_httpStatus' - The response's http status code.
newCreateRuleResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateRuleResponse
newCreateRuleResponse pHttpStatus_ =
  CreateRuleResponse'
    { rule = Core.Nothing,
      changeToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Rule returned in the @CreateRule@ response.
createRuleResponse_rule :: Lens.Lens' CreateRuleResponse (Core.Maybe Rule)
createRuleResponse_rule = Lens.lens (\CreateRuleResponse' {rule} -> rule) (\s@CreateRuleResponse' {} a -> s {rule = a} :: CreateRuleResponse)

-- | The @ChangeToken@ that you used to submit the @CreateRule@ request. You
-- can also use this value to query the status of the request. For more
-- information, see GetChangeTokenStatus.
createRuleResponse_changeToken :: Lens.Lens' CreateRuleResponse (Core.Maybe Core.Text)
createRuleResponse_changeToken = Lens.lens (\CreateRuleResponse' {changeToken} -> changeToken) (\s@CreateRuleResponse' {} a -> s {changeToken = a} :: CreateRuleResponse)

-- | The response's http status code.
createRuleResponse_httpStatus :: Lens.Lens' CreateRuleResponse Core.Int
createRuleResponse_httpStatus = Lens.lens (\CreateRuleResponse' {httpStatus} -> httpStatus) (\s@CreateRuleResponse' {} a -> s {httpStatus = a} :: CreateRuleResponse)

instance Core.NFData CreateRuleResponse
