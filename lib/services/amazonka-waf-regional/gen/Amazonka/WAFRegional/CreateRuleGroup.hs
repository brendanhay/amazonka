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
-- Module      : Amazonka.WAFRegional.CreateRuleGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- Creates a @RuleGroup@. A rule group is a collection of predefined rules
-- that you add to a web ACL. You use UpdateRuleGroup to add rules to the
-- rule group.
--
-- Rule groups are subject to the following limits:
--
-- -   Three rule groups per account. You can request an increase to this
--     limit by contacting customer support.
--
-- -   One rule group per web ACL.
--
-- -   Ten rules per rule group.
--
-- For more information about how to use the AWS WAF API to allow or block
-- HTTP requests, see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide>.
module Amazonka.WAFRegional.CreateRuleGroup
  ( -- * Creating a Request
    CreateRuleGroup (..),
    newCreateRuleGroup,

    -- * Request Lenses
    createRuleGroup_tags,
    createRuleGroup_name,
    createRuleGroup_metricName,
    createRuleGroup_changeToken,

    -- * Destructuring the Response
    CreateRuleGroupResponse (..),
    newCreateRuleGroupResponse,

    -- * Response Lenses
    createRuleGroupResponse_ruleGroup,
    createRuleGroupResponse_changeToken,
    createRuleGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFRegional.Types

-- | /See:/ 'newCreateRuleGroup' smart constructor.
data CreateRuleGroup = CreateRuleGroup'
  { tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | A friendly name or description of the RuleGroup. You can\'t change
    -- @Name@ after you create a @RuleGroup@.
    name :: Prelude.Text,
    -- | A friendly name or description for the metrics for this @RuleGroup@. The
    -- name can contain only alphanumeric characters (A-Z, a-z, 0-9), with
    -- maximum length 128 and minimum length one. It can\'t contain whitespace
    -- or metric names reserved for AWS WAF, including \"All\" and
    -- \"Default_Action.\" You can\'t change the name of the metric after you
    -- create the @RuleGroup@.
    metricName :: Prelude.Text,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRuleGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createRuleGroup_tags' -
--
-- 'name', 'createRuleGroup_name' - A friendly name or description of the RuleGroup. You can\'t change
-- @Name@ after you create a @RuleGroup@.
--
-- 'metricName', 'createRuleGroup_metricName' - A friendly name or description for the metrics for this @RuleGroup@. The
-- name can contain only alphanumeric characters (A-Z, a-z, 0-9), with
-- maximum length 128 and minimum length one. It can\'t contain whitespace
-- or metric names reserved for AWS WAF, including \"All\" and
-- \"Default_Action.\" You can\'t change the name of the metric after you
-- create the @RuleGroup@.
--
-- 'changeToken', 'createRuleGroup_changeToken' - The value returned by the most recent call to GetChangeToken.
newCreateRuleGroup ::
  -- | 'name'
  Prelude.Text ->
  -- | 'metricName'
  Prelude.Text ->
  -- | 'changeToken'
  Prelude.Text ->
  CreateRuleGroup
newCreateRuleGroup pName_ pMetricName_ pChangeToken_ =
  CreateRuleGroup'
    { tags = Prelude.Nothing,
      name = pName_,
      metricName = pMetricName_,
      changeToken = pChangeToken_
    }

-- |
createRuleGroup_tags :: Lens.Lens' CreateRuleGroup (Prelude.Maybe (Prelude.NonEmpty Tag))
createRuleGroup_tags = Lens.lens (\CreateRuleGroup' {tags} -> tags) (\s@CreateRuleGroup' {} a -> s {tags = a} :: CreateRuleGroup) Prelude.. Lens.mapping Lens.coerced

-- | A friendly name or description of the RuleGroup. You can\'t change
-- @Name@ after you create a @RuleGroup@.
createRuleGroup_name :: Lens.Lens' CreateRuleGroup Prelude.Text
createRuleGroup_name = Lens.lens (\CreateRuleGroup' {name} -> name) (\s@CreateRuleGroup' {} a -> s {name = a} :: CreateRuleGroup)

-- | A friendly name or description for the metrics for this @RuleGroup@. The
-- name can contain only alphanumeric characters (A-Z, a-z, 0-9), with
-- maximum length 128 and minimum length one. It can\'t contain whitespace
-- or metric names reserved for AWS WAF, including \"All\" and
-- \"Default_Action.\" You can\'t change the name of the metric after you
-- create the @RuleGroup@.
createRuleGroup_metricName :: Lens.Lens' CreateRuleGroup Prelude.Text
createRuleGroup_metricName = Lens.lens (\CreateRuleGroup' {metricName} -> metricName) (\s@CreateRuleGroup' {} a -> s {metricName = a} :: CreateRuleGroup)

-- | The value returned by the most recent call to GetChangeToken.
createRuleGroup_changeToken :: Lens.Lens' CreateRuleGroup Prelude.Text
createRuleGroup_changeToken = Lens.lens (\CreateRuleGroup' {changeToken} -> changeToken) (\s@CreateRuleGroup' {} a -> s {changeToken = a} :: CreateRuleGroup)

instance Core.AWSRequest CreateRuleGroup where
  type
    AWSResponse CreateRuleGroup =
      CreateRuleGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRuleGroupResponse'
            Prelude.<$> (x Core..?> "RuleGroup")
            Prelude.<*> (x Core..?> "ChangeToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateRuleGroup where
  hashWithSalt _salt CreateRuleGroup' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` changeToken

instance Prelude.NFData CreateRuleGroup where
  rnf CreateRuleGroup' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf changeToken

instance Core.ToHeaders CreateRuleGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_Regional_20161128.CreateRuleGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateRuleGroup where
  toJSON CreateRuleGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("MetricName" Core..= metricName),
            Prelude.Just ("ChangeToken" Core..= changeToken)
          ]
      )

instance Core.ToPath CreateRuleGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateRuleGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRuleGroupResponse' smart constructor.
data CreateRuleGroupResponse = CreateRuleGroupResponse'
  { -- | An empty RuleGroup.
    ruleGroup :: Prelude.Maybe RuleGroup,
    -- | The @ChangeToken@ that you used to submit the @CreateRuleGroup@ request.
    -- You can also use this value to query the status of the request. For more
    -- information, see GetChangeTokenStatus.
    changeToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRuleGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleGroup', 'createRuleGroupResponse_ruleGroup' - An empty RuleGroup.
--
-- 'changeToken', 'createRuleGroupResponse_changeToken' - The @ChangeToken@ that you used to submit the @CreateRuleGroup@ request.
-- You can also use this value to query the status of the request. For more
-- information, see GetChangeTokenStatus.
--
-- 'httpStatus', 'createRuleGroupResponse_httpStatus' - The response's http status code.
newCreateRuleGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateRuleGroupResponse
newCreateRuleGroupResponse pHttpStatus_ =
  CreateRuleGroupResponse'
    { ruleGroup =
        Prelude.Nothing,
      changeToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An empty RuleGroup.
createRuleGroupResponse_ruleGroup :: Lens.Lens' CreateRuleGroupResponse (Prelude.Maybe RuleGroup)
createRuleGroupResponse_ruleGroup = Lens.lens (\CreateRuleGroupResponse' {ruleGroup} -> ruleGroup) (\s@CreateRuleGroupResponse' {} a -> s {ruleGroup = a} :: CreateRuleGroupResponse)

-- | The @ChangeToken@ that you used to submit the @CreateRuleGroup@ request.
-- You can also use this value to query the status of the request. For more
-- information, see GetChangeTokenStatus.
createRuleGroupResponse_changeToken :: Lens.Lens' CreateRuleGroupResponse (Prelude.Maybe Prelude.Text)
createRuleGroupResponse_changeToken = Lens.lens (\CreateRuleGroupResponse' {changeToken} -> changeToken) (\s@CreateRuleGroupResponse' {} a -> s {changeToken = a} :: CreateRuleGroupResponse)

-- | The response's http status code.
createRuleGroupResponse_httpStatus :: Lens.Lens' CreateRuleGroupResponse Prelude.Int
createRuleGroupResponse_httpStatus = Lens.lens (\CreateRuleGroupResponse' {httpStatus} -> httpStatus) (\s@CreateRuleGroupResponse' {} a -> s {httpStatus = a} :: CreateRuleGroupResponse)

instance Prelude.NFData CreateRuleGroupResponse where
  rnf CreateRuleGroupResponse' {..} =
    Prelude.rnf ruleGroup
      `Prelude.seq` Prelude.rnf changeToken
      `Prelude.seq` Prelude.rnf httpStatus
