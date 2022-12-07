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
-- Module      : Amazonka.WAFRegional.UpdateRuleGroup
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
-- Inserts or deletes ActivatedRule objects in a @RuleGroup@.
--
-- You can only insert @REGULAR@ rules into a rule group.
--
-- You can have a maximum of ten rules per rule group.
--
-- To create and configure a @RuleGroup@, perform the following steps:
--
-- 1.  Create and update the @Rules@ that you want to include in the
--     @RuleGroup@. See CreateRule.
--
-- 2.  Use @GetChangeToken@ to get the change token that you provide in the
--     @ChangeToken@ parameter of an UpdateRuleGroup request.
--
-- 3.  Submit an @UpdateRuleGroup@ request to add @Rules@ to the
--     @RuleGroup@.
--
-- 4.  Create and update a @WebACL@ that contains the @RuleGroup@. See
--     CreateWebACL.
--
-- If you want to replace one @Rule@ with another, you delete the existing
-- one and add the new one.
--
-- For more information about how to use the AWS WAF API to allow or block
-- HTTP requests, see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide>.
module Amazonka.WAFRegional.UpdateRuleGroup
  ( -- * Creating a Request
    UpdateRuleGroup (..),
    newUpdateRuleGroup,

    -- * Request Lenses
    updateRuleGroup_ruleGroupId,
    updateRuleGroup_updates,
    updateRuleGroup_changeToken,

    -- * Destructuring the Response
    UpdateRuleGroupResponse (..),
    newUpdateRuleGroupResponse,

    -- * Response Lenses
    updateRuleGroupResponse_changeToken,
    updateRuleGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFRegional.Types

-- | /See:/ 'newUpdateRuleGroup' smart constructor.
data UpdateRuleGroup = UpdateRuleGroup'
  { -- | The @RuleGroupId@ of the RuleGroup that you want to update.
    -- @RuleGroupId@ is returned by CreateRuleGroup and by ListRuleGroups.
    ruleGroupId :: Prelude.Text,
    -- | An array of @RuleGroupUpdate@ objects that you want to insert into or
    -- delete from a RuleGroup.
    --
    -- You can only insert @REGULAR@ rules into a rule group.
    --
    -- @ActivatedRule|OverrideAction@ applies only when updating or adding a
    -- @RuleGroup@ to a @WebACL@. In this case you do not use
    -- @ActivatedRule|Action@. For all other update requests,
    -- @ActivatedRule|Action@ is used instead of
    -- @ActivatedRule|OverrideAction@.
    updates :: Prelude.NonEmpty RuleGroupUpdate,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRuleGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleGroupId', 'updateRuleGroup_ruleGroupId' - The @RuleGroupId@ of the RuleGroup that you want to update.
-- @RuleGroupId@ is returned by CreateRuleGroup and by ListRuleGroups.
--
-- 'updates', 'updateRuleGroup_updates' - An array of @RuleGroupUpdate@ objects that you want to insert into or
-- delete from a RuleGroup.
--
-- You can only insert @REGULAR@ rules into a rule group.
--
-- @ActivatedRule|OverrideAction@ applies only when updating or adding a
-- @RuleGroup@ to a @WebACL@. In this case you do not use
-- @ActivatedRule|Action@. For all other update requests,
-- @ActivatedRule|Action@ is used instead of
-- @ActivatedRule|OverrideAction@.
--
-- 'changeToken', 'updateRuleGroup_changeToken' - The value returned by the most recent call to GetChangeToken.
newUpdateRuleGroup ::
  -- | 'ruleGroupId'
  Prelude.Text ->
  -- | 'updates'
  Prelude.NonEmpty RuleGroupUpdate ->
  -- | 'changeToken'
  Prelude.Text ->
  UpdateRuleGroup
newUpdateRuleGroup
  pRuleGroupId_
  pUpdates_
  pChangeToken_ =
    UpdateRuleGroup'
      { ruleGroupId = pRuleGroupId_,
        updates = Lens.coerced Lens.# pUpdates_,
        changeToken = pChangeToken_
      }

-- | The @RuleGroupId@ of the RuleGroup that you want to update.
-- @RuleGroupId@ is returned by CreateRuleGroup and by ListRuleGroups.
updateRuleGroup_ruleGroupId :: Lens.Lens' UpdateRuleGroup Prelude.Text
updateRuleGroup_ruleGroupId = Lens.lens (\UpdateRuleGroup' {ruleGroupId} -> ruleGroupId) (\s@UpdateRuleGroup' {} a -> s {ruleGroupId = a} :: UpdateRuleGroup)

-- | An array of @RuleGroupUpdate@ objects that you want to insert into or
-- delete from a RuleGroup.
--
-- You can only insert @REGULAR@ rules into a rule group.
--
-- @ActivatedRule|OverrideAction@ applies only when updating or adding a
-- @RuleGroup@ to a @WebACL@. In this case you do not use
-- @ActivatedRule|Action@. For all other update requests,
-- @ActivatedRule|Action@ is used instead of
-- @ActivatedRule|OverrideAction@.
updateRuleGroup_updates :: Lens.Lens' UpdateRuleGroup (Prelude.NonEmpty RuleGroupUpdate)
updateRuleGroup_updates = Lens.lens (\UpdateRuleGroup' {updates} -> updates) (\s@UpdateRuleGroup' {} a -> s {updates = a} :: UpdateRuleGroup) Prelude.. Lens.coerced

-- | The value returned by the most recent call to GetChangeToken.
updateRuleGroup_changeToken :: Lens.Lens' UpdateRuleGroup Prelude.Text
updateRuleGroup_changeToken = Lens.lens (\UpdateRuleGroup' {changeToken} -> changeToken) (\s@UpdateRuleGroup' {} a -> s {changeToken = a} :: UpdateRuleGroup)

instance Core.AWSRequest UpdateRuleGroup where
  type
    AWSResponse UpdateRuleGroup =
      UpdateRuleGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRuleGroupResponse'
            Prelude.<$> (x Data..?> "ChangeToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRuleGroup where
  hashWithSalt _salt UpdateRuleGroup' {..} =
    _salt `Prelude.hashWithSalt` ruleGroupId
      `Prelude.hashWithSalt` updates
      `Prelude.hashWithSalt` changeToken

instance Prelude.NFData UpdateRuleGroup where
  rnf UpdateRuleGroup' {..} =
    Prelude.rnf ruleGroupId
      `Prelude.seq` Prelude.rnf updates
      `Prelude.seq` Prelude.rnf changeToken

instance Data.ToHeaders UpdateRuleGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_Regional_20161128.UpdateRuleGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRuleGroup where
  toJSON UpdateRuleGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("RuleGroupId" Data..= ruleGroupId),
            Prelude.Just ("Updates" Data..= updates),
            Prelude.Just ("ChangeToken" Data..= changeToken)
          ]
      )

instance Data.ToPath UpdateRuleGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateRuleGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRuleGroupResponse' smart constructor.
data UpdateRuleGroupResponse = UpdateRuleGroupResponse'
  { -- | The @ChangeToken@ that you used to submit the @UpdateRuleGroup@ request.
    -- You can also use this value to query the status of the request. For more
    -- information, see GetChangeTokenStatus.
    changeToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRuleGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeToken', 'updateRuleGroupResponse_changeToken' - The @ChangeToken@ that you used to submit the @UpdateRuleGroup@ request.
-- You can also use this value to query the status of the request. For more
-- information, see GetChangeTokenStatus.
--
-- 'httpStatus', 'updateRuleGroupResponse_httpStatus' - The response's http status code.
newUpdateRuleGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRuleGroupResponse
newUpdateRuleGroupResponse pHttpStatus_ =
  UpdateRuleGroupResponse'
    { changeToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ChangeToken@ that you used to submit the @UpdateRuleGroup@ request.
-- You can also use this value to query the status of the request. For more
-- information, see GetChangeTokenStatus.
updateRuleGroupResponse_changeToken :: Lens.Lens' UpdateRuleGroupResponse (Prelude.Maybe Prelude.Text)
updateRuleGroupResponse_changeToken = Lens.lens (\UpdateRuleGroupResponse' {changeToken} -> changeToken) (\s@UpdateRuleGroupResponse' {} a -> s {changeToken = a} :: UpdateRuleGroupResponse)

-- | The response's http status code.
updateRuleGroupResponse_httpStatus :: Lens.Lens' UpdateRuleGroupResponse Prelude.Int
updateRuleGroupResponse_httpStatus = Lens.lens (\UpdateRuleGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateRuleGroupResponse' {} a -> s {httpStatus = a} :: UpdateRuleGroupResponse)

instance Prelude.NFData UpdateRuleGroupResponse where
  rnf UpdateRuleGroupResponse' {..} =
    Prelude.rnf changeToken
      `Prelude.seq` Prelude.rnf httpStatus
