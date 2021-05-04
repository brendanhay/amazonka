{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.WAF.UpdateWebACL
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
-- Inserts or deletes ActivatedRule objects in a @WebACL@. Each @Rule@
-- identifies web requests that you want to allow, block, or count. When
-- you update a @WebACL@, you specify the following values:
--
-- -   A default action for the @WebACL@, either @ALLOW@ or @BLOCK@. AWS
--     WAF performs the default action if a request doesn\'t match the
--     criteria in any of the @Rules@ in a @WebACL@.
--
-- -   The @Rules@ that you want to add or delete. If you want to replace
--     one @Rule@ with another, you delete the existing @Rule@ and add the
--     new one.
--
-- -   For each @Rule@, whether you want AWS WAF to allow requests, block
--     requests, or count requests that match the conditions in the @Rule@.
--
-- -   The order in which you want AWS WAF to evaluate the @Rules@ in a
--     @WebACL@. If you add more than one @Rule@ to a @WebACL@, AWS WAF
--     evaluates each request against the @Rules@ in order based on the
--     value of @Priority@. (The @Rule@ that has the lowest value for
--     @Priority@ is evaluated first.) When a web request matches all the
--     predicates (such as @ByteMatchSets@ and @IPSets@) in a @Rule@, AWS
--     WAF immediately takes the corresponding action, allow or block, and
--     doesn\'t evaluate the request against the remaining @Rules@ in the
--     @WebACL@, if any.
--
-- To create and configure a @WebACL@, perform the following steps:
--
-- 1.  Create and update the predicates that you want to include in
--     @Rules@. For more information, see CreateByteMatchSet,
--     UpdateByteMatchSet, CreateIPSet, UpdateIPSet,
--     CreateSqlInjectionMatchSet, and UpdateSqlInjectionMatchSet.
--
-- 2.  Create and update the @Rules@ that you want to include in the
--     @WebACL@. For more information, see CreateRule and UpdateRule.
--
-- 3.  Create a @WebACL@. See CreateWebACL.
--
-- 4.  Use @GetChangeToken@ to get the change token that you provide in the
--     @ChangeToken@ parameter of an UpdateWebACL request.
--
-- 5.  Submit an @UpdateWebACL@ request to specify the @Rules@ that you
--     want to include in the @WebACL@, to specify the default action, and
--     to associate the @WebACL@ with a CloudFront distribution.
--
--     The @ActivatedRule@ can be a rule group. If you specify a rule group
--     as your @ActivatedRule@ , you can exclude specific rules from that
--     rule group.
--
--     If you already have a rule group associated with a web ACL and want
--     to submit an @UpdateWebACL@ request to exclude certain rules from
--     that rule group, you must first remove the rule group from the web
--     ACL, the re-insert it again, specifying the excluded rules. For
--     details, see ActivatedRule$ExcludedRules .
--
-- Be aware that if you try to add a RATE_BASED rule to a web ACL without
-- setting the rule type when first creating the rule, the UpdateWebACL
-- request will fail because the request tries to add a REGULAR rule (the
-- default rule type) with the specified ID, which does not exist.
--
-- For more information about how to use the AWS WAF API to allow or block
-- HTTP requests, see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide>.
module Network.AWS.WAF.UpdateWebACL
  ( -- * Creating a Request
    UpdateWebACL (..),
    newUpdateWebACL,

    -- * Request Lenses
    updateWebACL_updates,
    updateWebACL_defaultAction,
    updateWebACL_webACLId,
    updateWebACL_changeToken,

    -- * Destructuring the Response
    UpdateWebACLResponse (..),
    newUpdateWebACLResponse,

    -- * Response Lenses
    updateWebACLResponse_changeToken,
    updateWebACLResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | /See:/ 'newUpdateWebACL' smart constructor.
data UpdateWebACL = UpdateWebACL'
  { -- | An array of updates to make to the WebACL.
    --
    -- An array of @WebACLUpdate@ objects that you want to insert into or
    -- delete from a WebACL. For more information, see the applicable data
    -- types:
    --
    -- -   WebACLUpdate: Contains @Action@ and @ActivatedRule@
    --
    -- -   ActivatedRule: Contains @Action@, @OverrideAction@, @Priority@,
    --     @RuleId@, and @Type@. @ActivatedRule|OverrideAction@ applies only
    --     when updating or adding a @RuleGroup@ to a @WebACL@. In this case,
    --     you do not use @ActivatedRule|Action@. For all other update
    --     requests, @ActivatedRule|Action@ is used instead of
    --     @ActivatedRule|OverrideAction@.
    --
    -- -   WafAction: Contains @Type@
    updates :: Prelude.Maybe [WebACLUpdate],
    -- | A default action for the web ACL, either ALLOW or BLOCK. AWS WAF
    -- performs the default action if a request doesn\'t match the criteria in
    -- any of the rules in a web ACL.
    defaultAction :: Prelude.Maybe WafAction,
    -- | The @WebACLId@ of the WebACL that you want to update. @WebACLId@ is
    -- returned by CreateWebACL and by ListWebACLs.
    webACLId :: Prelude.Text,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateWebACL' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updates', 'updateWebACL_updates' - An array of updates to make to the WebACL.
--
-- An array of @WebACLUpdate@ objects that you want to insert into or
-- delete from a WebACL. For more information, see the applicable data
-- types:
--
-- -   WebACLUpdate: Contains @Action@ and @ActivatedRule@
--
-- -   ActivatedRule: Contains @Action@, @OverrideAction@, @Priority@,
--     @RuleId@, and @Type@. @ActivatedRule|OverrideAction@ applies only
--     when updating or adding a @RuleGroup@ to a @WebACL@. In this case,
--     you do not use @ActivatedRule|Action@. For all other update
--     requests, @ActivatedRule|Action@ is used instead of
--     @ActivatedRule|OverrideAction@.
--
-- -   WafAction: Contains @Type@
--
-- 'defaultAction', 'updateWebACL_defaultAction' - A default action for the web ACL, either ALLOW or BLOCK. AWS WAF
-- performs the default action if a request doesn\'t match the criteria in
-- any of the rules in a web ACL.
--
-- 'webACLId', 'updateWebACL_webACLId' - The @WebACLId@ of the WebACL that you want to update. @WebACLId@ is
-- returned by CreateWebACL and by ListWebACLs.
--
-- 'changeToken', 'updateWebACL_changeToken' - The value returned by the most recent call to GetChangeToken.
newUpdateWebACL ::
  -- | 'webACLId'
  Prelude.Text ->
  -- | 'changeToken'
  Prelude.Text ->
  UpdateWebACL
newUpdateWebACL pWebACLId_ pChangeToken_ =
  UpdateWebACL'
    { updates = Prelude.Nothing,
      defaultAction = Prelude.Nothing,
      webACLId = pWebACLId_,
      changeToken = pChangeToken_
    }

-- | An array of updates to make to the WebACL.
--
-- An array of @WebACLUpdate@ objects that you want to insert into or
-- delete from a WebACL. For more information, see the applicable data
-- types:
--
-- -   WebACLUpdate: Contains @Action@ and @ActivatedRule@
--
-- -   ActivatedRule: Contains @Action@, @OverrideAction@, @Priority@,
--     @RuleId@, and @Type@. @ActivatedRule|OverrideAction@ applies only
--     when updating or adding a @RuleGroup@ to a @WebACL@. In this case,
--     you do not use @ActivatedRule|Action@. For all other update
--     requests, @ActivatedRule|Action@ is used instead of
--     @ActivatedRule|OverrideAction@.
--
-- -   WafAction: Contains @Type@
updateWebACL_updates :: Lens.Lens' UpdateWebACL (Prelude.Maybe [WebACLUpdate])
updateWebACL_updates = Lens.lens (\UpdateWebACL' {updates} -> updates) (\s@UpdateWebACL' {} a -> s {updates = a} :: UpdateWebACL) Prelude.. Lens.mapping Prelude._Coerce

-- | A default action for the web ACL, either ALLOW or BLOCK. AWS WAF
-- performs the default action if a request doesn\'t match the criteria in
-- any of the rules in a web ACL.
updateWebACL_defaultAction :: Lens.Lens' UpdateWebACL (Prelude.Maybe WafAction)
updateWebACL_defaultAction = Lens.lens (\UpdateWebACL' {defaultAction} -> defaultAction) (\s@UpdateWebACL' {} a -> s {defaultAction = a} :: UpdateWebACL)

-- | The @WebACLId@ of the WebACL that you want to update. @WebACLId@ is
-- returned by CreateWebACL and by ListWebACLs.
updateWebACL_webACLId :: Lens.Lens' UpdateWebACL Prelude.Text
updateWebACL_webACLId = Lens.lens (\UpdateWebACL' {webACLId} -> webACLId) (\s@UpdateWebACL' {} a -> s {webACLId = a} :: UpdateWebACL)

-- | The value returned by the most recent call to GetChangeToken.
updateWebACL_changeToken :: Lens.Lens' UpdateWebACL Prelude.Text
updateWebACL_changeToken = Lens.lens (\UpdateWebACL' {changeToken} -> changeToken) (\s@UpdateWebACL' {} a -> s {changeToken = a} :: UpdateWebACL)

instance Prelude.AWSRequest UpdateWebACL where
  type Rs UpdateWebACL = UpdateWebACLResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateWebACLResponse'
            Prelude.<$> (x Prelude..?> "ChangeToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateWebACL

instance Prelude.NFData UpdateWebACL

instance Prelude.ToHeaders UpdateWebACL where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSWAF_20150824.UpdateWebACL" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateWebACL where
  toJSON UpdateWebACL' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Updates" Prelude..=) Prelude.<$> updates,
            ("DefaultAction" Prelude..=)
              Prelude.<$> defaultAction,
            Prelude.Just ("WebACLId" Prelude..= webACLId),
            Prelude.Just ("ChangeToken" Prelude..= changeToken)
          ]
      )

instance Prelude.ToPath UpdateWebACL where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateWebACL where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateWebACLResponse' smart constructor.
data UpdateWebACLResponse = UpdateWebACLResponse'
  { -- | The @ChangeToken@ that you used to submit the @UpdateWebACL@ request.
    -- You can also use this value to query the status of the request. For more
    -- information, see GetChangeTokenStatus.
    changeToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateWebACLResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeToken', 'updateWebACLResponse_changeToken' - The @ChangeToken@ that you used to submit the @UpdateWebACL@ request.
-- You can also use this value to query the status of the request. For more
-- information, see GetChangeTokenStatus.
--
-- 'httpStatus', 'updateWebACLResponse_httpStatus' - The response's http status code.
newUpdateWebACLResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateWebACLResponse
newUpdateWebACLResponse pHttpStatus_ =
  UpdateWebACLResponse'
    { changeToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ChangeToken@ that you used to submit the @UpdateWebACL@ request.
-- You can also use this value to query the status of the request. For more
-- information, see GetChangeTokenStatus.
updateWebACLResponse_changeToken :: Lens.Lens' UpdateWebACLResponse (Prelude.Maybe Prelude.Text)
updateWebACLResponse_changeToken = Lens.lens (\UpdateWebACLResponse' {changeToken} -> changeToken) (\s@UpdateWebACLResponse' {} a -> s {changeToken = a} :: UpdateWebACLResponse)

-- | The response's http status code.
updateWebACLResponse_httpStatus :: Lens.Lens' UpdateWebACLResponse Prelude.Int
updateWebACLResponse_httpStatus = Lens.lens (\UpdateWebACLResponse' {httpStatus} -> httpStatus) (\s@UpdateWebACLResponse' {} a -> s {httpStatus = a} :: UpdateWebACLResponse)

instance Prelude.NFData UpdateWebACLResponse
