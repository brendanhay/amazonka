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
-- Module      : Network.AWS.WAF.DeleteRuleGroup
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
-- Permanently deletes a RuleGroup. You can\'t delete a @RuleGroup@ if
-- it\'s still used in any @WebACL@ objects or if it still includes any
-- rules.
--
-- If you just want to remove a @RuleGroup@ from a @WebACL@, use
-- UpdateWebACL.
--
-- To permanently delete a @RuleGroup@ from AWS WAF, perform the following
-- steps:
--
-- 1.  Update the @RuleGroup@ to remove rules, if any. For more
--     information, see UpdateRuleGroup.
--
-- 2.  Use GetChangeToken to get the change token that you provide in the
--     @ChangeToken@ parameter of a @DeleteRuleGroup@ request.
--
-- 3.  Submit a @DeleteRuleGroup@ request.
module Network.AWS.WAF.DeleteRuleGroup
  ( -- * Creating a Request
    DeleteRuleGroup (..),
    newDeleteRuleGroup,

    -- * Request Lenses
    deleteRuleGroup_ruleGroupId,
    deleteRuleGroup_changeToken,

    -- * Destructuring the Response
    DeleteRuleGroupResponse (..),
    newDeleteRuleGroupResponse,

    -- * Response Lenses
    deleteRuleGroupResponse_changeToken,
    deleteRuleGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | /See:/ 'newDeleteRuleGroup' smart constructor.
data DeleteRuleGroup = DeleteRuleGroup'
  { -- | The @RuleGroupId@ of the RuleGroup that you want to delete.
    -- @RuleGroupId@ is returned by CreateRuleGroup and by ListRuleGroups.
    ruleGroupId :: Prelude.Text,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteRuleGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleGroupId', 'deleteRuleGroup_ruleGroupId' - The @RuleGroupId@ of the RuleGroup that you want to delete.
-- @RuleGroupId@ is returned by CreateRuleGroup and by ListRuleGroups.
--
-- 'changeToken', 'deleteRuleGroup_changeToken' - The value returned by the most recent call to GetChangeToken.
newDeleteRuleGroup ::
  -- | 'ruleGroupId'
  Prelude.Text ->
  -- | 'changeToken'
  Prelude.Text ->
  DeleteRuleGroup
newDeleteRuleGroup pRuleGroupId_ pChangeToken_ =
  DeleteRuleGroup'
    { ruleGroupId = pRuleGroupId_,
      changeToken = pChangeToken_
    }

-- | The @RuleGroupId@ of the RuleGroup that you want to delete.
-- @RuleGroupId@ is returned by CreateRuleGroup and by ListRuleGroups.
deleteRuleGroup_ruleGroupId :: Lens.Lens' DeleteRuleGroup Prelude.Text
deleteRuleGroup_ruleGroupId = Lens.lens (\DeleteRuleGroup' {ruleGroupId} -> ruleGroupId) (\s@DeleteRuleGroup' {} a -> s {ruleGroupId = a} :: DeleteRuleGroup)

-- | The value returned by the most recent call to GetChangeToken.
deleteRuleGroup_changeToken :: Lens.Lens' DeleteRuleGroup Prelude.Text
deleteRuleGroup_changeToken = Lens.lens (\DeleteRuleGroup' {changeToken} -> changeToken) (\s@DeleteRuleGroup' {} a -> s {changeToken = a} :: DeleteRuleGroup)

instance Prelude.AWSRequest DeleteRuleGroup where
  type Rs DeleteRuleGroup = DeleteRuleGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRuleGroupResponse'
            Prelude.<$> (x Prelude..?> "ChangeToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRuleGroup

instance Prelude.NFData DeleteRuleGroup

instance Prelude.ToHeaders DeleteRuleGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSWAF_20150824.DeleteRuleGroup" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteRuleGroup where
  toJSON DeleteRuleGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("RuleGroupId" Prelude..= ruleGroupId),
            Prelude.Just ("ChangeToken" Prelude..= changeToken)
          ]
      )

instance Prelude.ToPath DeleteRuleGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteRuleGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRuleGroupResponse' smart constructor.
data DeleteRuleGroupResponse = DeleteRuleGroupResponse'
  { -- | The @ChangeToken@ that you used to submit the @DeleteRuleGroup@ request.
    -- You can also use this value to query the status of the request. For more
    -- information, see GetChangeTokenStatus.
    changeToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteRuleGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeToken', 'deleteRuleGroupResponse_changeToken' - The @ChangeToken@ that you used to submit the @DeleteRuleGroup@ request.
-- You can also use this value to query the status of the request. For more
-- information, see GetChangeTokenStatus.
--
-- 'httpStatus', 'deleteRuleGroupResponse_httpStatus' - The response's http status code.
newDeleteRuleGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteRuleGroupResponse
newDeleteRuleGroupResponse pHttpStatus_ =
  DeleteRuleGroupResponse'
    { changeToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ChangeToken@ that you used to submit the @DeleteRuleGroup@ request.
-- You can also use this value to query the status of the request. For more
-- information, see GetChangeTokenStatus.
deleteRuleGroupResponse_changeToken :: Lens.Lens' DeleteRuleGroupResponse (Prelude.Maybe Prelude.Text)
deleteRuleGroupResponse_changeToken = Lens.lens (\DeleteRuleGroupResponse' {changeToken} -> changeToken) (\s@DeleteRuleGroupResponse' {} a -> s {changeToken = a} :: DeleteRuleGroupResponse)

-- | The response's http status code.
deleteRuleGroupResponse_httpStatus :: Lens.Lens' DeleteRuleGroupResponse Prelude.Int
deleteRuleGroupResponse_httpStatus = Lens.lens (\DeleteRuleGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteRuleGroupResponse' {} a -> s {httpStatus = a} :: DeleteRuleGroupResponse)

instance Prelude.NFData DeleteRuleGroupResponse
