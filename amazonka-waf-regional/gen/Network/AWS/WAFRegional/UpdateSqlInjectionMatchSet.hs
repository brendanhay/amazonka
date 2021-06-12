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
-- Module      : Network.AWS.WAFRegional.UpdateSqlInjectionMatchSet
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
-- Inserts or deletes SqlInjectionMatchTuple objects (filters) in a
-- SqlInjectionMatchSet. For each @SqlInjectionMatchTuple@ object, you
-- specify the following values:
--
-- -   @Action@: Whether to insert the object into or delete the object
--     from the array. To change a @SqlInjectionMatchTuple@, you delete the
--     existing object and add a new one.
--
-- -   @FieldToMatch@: The part of web requests that you want AWS WAF to
--     inspect and, if you want AWS WAF to inspect a header or custom query
--     parameter, the name of the header or parameter.
--
-- -   @TextTransformation@: Which text transformation, if any, to perform
--     on the web request before inspecting the request for snippets of
--     malicious SQL code.
--
--     You can only specify a single type of TextTransformation.
--
-- You use @SqlInjectionMatchSet@ objects to specify which CloudFront
-- requests that you want to allow, block, or count. For example, if
-- you\'re receiving requests that contain snippets of SQL code in the
-- query string and you want to block the requests, you can create a
-- @SqlInjectionMatchSet@ with the applicable settings, and then configure
-- AWS WAF to block the requests.
--
-- To create and configure a @SqlInjectionMatchSet@, perform the following
-- steps:
--
-- 1.  Submit a CreateSqlInjectionMatchSet request.
--
-- 2.  Use GetChangeToken to get the change token that you provide in the
--     @ChangeToken@ parameter of an UpdateIPSet request.
--
-- 3.  Submit an @UpdateSqlInjectionMatchSet@ request to specify the parts
--     of web requests that you want AWS WAF to inspect for snippets of SQL
--     code.
--
-- For more information about how to use the AWS WAF API to allow or block
-- HTTP requests, see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide>.
module Network.AWS.WAFRegional.UpdateSqlInjectionMatchSet
  ( -- * Creating a Request
    UpdateSqlInjectionMatchSet (..),
    newUpdateSqlInjectionMatchSet,

    -- * Request Lenses
    updateSqlInjectionMatchSet_sqlInjectionMatchSetId,
    updateSqlInjectionMatchSet_changeToken,
    updateSqlInjectionMatchSet_updates,

    -- * Destructuring the Response
    UpdateSqlInjectionMatchSetResponse (..),
    newUpdateSqlInjectionMatchSetResponse,

    -- * Response Lenses
    updateSqlInjectionMatchSetResponse_changeToken,
    updateSqlInjectionMatchSetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAFRegional.Types

-- | A request to update a SqlInjectionMatchSet.
--
-- /See:/ 'newUpdateSqlInjectionMatchSet' smart constructor.
data UpdateSqlInjectionMatchSet = UpdateSqlInjectionMatchSet'
  { -- | The @SqlInjectionMatchSetId@ of the @SqlInjectionMatchSet@ that you want
    -- to update. @SqlInjectionMatchSetId@ is returned by
    -- CreateSqlInjectionMatchSet and by ListSqlInjectionMatchSets.
    sqlInjectionMatchSetId :: Core.Text,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Core.Text,
    -- | An array of @SqlInjectionMatchSetUpdate@ objects that you want to insert
    -- into or delete from a SqlInjectionMatchSet. For more information, see
    -- the applicable data types:
    --
    -- -   SqlInjectionMatchSetUpdate: Contains @Action@ and
    --     @SqlInjectionMatchTuple@
    --
    -- -   SqlInjectionMatchTuple: Contains @FieldToMatch@ and
    --     @TextTransformation@
    --
    -- -   FieldToMatch: Contains @Data@ and @Type@
    updates :: Core.NonEmpty SqlInjectionMatchSetUpdate
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateSqlInjectionMatchSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sqlInjectionMatchSetId', 'updateSqlInjectionMatchSet_sqlInjectionMatchSetId' - The @SqlInjectionMatchSetId@ of the @SqlInjectionMatchSet@ that you want
-- to update. @SqlInjectionMatchSetId@ is returned by
-- CreateSqlInjectionMatchSet and by ListSqlInjectionMatchSets.
--
-- 'changeToken', 'updateSqlInjectionMatchSet_changeToken' - The value returned by the most recent call to GetChangeToken.
--
-- 'updates', 'updateSqlInjectionMatchSet_updates' - An array of @SqlInjectionMatchSetUpdate@ objects that you want to insert
-- into or delete from a SqlInjectionMatchSet. For more information, see
-- the applicable data types:
--
-- -   SqlInjectionMatchSetUpdate: Contains @Action@ and
--     @SqlInjectionMatchTuple@
--
-- -   SqlInjectionMatchTuple: Contains @FieldToMatch@ and
--     @TextTransformation@
--
-- -   FieldToMatch: Contains @Data@ and @Type@
newUpdateSqlInjectionMatchSet ::
  -- | 'sqlInjectionMatchSetId'
  Core.Text ->
  -- | 'changeToken'
  Core.Text ->
  -- | 'updates'
  Core.NonEmpty SqlInjectionMatchSetUpdate ->
  UpdateSqlInjectionMatchSet
newUpdateSqlInjectionMatchSet
  pSqlInjectionMatchSetId_
  pChangeToken_
  pUpdates_ =
    UpdateSqlInjectionMatchSet'
      { sqlInjectionMatchSetId =
          pSqlInjectionMatchSetId_,
        changeToken = pChangeToken_,
        updates = Lens._Coerce Lens.# pUpdates_
      }

-- | The @SqlInjectionMatchSetId@ of the @SqlInjectionMatchSet@ that you want
-- to update. @SqlInjectionMatchSetId@ is returned by
-- CreateSqlInjectionMatchSet and by ListSqlInjectionMatchSets.
updateSqlInjectionMatchSet_sqlInjectionMatchSetId :: Lens.Lens' UpdateSqlInjectionMatchSet Core.Text
updateSqlInjectionMatchSet_sqlInjectionMatchSetId = Lens.lens (\UpdateSqlInjectionMatchSet' {sqlInjectionMatchSetId} -> sqlInjectionMatchSetId) (\s@UpdateSqlInjectionMatchSet' {} a -> s {sqlInjectionMatchSetId = a} :: UpdateSqlInjectionMatchSet)

-- | The value returned by the most recent call to GetChangeToken.
updateSqlInjectionMatchSet_changeToken :: Lens.Lens' UpdateSqlInjectionMatchSet Core.Text
updateSqlInjectionMatchSet_changeToken = Lens.lens (\UpdateSqlInjectionMatchSet' {changeToken} -> changeToken) (\s@UpdateSqlInjectionMatchSet' {} a -> s {changeToken = a} :: UpdateSqlInjectionMatchSet)

-- | An array of @SqlInjectionMatchSetUpdate@ objects that you want to insert
-- into or delete from a SqlInjectionMatchSet. For more information, see
-- the applicable data types:
--
-- -   SqlInjectionMatchSetUpdate: Contains @Action@ and
--     @SqlInjectionMatchTuple@
--
-- -   SqlInjectionMatchTuple: Contains @FieldToMatch@ and
--     @TextTransformation@
--
-- -   FieldToMatch: Contains @Data@ and @Type@
updateSqlInjectionMatchSet_updates :: Lens.Lens' UpdateSqlInjectionMatchSet (Core.NonEmpty SqlInjectionMatchSetUpdate)
updateSqlInjectionMatchSet_updates = Lens.lens (\UpdateSqlInjectionMatchSet' {updates} -> updates) (\s@UpdateSqlInjectionMatchSet' {} a -> s {updates = a} :: UpdateSqlInjectionMatchSet) Core.. Lens._Coerce

instance Core.AWSRequest UpdateSqlInjectionMatchSet where
  type
    AWSResponse UpdateSqlInjectionMatchSet =
      UpdateSqlInjectionMatchSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSqlInjectionMatchSetResponse'
            Core.<$> (x Core..?> "ChangeToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateSqlInjectionMatchSet

instance Core.NFData UpdateSqlInjectionMatchSet

instance Core.ToHeaders UpdateSqlInjectionMatchSet where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_Regional_20161128.UpdateSqlInjectionMatchSet" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateSqlInjectionMatchSet where
  toJSON UpdateSqlInjectionMatchSet' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "SqlInjectionMatchSetId"
                  Core..= sqlInjectionMatchSetId
              ),
            Core.Just ("ChangeToken" Core..= changeToken),
            Core.Just ("Updates" Core..= updates)
          ]
      )

instance Core.ToPath UpdateSqlInjectionMatchSet where
  toPath = Core.const "/"

instance Core.ToQuery UpdateSqlInjectionMatchSet where
  toQuery = Core.const Core.mempty

-- | The response to an UpdateSqlInjectionMatchSets request.
--
-- /See:/ 'newUpdateSqlInjectionMatchSetResponse' smart constructor.
data UpdateSqlInjectionMatchSetResponse = UpdateSqlInjectionMatchSetResponse'
  { -- | The @ChangeToken@ that you used to submit the
    -- @UpdateSqlInjectionMatchSet@ request. You can also use this value to
    -- query the status of the request. For more information, see
    -- GetChangeTokenStatus.
    changeToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateSqlInjectionMatchSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeToken', 'updateSqlInjectionMatchSetResponse_changeToken' - The @ChangeToken@ that you used to submit the
-- @UpdateSqlInjectionMatchSet@ request. You can also use this value to
-- query the status of the request. For more information, see
-- GetChangeTokenStatus.
--
-- 'httpStatus', 'updateSqlInjectionMatchSetResponse_httpStatus' - The response's http status code.
newUpdateSqlInjectionMatchSetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateSqlInjectionMatchSetResponse
newUpdateSqlInjectionMatchSetResponse pHttpStatus_ =
  UpdateSqlInjectionMatchSetResponse'
    { changeToken =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ChangeToken@ that you used to submit the
-- @UpdateSqlInjectionMatchSet@ request. You can also use this value to
-- query the status of the request. For more information, see
-- GetChangeTokenStatus.
updateSqlInjectionMatchSetResponse_changeToken :: Lens.Lens' UpdateSqlInjectionMatchSetResponse (Core.Maybe Core.Text)
updateSqlInjectionMatchSetResponse_changeToken = Lens.lens (\UpdateSqlInjectionMatchSetResponse' {changeToken} -> changeToken) (\s@UpdateSqlInjectionMatchSetResponse' {} a -> s {changeToken = a} :: UpdateSqlInjectionMatchSetResponse)

-- | The response's http status code.
updateSqlInjectionMatchSetResponse_httpStatus :: Lens.Lens' UpdateSqlInjectionMatchSetResponse Core.Int
updateSqlInjectionMatchSetResponse_httpStatus = Lens.lens (\UpdateSqlInjectionMatchSetResponse' {httpStatus} -> httpStatus) (\s@UpdateSqlInjectionMatchSetResponse' {} a -> s {httpStatus = a} :: UpdateSqlInjectionMatchSetResponse)

instance
  Core.NFData
    UpdateSqlInjectionMatchSetResponse
