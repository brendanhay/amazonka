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
-- Module      : Network.AWS.WAF.DeleteRegexMatchSet
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
-- Permanently deletes a RegexMatchSet. You can\'t delete a @RegexMatchSet@
-- if it\'s still used in any @Rules@ or if it still includes any
-- @RegexMatchTuples@ objects (any filters).
--
-- If you just want to remove a @RegexMatchSet@ from a @Rule@, use
-- UpdateRule.
--
-- To permanently delete a @RegexMatchSet@, perform the following steps:
--
-- 1.  Update the @RegexMatchSet@ to remove filters, if any. For more
--     information, see UpdateRegexMatchSet.
--
-- 2.  Use GetChangeToken to get the change token that you provide in the
--     @ChangeToken@ parameter of a @DeleteRegexMatchSet@ request.
--
-- 3.  Submit a @DeleteRegexMatchSet@ request.
module Network.AWS.WAF.DeleteRegexMatchSet
  ( -- * Creating a Request
    DeleteRegexMatchSet (..),
    newDeleteRegexMatchSet,

    -- * Request Lenses
    deleteRegexMatchSet_regexMatchSetId,
    deleteRegexMatchSet_changeToken,

    -- * Destructuring the Response
    DeleteRegexMatchSetResponse (..),
    newDeleteRegexMatchSetResponse,

    -- * Response Lenses
    deleteRegexMatchSetResponse_changeToken,
    deleteRegexMatchSetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | /See:/ 'newDeleteRegexMatchSet' smart constructor.
data DeleteRegexMatchSet = DeleteRegexMatchSet'
  { -- | The @RegexMatchSetId@ of the RegexMatchSet that you want to delete.
    -- @RegexMatchSetId@ is returned by CreateRegexMatchSet and by
    -- ListRegexMatchSets.
    regexMatchSetId :: Core.Text,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteRegexMatchSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regexMatchSetId', 'deleteRegexMatchSet_regexMatchSetId' - The @RegexMatchSetId@ of the RegexMatchSet that you want to delete.
-- @RegexMatchSetId@ is returned by CreateRegexMatchSet and by
-- ListRegexMatchSets.
--
-- 'changeToken', 'deleteRegexMatchSet_changeToken' - The value returned by the most recent call to GetChangeToken.
newDeleteRegexMatchSet ::
  -- | 'regexMatchSetId'
  Core.Text ->
  -- | 'changeToken'
  Core.Text ->
  DeleteRegexMatchSet
newDeleteRegexMatchSet
  pRegexMatchSetId_
  pChangeToken_ =
    DeleteRegexMatchSet'
      { regexMatchSetId =
          pRegexMatchSetId_,
        changeToken = pChangeToken_
      }

-- | The @RegexMatchSetId@ of the RegexMatchSet that you want to delete.
-- @RegexMatchSetId@ is returned by CreateRegexMatchSet and by
-- ListRegexMatchSets.
deleteRegexMatchSet_regexMatchSetId :: Lens.Lens' DeleteRegexMatchSet Core.Text
deleteRegexMatchSet_regexMatchSetId = Lens.lens (\DeleteRegexMatchSet' {regexMatchSetId} -> regexMatchSetId) (\s@DeleteRegexMatchSet' {} a -> s {regexMatchSetId = a} :: DeleteRegexMatchSet)

-- | The value returned by the most recent call to GetChangeToken.
deleteRegexMatchSet_changeToken :: Lens.Lens' DeleteRegexMatchSet Core.Text
deleteRegexMatchSet_changeToken = Lens.lens (\DeleteRegexMatchSet' {changeToken} -> changeToken) (\s@DeleteRegexMatchSet' {} a -> s {changeToken = a} :: DeleteRegexMatchSet)

instance Core.AWSRequest DeleteRegexMatchSet where
  type
    AWSResponse DeleteRegexMatchSet =
      DeleteRegexMatchSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRegexMatchSetResponse'
            Core.<$> (x Core..?> "ChangeToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteRegexMatchSet

instance Core.NFData DeleteRegexMatchSet

instance Core.ToHeaders DeleteRegexMatchSet where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_20150824.DeleteRegexMatchSet" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteRegexMatchSet where
  toJSON DeleteRegexMatchSet' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("RegexMatchSetId" Core..= regexMatchSetId),
            Core.Just ("ChangeToken" Core..= changeToken)
          ]
      )

instance Core.ToPath DeleteRegexMatchSet where
  toPath = Core.const "/"

instance Core.ToQuery DeleteRegexMatchSet where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteRegexMatchSetResponse' smart constructor.
data DeleteRegexMatchSetResponse = DeleteRegexMatchSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @DeleteRegexMatchSet@
    -- request. You can also use this value to query the status of the request.
    -- For more information, see GetChangeTokenStatus.
    changeToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteRegexMatchSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeToken', 'deleteRegexMatchSetResponse_changeToken' - The @ChangeToken@ that you used to submit the @DeleteRegexMatchSet@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
--
-- 'httpStatus', 'deleteRegexMatchSetResponse_httpStatus' - The response's http status code.
newDeleteRegexMatchSetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteRegexMatchSetResponse
newDeleteRegexMatchSetResponse pHttpStatus_ =
  DeleteRegexMatchSetResponse'
    { changeToken =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ChangeToken@ that you used to submit the @DeleteRegexMatchSet@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
deleteRegexMatchSetResponse_changeToken :: Lens.Lens' DeleteRegexMatchSetResponse (Core.Maybe Core.Text)
deleteRegexMatchSetResponse_changeToken = Lens.lens (\DeleteRegexMatchSetResponse' {changeToken} -> changeToken) (\s@DeleteRegexMatchSetResponse' {} a -> s {changeToken = a} :: DeleteRegexMatchSetResponse)

-- | The response's http status code.
deleteRegexMatchSetResponse_httpStatus :: Lens.Lens' DeleteRegexMatchSetResponse Core.Int
deleteRegexMatchSetResponse_httpStatus = Lens.lens (\DeleteRegexMatchSetResponse' {httpStatus} -> httpStatus) (\s@DeleteRegexMatchSetResponse' {} a -> s {httpStatus = a} :: DeleteRegexMatchSetResponse)

instance Core.NFData DeleteRegexMatchSetResponse
