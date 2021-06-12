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
-- Module      : Network.AWS.WAF.DeleteRegexPatternSet
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
-- Permanently deletes a RegexPatternSet. You can\'t delete a
-- @RegexPatternSet@ if it\'s still used in any @RegexMatchSet@ or if the
-- @RegexPatternSet@ is not empty.
module Network.AWS.WAF.DeleteRegexPatternSet
  ( -- * Creating a Request
    DeleteRegexPatternSet (..),
    newDeleteRegexPatternSet,

    -- * Request Lenses
    deleteRegexPatternSet_regexPatternSetId,
    deleteRegexPatternSet_changeToken,

    -- * Destructuring the Response
    DeleteRegexPatternSetResponse (..),
    newDeleteRegexPatternSetResponse,

    -- * Response Lenses
    deleteRegexPatternSetResponse_changeToken,
    deleteRegexPatternSetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | /See:/ 'newDeleteRegexPatternSet' smart constructor.
data DeleteRegexPatternSet = DeleteRegexPatternSet'
  { -- | The @RegexPatternSetId@ of the RegexPatternSet that you want to delete.
    -- @RegexPatternSetId@ is returned by CreateRegexPatternSet and by
    -- ListRegexPatternSets.
    regexPatternSetId :: Core.Text,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteRegexPatternSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regexPatternSetId', 'deleteRegexPatternSet_regexPatternSetId' - The @RegexPatternSetId@ of the RegexPatternSet that you want to delete.
-- @RegexPatternSetId@ is returned by CreateRegexPatternSet and by
-- ListRegexPatternSets.
--
-- 'changeToken', 'deleteRegexPatternSet_changeToken' - The value returned by the most recent call to GetChangeToken.
newDeleteRegexPatternSet ::
  -- | 'regexPatternSetId'
  Core.Text ->
  -- | 'changeToken'
  Core.Text ->
  DeleteRegexPatternSet
newDeleteRegexPatternSet
  pRegexPatternSetId_
  pChangeToken_ =
    DeleteRegexPatternSet'
      { regexPatternSetId =
          pRegexPatternSetId_,
        changeToken = pChangeToken_
      }

-- | The @RegexPatternSetId@ of the RegexPatternSet that you want to delete.
-- @RegexPatternSetId@ is returned by CreateRegexPatternSet and by
-- ListRegexPatternSets.
deleteRegexPatternSet_regexPatternSetId :: Lens.Lens' DeleteRegexPatternSet Core.Text
deleteRegexPatternSet_regexPatternSetId = Lens.lens (\DeleteRegexPatternSet' {regexPatternSetId} -> regexPatternSetId) (\s@DeleteRegexPatternSet' {} a -> s {regexPatternSetId = a} :: DeleteRegexPatternSet)

-- | The value returned by the most recent call to GetChangeToken.
deleteRegexPatternSet_changeToken :: Lens.Lens' DeleteRegexPatternSet Core.Text
deleteRegexPatternSet_changeToken = Lens.lens (\DeleteRegexPatternSet' {changeToken} -> changeToken) (\s@DeleteRegexPatternSet' {} a -> s {changeToken = a} :: DeleteRegexPatternSet)

instance Core.AWSRequest DeleteRegexPatternSet where
  type
    AWSResponse DeleteRegexPatternSet =
      DeleteRegexPatternSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRegexPatternSetResponse'
            Core.<$> (x Core..?> "ChangeToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteRegexPatternSet

instance Core.NFData DeleteRegexPatternSet

instance Core.ToHeaders DeleteRegexPatternSet where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_20150824.DeleteRegexPatternSet" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteRegexPatternSet where
  toJSON DeleteRegexPatternSet' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("RegexPatternSetId" Core..= regexPatternSetId),
            Core.Just ("ChangeToken" Core..= changeToken)
          ]
      )

instance Core.ToPath DeleteRegexPatternSet where
  toPath = Core.const "/"

instance Core.ToQuery DeleteRegexPatternSet where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteRegexPatternSetResponse' smart constructor.
data DeleteRegexPatternSetResponse = DeleteRegexPatternSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @DeleteRegexPatternSet@
    -- request. You can also use this value to query the status of the request.
    -- For more information, see GetChangeTokenStatus.
    changeToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteRegexPatternSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeToken', 'deleteRegexPatternSetResponse_changeToken' - The @ChangeToken@ that you used to submit the @DeleteRegexPatternSet@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
--
-- 'httpStatus', 'deleteRegexPatternSetResponse_httpStatus' - The response's http status code.
newDeleteRegexPatternSetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteRegexPatternSetResponse
newDeleteRegexPatternSetResponse pHttpStatus_ =
  DeleteRegexPatternSetResponse'
    { changeToken =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ChangeToken@ that you used to submit the @DeleteRegexPatternSet@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
deleteRegexPatternSetResponse_changeToken :: Lens.Lens' DeleteRegexPatternSetResponse (Core.Maybe Core.Text)
deleteRegexPatternSetResponse_changeToken = Lens.lens (\DeleteRegexPatternSetResponse' {changeToken} -> changeToken) (\s@DeleteRegexPatternSetResponse' {} a -> s {changeToken = a} :: DeleteRegexPatternSetResponse)

-- | The response's http status code.
deleteRegexPatternSetResponse_httpStatus :: Lens.Lens' DeleteRegexPatternSetResponse Core.Int
deleteRegexPatternSetResponse_httpStatus = Lens.lens (\DeleteRegexPatternSetResponse' {httpStatus} -> httpStatus) (\s@DeleteRegexPatternSetResponse' {} a -> s {httpStatus = a} :: DeleteRegexPatternSetResponse)

instance Core.NFData DeleteRegexPatternSetResponse
