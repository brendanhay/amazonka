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
-- Module      : Network.AWS.WAF.DeleteByteMatchSet
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
-- Permanently deletes a ByteMatchSet. You can\'t delete a @ByteMatchSet@
-- if it\'s still used in any @Rules@ or if it still includes any
-- ByteMatchTuple objects (any filters).
--
-- If you just want to remove a @ByteMatchSet@ from a @Rule@, use
-- UpdateRule.
--
-- To permanently delete a @ByteMatchSet@, perform the following steps:
--
-- 1.  Update the @ByteMatchSet@ to remove filters, if any. For more
--     information, see UpdateByteMatchSet.
--
-- 2.  Use GetChangeToken to get the change token that you provide in the
--     @ChangeToken@ parameter of a @DeleteByteMatchSet@ request.
--
-- 3.  Submit a @DeleteByteMatchSet@ request.
module Network.AWS.WAF.DeleteByteMatchSet
  ( -- * Creating a Request
    DeleteByteMatchSet (..),
    newDeleteByteMatchSet,

    -- * Request Lenses
    deleteByteMatchSet_byteMatchSetId,
    deleteByteMatchSet_changeToken,

    -- * Destructuring the Response
    DeleteByteMatchSetResponse (..),
    newDeleteByteMatchSetResponse,

    -- * Response Lenses
    deleteByteMatchSetResponse_changeToken,
    deleteByteMatchSetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | /See:/ 'newDeleteByteMatchSet' smart constructor.
data DeleteByteMatchSet = DeleteByteMatchSet'
  { -- | The @ByteMatchSetId@ of the ByteMatchSet that you want to delete.
    -- @ByteMatchSetId@ is returned by CreateByteMatchSet and by
    -- ListByteMatchSets.
    byteMatchSetId :: Core.Text,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteByteMatchSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'byteMatchSetId', 'deleteByteMatchSet_byteMatchSetId' - The @ByteMatchSetId@ of the ByteMatchSet that you want to delete.
-- @ByteMatchSetId@ is returned by CreateByteMatchSet and by
-- ListByteMatchSets.
--
-- 'changeToken', 'deleteByteMatchSet_changeToken' - The value returned by the most recent call to GetChangeToken.
newDeleteByteMatchSet ::
  -- | 'byteMatchSetId'
  Core.Text ->
  -- | 'changeToken'
  Core.Text ->
  DeleteByteMatchSet
newDeleteByteMatchSet pByteMatchSetId_ pChangeToken_ =
  DeleteByteMatchSet'
    { byteMatchSetId =
        pByteMatchSetId_,
      changeToken = pChangeToken_
    }

-- | The @ByteMatchSetId@ of the ByteMatchSet that you want to delete.
-- @ByteMatchSetId@ is returned by CreateByteMatchSet and by
-- ListByteMatchSets.
deleteByteMatchSet_byteMatchSetId :: Lens.Lens' DeleteByteMatchSet Core.Text
deleteByteMatchSet_byteMatchSetId = Lens.lens (\DeleteByteMatchSet' {byteMatchSetId} -> byteMatchSetId) (\s@DeleteByteMatchSet' {} a -> s {byteMatchSetId = a} :: DeleteByteMatchSet)

-- | The value returned by the most recent call to GetChangeToken.
deleteByteMatchSet_changeToken :: Lens.Lens' DeleteByteMatchSet Core.Text
deleteByteMatchSet_changeToken = Lens.lens (\DeleteByteMatchSet' {changeToken} -> changeToken) (\s@DeleteByteMatchSet' {} a -> s {changeToken = a} :: DeleteByteMatchSet)

instance Core.AWSRequest DeleteByteMatchSet where
  type
    AWSResponse DeleteByteMatchSet =
      DeleteByteMatchSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteByteMatchSetResponse'
            Core.<$> (x Core..?> "ChangeToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteByteMatchSet

instance Core.NFData DeleteByteMatchSet

instance Core.ToHeaders DeleteByteMatchSet where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_20150824.DeleteByteMatchSet" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteByteMatchSet where
  toJSON DeleteByteMatchSet' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ByteMatchSetId" Core..= byteMatchSetId),
            Core.Just ("ChangeToken" Core..= changeToken)
          ]
      )

instance Core.ToPath DeleteByteMatchSet where
  toPath = Core.const "/"

instance Core.ToQuery DeleteByteMatchSet where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteByteMatchSetResponse' smart constructor.
data DeleteByteMatchSetResponse = DeleteByteMatchSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @DeleteByteMatchSet@
    -- request. You can also use this value to query the status of the request.
    -- For more information, see GetChangeTokenStatus.
    changeToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteByteMatchSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeToken', 'deleteByteMatchSetResponse_changeToken' - The @ChangeToken@ that you used to submit the @DeleteByteMatchSet@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
--
-- 'httpStatus', 'deleteByteMatchSetResponse_httpStatus' - The response's http status code.
newDeleteByteMatchSetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteByteMatchSetResponse
newDeleteByteMatchSetResponse pHttpStatus_ =
  DeleteByteMatchSetResponse'
    { changeToken =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ChangeToken@ that you used to submit the @DeleteByteMatchSet@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
deleteByteMatchSetResponse_changeToken :: Lens.Lens' DeleteByteMatchSetResponse (Core.Maybe Core.Text)
deleteByteMatchSetResponse_changeToken = Lens.lens (\DeleteByteMatchSetResponse' {changeToken} -> changeToken) (\s@DeleteByteMatchSetResponse' {} a -> s {changeToken = a} :: DeleteByteMatchSetResponse)

-- | The response's http status code.
deleteByteMatchSetResponse_httpStatus :: Lens.Lens' DeleteByteMatchSetResponse Core.Int
deleteByteMatchSetResponse_httpStatus = Lens.lens (\DeleteByteMatchSetResponse' {httpStatus} -> httpStatus) (\s@DeleteByteMatchSetResponse' {} a -> s {httpStatus = a} :: DeleteByteMatchSetResponse)

instance Core.NFData DeleteByteMatchSetResponse
