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
-- Module      : Amazonka.WAFRegional.DeleteSizeConstraintSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
-- Permanently deletes a SizeConstraintSet. You can\'t delete a
-- @SizeConstraintSet@ if it\'s still used in any @Rules@ or if it still
-- includes any SizeConstraint objects (any filters).
--
-- If you just want to remove a @SizeConstraintSet@ from a @Rule@, use
-- UpdateRule.
--
-- To permanently delete a @SizeConstraintSet@, perform the following
-- steps:
--
-- 1.  Update the @SizeConstraintSet@ to remove filters, if any. For more
--     information, see UpdateSizeConstraintSet.
--
-- 2.  Use GetChangeToken to get the change token that you provide in the
--     @ChangeToken@ parameter of a @DeleteSizeConstraintSet@ request.
--
-- 3.  Submit a @DeleteSizeConstraintSet@ request.
module Amazonka.WAFRegional.DeleteSizeConstraintSet
  ( -- * Creating a Request
    DeleteSizeConstraintSet (..),
    newDeleteSizeConstraintSet,

    -- * Request Lenses
    deleteSizeConstraintSet_sizeConstraintSetId,
    deleteSizeConstraintSet_changeToken,

    -- * Destructuring the Response
    DeleteSizeConstraintSetResponse (..),
    newDeleteSizeConstraintSetResponse,

    -- * Response Lenses
    deleteSizeConstraintSetResponse_changeToken,
    deleteSizeConstraintSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFRegional.Types

-- | /See:/ 'newDeleteSizeConstraintSet' smart constructor.
data DeleteSizeConstraintSet = DeleteSizeConstraintSet'
  { -- | The @SizeConstraintSetId@ of the SizeConstraintSet that you want to
    -- delete. @SizeConstraintSetId@ is returned by CreateSizeConstraintSet and
    -- by ListSizeConstraintSets.
    sizeConstraintSetId :: Prelude.Text,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSizeConstraintSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sizeConstraintSetId', 'deleteSizeConstraintSet_sizeConstraintSetId' - The @SizeConstraintSetId@ of the SizeConstraintSet that you want to
-- delete. @SizeConstraintSetId@ is returned by CreateSizeConstraintSet and
-- by ListSizeConstraintSets.
--
-- 'changeToken', 'deleteSizeConstraintSet_changeToken' - The value returned by the most recent call to GetChangeToken.
newDeleteSizeConstraintSet ::
  -- | 'sizeConstraintSetId'
  Prelude.Text ->
  -- | 'changeToken'
  Prelude.Text ->
  DeleteSizeConstraintSet
newDeleteSizeConstraintSet
  pSizeConstraintSetId_
  pChangeToken_ =
    DeleteSizeConstraintSet'
      { sizeConstraintSetId =
          pSizeConstraintSetId_,
        changeToken = pChangeToken_
      }

-- | The @SizeConstraintSetId@ of the SizeConstraintSet that you want to
-- delete. @SizeConstraintSetId@ is returned by CreateSizeConstraintSet and
-- by ListSizeConstraintSets.
deleteSizeConstraintSet_sizeConstraintSetId :: Lens.Lens' DeleteSizeConstraintSet Prelude.Text
deleteSizeConstraintSet_sizeConstraintSetId = Lens.lens (\DeleteSizeConstraintSet' {sizeConstraintSetId} -> sizeConstraintSetId) (\s@DeleteSizeConstraintSet' {} a -> s {sizeConstraintSetId = a} :: DeleteSizeConstraintSet)

-- | The value returned by the most recent call to GetChangeToken.
deleteSizeConstraintSet_changeToken :: Lens.Lens' DeleteSizeConstraintSet Prelude.Text
deleteSizeConstraintSet_changeToken = Lens.lens (\DeleteSizeConstraintSet' {changeToken} -> changeToken) (\s@DeleteSizeConstraintSet' {} a -> s {changeToken = a} :: DeleteSizeConstraintSet)

instance Core.AWSRequest DeleteSizeConstraintSet where
  type
    AWSResponse DeleteSizeConstraintSet =
      DeleteSizeConstraintSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSizeConstraintSetResponse'
            Prelude.<$> (x Data..?> "ChangeToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSizeConstraintSet where
  hashWithSalt _salt DeleteSizeConstraintSet' {..} =
    _salt
      `Prelude.hashWithSalt` sizeConstraintSetId
      `Prelude.hashWithSalt` changeToken

instance Prelude.NFData DeleteSizeConstraintSet where
  rnf DeleteSizeConstraintSet' {..} =
    Prelude.rnf sizeConstraintSetId
      `Prelude.seq` Prelude.rnf changeToken

instance Data.ToHeaders DeleteSizeConstraintSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_Regional_20161128.DeleteSizeConstraintSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteSizeConstraintSet where
  toJSON DeleteSizeConstraintSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SizeConstraintSetId" Data..= sizeConstraintSetId),
            Prelude.Just ("ChangeToken" Data..= changeToken)
          ]
      )

instance Data.ToPath DeleteSizeConstraintSet where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteSizeConstraintSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSizeConstraintSetResponse' smart constructor.
data DeleteSizeConstraintSetResponse = DeleteSizeConstraintSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @DeleteSizeConstraintSet@
    -- request. You can also use this value to query the status of the request.
    -- For more information, see GetChangeTokenStatus.
    changeToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSizeConstraintSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeToken', 'deleteSizeConstraintSetResponse_changeToken' - The @ChangeToken@ that you used to submit the @DeleteSizeConstraintSet@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
--
-- 'httpStatus', 'deleteSizeConstraintSetResponse_httpStatus' - The response's http status code.
newDeleteSizeConstraintSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSizeConstraintSetResponse
newDeleteSizeConstraintSetResponse pHttpStatus_ =
  DeleteSizeConstraintSetResponse'
    { changeToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ChangeToken@ that you used to submit the @DeleteSizeConstraintSet@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
deleteSizeConstraintSetResponse_changeToken :: Lens.Lens' DeleteSizeConstraintSetResponse (Prelude.Maybe Prelude.Text)
deleteSizeConstraintSetResponse_changeToken = Lens.lens (\DeleteSizeConstraintSetResponse' {changeToken} -> changeToken) (\s@DeleteSizeConstraintSetResponse' {} a -> s {changeToken = a} :: DeleteSizeConstraintSetResponse)

-- | The response's http status code.
deleteSizeConstraintSetResponse_httpStatus :: Lens.Lens' DeleteSizeConstraintSetResponse Prelude.Int
deleteSizeConstraintSetResponse_httpStatus = Lens.lens (\DeleteSizeConstraintSetResponse' {httpStatus} -> httpStatus) (\s@DeleteSizeConstraintSetResponse' {} a -> s {httpStatus = a} :: DeleteSizeConstraintSetResponse)

instance
  Prelude.NFData
    DeleteSizeConstraintSetResponse
  where
  rnf DeleteSizeConstraintSetResponse' {..} =
    Prelude.rnf changeToken
      `Prelude.seq` Prelude.rnf httpStatus
