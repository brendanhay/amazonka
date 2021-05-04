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
-- Module      : Network.AWS.WAF.DeleteXssMatchSet
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
-- Permanently deletes an XssMatchSet. You can\'t delete an @XssMatchSet@
-- if it\'s still used in any @Rules@ or if it still contains any
-- XssMatchTuple objects.
--
-- If you just want to remove an @XssMatchSet@ from a @Rule@, use
-- UpdateRule.
--
-- To permanently delete an @XssMatchSet@ from AWS WAF, perform the
-- following steps:
--
-- 1.  Update the @XssMatchSet@ to remove filters, if any. For more
--     information, see UpdateXssMatchSet.
--
-- 2.  Use GetChangeToken to get the change token that you provide in the
--     @ChangeToken@ parameter of a @DeleteXssMatchSet@ request.
--
-- 3.  Submit a @DeleteXssMatchSet@ request.
module Network.AWS.WAF.DeleteXssMatchSet
  ( -- * Creating a Request
    DeleteXssMatchSet (..),
    newDeleteXssMatchSet,

    -- * Request Lenses
    deleteXssMatchSet_xssMatchSetId,
    deleteXssMatchSet_changeToken,

    -- * Destructuring the Response
    DeleteXssMatchSetResponse (..),
    newDeleteXssMatchSetResponse,

    -- * Response Lenses
    deleteXssMatchSetResponse_changeToken,
    deleteXssMatchSetResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | A request to delete an XssMatchSet from AWS WAF.
--
-- /See:/ 'newDeleteXssMatchSet' smart constructor.
data DeleteXssMatchSet = DeleteXssMatchSet'
  { -- | The @XssMatchSetId@ of the XssMatchSet that you want to delete.
    -- @XssMatchSetId@ is returned by CreateXssMatchSet and by
    -- ListXssMatchSets.
    xssMatchSetId :: Prelude.Text,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteXssMatchSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'xssMatchSetId', 'deleteXssMatchSet_xssMatchSetId' - The @XssMatchSetId@ of the XssMatchSet that you want to delete.
-- @XssMatchSetId@ is returned by CreateXssMatchSet and by
-- ListXssMatchSets.
--
-- 'changeToken', 'deleteXssMatchSet_changeToken' - The value returned by the most recent call to GetChangeToken.
newDeleteXssMatchSet ::
  -- | 'xssMatchSetId'
  Prelude.Text ->
  -- | 'changeToken'
  Prelude.Text ->
  DeleteXssMatchSet
newDeleteXssMatchSet pXssMatchSetId_ pChangeToken_ =
  DeleteXssMatchSet'
    { xssMatchSetId = pXssMatchSetId_,
      changeToken = pChangeToken_
    }

-- | The @XssMatchSetId@ of the XssMatchSet that you want to delete.
-- @XssMatchSetId@ is returned by CreateXssMatchSet and by
-- ListXssMatchSets.
deleteXssMatchSet_xssMatchSetId :: Lens.Lens' DeleteXssMatchSet Prelude.Text
deleteXssMatchSet_xssMatchSetId = Lens.lens (\DeleteXssMatchSet' {xssMatchSetId} -> xssMatchSetId) (\s@DeleteXssMatchSet' {} a -> s {xssMatchSetId = a} :: DeleteXssMatchSet)

-- | The value returned by the most recent call to GetChangeToken.
deleteXssMatchSet_changeToken :: Lens.Lens' DeleteXssMatchSet Prelude.Text
deleteXssMatchSet_changeToken = Lens.lens (\DeleteXssMatchSet' {changeToken} -> changeToken) (\s@DeleteXssMatchSet' {} a -> s {changeToken = a} :: DeleteXssMatchSet)

instance Prelude.AWSRequest DeleteXssMatchSet where
  type Rs DeleteXssMatchSet = DeleteXssMatchSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteXssMatchSetResponse'
            Prelude.<$> (x Prelude..?> "ChangeToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteXssMatchSet

instance Prelude.NFData DeleteXssMatchSet

instance Prelude.ToHeaders DeleteXssMatchSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSWAF_20150824.DeleteXssMatchSet" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteXssMatchSet where
  toJSON DeleteXssMatchSet' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("XssMatchSetId" Prelude..= xssMatchSetId),
            Prelude.Just ("ChangeToken" Prelude..= changeToken)
          ]
      )

instance Prelude.ToPath DeleteXssMatchSet where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteXssMatchSet where
  toQuery = Prelude.const Prelude.mempty

-- | The response to a request to delete an XssMatchSet from AWS WAF.
--
-- /See:/ 'newDeleteXssMatchSetResponse' smart constructor.
data DeleteXssMatchSetResponse = DeleteXssMatchSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @DeleteXssMatchSet@
    -- request. You can also use this value to query the status of the request.
    -- For more information, see GetChangeTokenStatus.
    changeToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteXssMatchSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeToken', 'deleteXssMatchSetResponse_changeToken' - The @ChangeToken@ that you used to submit the @DeleteXssMatchSet@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
--
-- 'httpStatus', 'deleteXssMatchSetResponse_httpStatus' - The response's http status code.
newDeleteXssMatchSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteXssMatchSetResponse
newDeleteXssMatchSetResponse pHttpStatus_ =
  DeleteXssMatchSetResponse'
    { changeToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ChangeToken@ that you used to submit the @DeleteXssMatchSet@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
deleteXssMatchSetResponse_changeToken :: Lens.Lens' DeleteXssMatchSetResponse (Prelude.Maybe Prelude.Text)
deleteXssMatchSetResponse_changeToken = Lens.lens (\DeleteXssMatchSetResponse' {changeToken} -> changeToken) (\s@DeleteXssMatchSetResponse' {} a -> s {changeToken = a} :: DeleteXssMatchSetResponse)

-- | The response's http status code.
deleteXssMatchSetResponse_httpStatus :: Lens.Lens' DeleteXssMatchSetResponse Prelude.Int
deleteXssMatchSetResponse_httpStatus = Lens.lens (\DeleteXssMatchSetResponse' {httpStatus} -> httpStatus) (\s@DeleteXssMatchSetResponse' {} a -> s {httpStatus = a} :: DeleteXssMatchSetResponse)

instance Prelude.NFData DeleteXssMatchSetResponse
