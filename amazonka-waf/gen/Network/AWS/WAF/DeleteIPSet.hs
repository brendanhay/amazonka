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
-- Module      : Network.AWS.WAF.DeleteIPSet
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
-- Permanently deletes an IPSet. You can\'t delete an @IPSet@ if it\'s
-- still used in any @Rules@ or if it still includes any IP addresses.
--
-- If you just want to remove an @IPSet@ from a @Rule@, use UpdateRule.
--
-- To permanently delete an @IPSet@ from AWS WAF, perform the following
-- steps:
--
-- 1.  Update the @IPSet@ to remove IP address ranges, if any. For more
--     information, see UpdateIPSet.
--
-- 2.  Use GetChangeToken to get the change token that you provide in the
--     @ChangeToken@ parameter of a @DeleteIPSet@ request.
--
-- 3.  Submit a @DeleteIPSet@ request.
module Network.AWS.WAF.DeleteIPSet
  ( -- * Creating a Request
    DeleteIPSet (..),
    newDeleteIPSet,

    -- * Request Lenses
    deleteIPSet_iPSetId,
    deleteIPSet_changeToken,

    -- * Destructuring the Response
    DeleteIPSetResponse (..),
    newDeleteIPSetResponse,

    -- * Response Lenses
    deleteIPSetResponse_changeToken,
    deleteIPSetResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | /See:/ 'newDeleteIPSet' smart constructor.
data DeleteIPSet = DeleteIPSet'
  { -- | The @IPSetId@ of the IPSet that you want to delete. @IPSetId@ is
    -- returned by CreateIPSet and by ListIPSets.
    iPSetId :: Prelude.Text,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteIPSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iPSetId', 'deleteIPSet_iPSetId' - The @IPSetId@ of the IPSet that you want to delete. @IPSetId@ is
-- returned by CreateIPSet and by ListIPSets.
--
-- 'changeToken', 'deleteIPSet_changeToken' - The value returned by the most recent call to GetChangeToken.
newDeleteIPSet ::
  -- | 'iPSetId'
  Prelude.Text ->
  -- | 'changeToken'
  Prelude.Text ->
  DeleteIPSet
newDeleteIPSet pIPSetId_ pChangeToken_ =
  DeleteIPSet'
    { iPSetId = pIPSetId_,
      changeToken = pChangeToken_
    }

-- | The @IPSetId@ of the IPSet that you want to delete. @IPSetId@ is
-- returned by CreateIPSet and by ListIPSets.
deleteIPSet_iPSetId :: Lens.Lens' DeleteIPSet Prelude.Text
deleteIPSet_iPSetId = Lens.lens (\DeleteIPSet' {iPSetId} -> iPSetId) (\s@DeleteIPSet' {} a -> s {iPSetId = a} :: DeleteIPSet)

-- | The value returned by the most recent call to GetChangeToken.
deleteIPSet_changeToken :: Lens.Lens' DeleteIPSet Prelude.Text
deleteIPSet_changeToken = Lens.lens (\DeleteIPSet' {changeToken} -> changeToken) (\s@DeleteIPSet' {} a -> s {changeToken = a} :: DeleteIPSet)

instance Prelude.AWSRequest DeleteIPSet where
  type Rs DeleteIPSet = DeleteIPSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteIPSetResponse'
            Prelude.<$> (x Prelude..?> "ChangeToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteIPSet

instance Prelude.NFData DeleteIPSet

instance Prelude.ToHeaders DeleteIPSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSWAF_20150824.DeleteIPSet" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteIPSet where
  toJSON DeleteIPSet' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("IPSetId" Prelude..= iPSetId),
            Prelude.Just ("ChangeToken" Prelude..= changeToken)
          ]
      )

instance Prelude.ToPath DeleteIPSet where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteIPSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteIPSetResponse' smart constructor.
data DeleteIPSetResponse = DeleteIPSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @DeleteIPSet@ request. You
    -- can also use this value to query the status of the request. For more
    -- information, see GetChangeTokenStatus.
    changeToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteIPSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeToken', 'deleteIPSetResponse_changeToken' - The @ChangeToken@ that you used to submit the @DeleteIPSet@ request. You
-- can also use this value to query the status of the request. For more
-- information, see GetChangeTokenStatus.
--
-- 'httpStatus', 'deleteIPSetResponse_httpStatus' - The response's http status code.
newDeleteIPSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteIPSetResponse
newDeleteIPSetResponse pHttpStatus_ =
  DeleteIPSetResponse'
    { changeToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ChangeToken@ that you used to submit the @DeleteIPSet@ request. You
-- can also use this value to query the status of the request. For more
-- information, see GetChangeTokenStatus.
deleteIPSetResponse_changeToken :: Lens.Lens' DeleteIPSetResponse (Prelude.Maybe Prelude.Text)
deleteIPSetResponse_changeToken = Lens.lens (\DeleteIPSetResponse' {changeToken} -> changeToken) (\s@DeleteIPSetResponse' {} a -> s {changeToken = a} :: DeleteIPSetResponse)

-- | The response's http status code.
deleteIPSetResponse_httpStatus :: Lens.Lens' DeleteIPSetResponse Prelude.Int
deleteIPSetResponse_httpStatus = Lens.lens (\DeleteIPSetResponse' {httpStatus} -> httpStatus) (\s@DeleteIPSetResponse' {} a -> s {httpStatus = a} :: DeleteIPSetResponse)

instance Prelude.NFData DeleteIPSetResponse
