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
-- Module      : Network.AWS.WAF.DeleteWebACL
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
-- Permanently deletes a WebACL. You can\'t delete a @WebACL@ if it still
-- contains any @Rules@.
--
-- To delete a @WebACL@, perform the following steps:
--
-- 1.  Update the @WebACL@ to remove @Rules@, if any. For more information,
--     see UpdateWebACL.
--
-- 2.  Use GetChangeToken to get the change token that you provide in the
--     @ChangeToken@ parameter of a @DeleteWebACL@ request.
--
-- 3.  Submit a @DeleteWebACL@ request.
module Network.AWS.WAF.DeleteWebACL
  ( -- * Creating a Request
    DeleteWebACL (..),
    newDeleteWebACL,

    -- * Request Lenses
    deleteWebACL_webACLId,
    deleteWebACL_changeToken,

    -- * Destructuring the Response
    DeleteWebACLResponse (..),
    newDeleteWebACLResponse,

    -- * Response Lenses
    deleteWebACLResponse_changeToken,
    deleteWebACLResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | /See:/ 'newDeleteWebACL' smart constructor.
data DeleteWebACL = DeleteWebACL'
  { -- | The @WebACLId@ of the WebACL that you want to delete. @WebACLId@ is
    -- returned by CreateWebACL and by ListWebACLs.
    webACLId :: Prelude.Text,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteWebACL' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'webACLId', 'deleteWebACL_webACLId' - The @WebACLId@ of the WebACL that you want to delete. @WebACLId@ is
-- returned by CreateWebACL and by ListWebACLs.
--
-- 'changeToken', 'deleteWebACL_changeToken' - The value returned by the most recent call to GetChangeToken.
newDeleteWebACL ::
  -- | 'webACLId'
  Prelude.Text ->
  -- | 'changeToken'
  Prelude.Text ->
  DeleteWebACL
newDeleteWebACL pWebACLId_ pChangeToken_ =
  DeleteWebACL'
    { webACLId = pWebACLId_,
      changeToken = pChangeToken_
    }

-- | The @WebACLId@ of the WebACL that you want to delete. @WebACLId@ is
-- returned by CreateWebACL and by ListWebACLs.
deleteWebACL_webACLId :: Lens.Lens' DeleteWebACL Prelude.Text
deleteWebACL_webACLId = Lens.lens (\DeleteWebACL' {webACLId} -> webACLId) (\s@DeleteWebACL' {} a -> s {webACLId = a} :: DeleteWebACL)

-- | The value returned by the most recent call to GetChangeToken.
deleteWebACL_changeToken :: Lens.Lens' DeleteWebACL Prelude.Text
deleteWebACL_changeToken = Lens.lens (\DeleteWebACL' {changeToken} -> changeToken) (\s@DeleteWebACL' {} a -> s {changeToken = a} :: DeleteWebACL)

instance Prelude.AWSRequest DeleteWebACL where
  type Rs DeleteWebACL = DeleteWebACLResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteWebACLResponse'
            Prelude.<$> (x Prelude..?> "ChangeToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteWebACL

instance Prelude.NFData DeleteWebACL

instance Prelude.ToHeaders DeleteWebACL where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSWAF_20150824.DeleteWebACL" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteWebACL where
  toJSON DeleteWebACL' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("WebACLId" Prelude..= webACLId),
            Prelude.Just ("ChangeToken" Prelude..= changeToken)
          ]
      )

instance Prelude.ToPath DeleteWebACL where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteWebACL where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteWebACLResponse' smart constructor.
data DeleteWebACLResponse = DeleteWebACLResponse'
  { -- | The @ChangeToken@ that you used to submit the @DeleteWebACL@ request.
    -- You can also use this value to query the status of the request. For more
    -- information, see GetChangeTokenStatus.
    changeToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteWebACLResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeToken', 'deleteWebACLResponse_changeToken' - The @ChangeToken@ that you used to submit the @DeleteWebACL@ request.
-- You can also use this value to query the status of the request. For more
-- information, see GetChangeTokenStatus.
--
-- 'httpStatus', 'deleteWebACLResponse_httpStatus' - The response's http status code.
newDeleteWebACLResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteWebACLResponse
newDeleteWebACLResponse pHttpStatus_ =
  DeleteWebACLResponse'
    { changeToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ChangeToken@ that you used to submit the @DeleteWebACL@ request.
-- You can also use this value to query the status of the request. For more
-- information, see GetChangeTokenStatus.
deleteWebACLResponse_changeToken :: Lens.Lens' DeleteWebACLResponse (Prelude.Maybe Prelude.Text)
deleteWebACLResponse_changeToken = Lens.lens (\DeleteWebACLResponse' {changeToken} -> changeToken) (\s@DeleteWebACLResponse' {} a -> s {changeToken = a} :: DeleteWebACLResponse)

-- | The response's http status code.
deleteWebACLResponse_httpStatus :: Lens.Lens' DeleteWebACLResponse Prelude.Int
deleteWebACLResponse_httpStatus = Lens.lens (\DeleteWebACLResponse' {httpStatus} -> httpStatus) (\s@DeleteWebACLResponse' {} a -> s {httpStatus = a} :: DeleteWebACLResponse)

instance Prelude.NFData DeleteWebACLResponse
