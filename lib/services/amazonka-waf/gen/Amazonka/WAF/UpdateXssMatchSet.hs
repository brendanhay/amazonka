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
-- Module      : Amazonka.WAF.UpdateXssMatchSet
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- Inserts or deletes XssMatchTuple objects (filters) in an XssMatchSet.
-- For each @XssMatchTuple@ object, you specify the following values:
--
-- -   @Action@: Whether to insert the object into or delete the object
--     from the array. To change an @XssMatchTuple@, you delete the
--     existing object and add a new one.
--
-- -   @FieldToMatch@: The part of web requests that you want AWS WAF to
--     inspect and, if you want AWS WAF to inspect a header or custom query
--     parameter, the name of the header or parameter.
--
-- -   @TextTransformation@: Which text transformation, if any, to perform
--     on the web request before inspecting the request for cross-site
--     scripting attacks.
--
--     You can only specify a single type of TextTransformation.
--
-- You use @XssMatchSet@ objects to specify which CloudFront requests that
-- you want to allow, block, or count. For example, if you\'re receiving
-- requests that contain cross-site scripting attacks in the request body
-- and you want to block the requests, you can create an @XssMatchSet@ with
-- the applicable settings, and then configure AWS WAF to block the
-- requests.
--
-- To create and configure an @XssMatchSet@, perform the following steps:
--
-- 1.  Submit a CreateXssMatchSet request.
--
-- 2.  Use GetChangeToken to get the change token that you provide in the
--     @ChangeToken@ parameter of an UpdateIPSet request.
--
-- 3.  Submit an @UpdateXssMatchSet@ request to specify the parts of web
--     requests that you want AWS WAF to inspect for cross-site scripting
--     attacks.
--
-- For more information about how to use the AWS WAF API to allow or block
-- HTTP requests, see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide>.
module Amazonka.WAF.UpdateXssMatchSet
  ( -- * Creating a Request
    UpdateXssMatchSet (..),
    newUpdateXssMatchSet,

    -- * Request Lenses
    updateXssMatchSet_xssMatchSetId,
    updateXssMatchSet_changeToken,
    updateXssMatchSet_updates,

    -- * Destructuring the Response
    UpdateXssMatchSetResponse (..),
    newUpdateXssMatchSetResponse,

    -- * Response Lenses
    updateXssMatchSetResponse_changeToken,
    updateXssMatchSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAF.Types

-- | A request to update an XssMatchSet.
--
-- /See:/ 'newUpdateXssMatchSet' smart constructor.
data UpdateXssMatchSet = UpdateXssMatchSet'
  { -- | The @XssMatchSetId@ of the @XssMatchSet@ that you want to update.
    -- @XssMatchSetId@ is returned by CreateXssMatchSet and by
    -- ListXssMatchSets.
    xssMatchSetId :: Prelude.Text,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Prelude.Text,
    -- | An array of @XssMatchSetUpdate@ objects that you want to insert into or
    -- delete from an XssMatchSet. For more information, see the applicable
    -- data types:
    --
    -- -   XssMatchSetUpdate: Contains @Action@ and @XssMatchTuple@
    --
    -- -   XssMatchTuple: Contains @FieldToMatch@ and @TextTransformation@
    --
    -- -   FieldToMatch: Contains @Data@ and @Type@
    updates :: Prelude.NonEmpty XssMatchSetUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateXssMatchSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'xssMatchSetId', 'updateXssMatchSet_xssMatchSetId' - The @XssMatchSetId@ of the @XssMatchSet@ that you want to update.
-- @XssMatchSetId@ is returned by CreateXssMatchSet and by
-- ListXssMatchSets.
--
-- 'changeToken', 'updateXssMatchSet_changeToken' - The value returned by the most recent call to GetChangeToken.
--
-- 'updates', 'updateXssMatchSet_updates' - An array of @XssMatchSetUpdate@ objects that you want to insert into or
-- delete from an XssMatchSet. For more information, see the applicable
-- data types:
--
-- -   XssMatchSetUpdate: Contains @Action@ and @XssMatchTuple@
--
-- -   XssMatchTuple: Contains @FieldToMatch@ and @TextTransformation@
--
-- -   FieldToMatch: Contains @Data@ and @Type@
newUpdateXssMatchSet ::
  -- | 'xssMatchSetId'
  Prelude.Text ->
  -- | 'changeToken'
  Prelude.Text ->
  -- | 'updates'
  Prelude.NonEmpty XssMatchSetUpdate ->
  UpdateXssMatchSet
newUpdateXssMatchSet
  pXssMatchSetId_
  pChangeToken_
  pUpdates_ =
    UpdateXssMatchSet'
      { xssMatchSetId = pXssMatchSetId_,
        changeToken = pChangeToken_,
        updates = Lens.coerced Lens.# pUpdates_
      }

-- | The @XssMatchSetId@ of the @XssMatchSet@ that you want to update.
-- @XssMatchSetId@ is returned by CreateXssMatchSet and by
-- ListXssMatchSets.
updateXssMatchSet_xssMatchSetId :: Lens.Lens' UpdateXssMatchSet Prelude.Text
updateXssMatchSet_xssMatchSetId = Lens.lens (\UpdateXssMatchSet' {xssMatchSetId} -> xssMatchSetId) (\s@UpdateXssMatchSet' {} a -> s {xssMatchSetId = a} :: UpdateXssMatchSet)

-- | The value returned by the most recent call to GetChangeToken.
updateXssMatchSet_changeToken :: Lens.Lens' UpdateXssMatchSet Prelude.Text
updateXssMatchSet_changeToken = Lens.lens (\UpdateXssMatchSet' {changeToken} -> changeToken) (\s@UpdateXssMatchSet' {} a -> s {changeToken = a} :: UpdateXssMatchSet)

-- | An array of @XssMatchSetUpdate@ objects that you want to insert into or
-- delete from an XssMatchSet. For more information, see the applicable
-- data types:
--
-- -   XssMatchSetUpdate: Contains @Action@ and @XssMatchTuple@
--
-- -   XssMatchTuple: Contains @FieldToMatch@ and @TextTransformation@
--
-- -   FieldToMatch: Contains @Data@ and @Type@
updateXssMatchSet_updates :: Lens.Lens' UpdateXssMatchSet (Prelude.NonEmpty XssMatchSetUpdate)
updateXssMatchSet_updates = Lens.lens (\UpdateXssMatchSet' {updates} -> updates) (\s@UpdateXssMatchSet' {} a -> s {updates = a} :: UpdateXssMatchSet) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateXssMatchSet where
  type
    AWSResponse UpdateXssMatchSet =
      UpdateXssMatchSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateXssMatchSetResponse'
            Prelude.<$> (x Data..?> "ChangeToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateXssMatchSet where
  hashWithSalt _salt UpdateXssMatchSet' {..} =
    _salt `Prelude.hashWithSalt` xssMatchSetId
      `Prelude.hashWithSalt` changeToken
      `Prelude.hashWithSalt` updates

instance Prelude.NFData UpdateXssMatchSet where
  rnf UpdateXssMatchSet' {..} =
    Prelude.rnf xssMatchSetId
      `Prelude.seq` Prelude.rnf changeToken
      `Prelude.seq` Prelude.rnf updates

instance Data.ToHeaders UpdateXssMatchSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20150824.UpdateXssMatchSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateXssMatchSet where
  toJSON UpdateXssMatchSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("XssMatchSetId" Data..= xssMatchSetId),
            Prelude.Just ("ChangeToken" Data..= changeToken),
            Prelude.Just ("Updates" Data..= updates)
          ]
      )

instance Data.ToPath UpdateXssMatchSet where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateXssMatchSet where
  toQuery = Prelude.const Prelude.mempty

-- | The response to an UpdateXssMatchSets request.
--
-- /See:/ 'newUpdateXssMatchSetResponse' smart constructor.
data UpdateXssMatchSetResponse = UpdateXssMatchSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @UpdateXssMatchSet@
    -- request. You can also use this value to query the status of the request.
    -- For more information, see GetChangeTokenStatus.
    changeToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateXssMatchSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeToken', 'updateXssMatchSetResponse_changeToken' - The @ChangeToken@ that you used to submit the @UpdateXssMatchSet@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
--
-- 'httpStatus', 'updateXssMatchSetResponse_httpStatus' - The response's http status code.
newUpdateXssMatchSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateXssMatchSetResponse
newUpdateXssMatchSetResponse pHttpStatus_ =
  UpdateXssMatchSetResponse'
    { changeToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ChangeToken@ that you used to submit the @UpdateXssMatchSet@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
updateXssMatchSetResponse_changeToken :: Lens.Lens' UpdateXssMatchSetResponse (Prelude.Maybe Prelude.Text)
updateXssMatchSetResponse_changeToken = Lens.lens (\UpdateXssMatchSetResponse' {changeToken} -> changeToken) (\s@UpdateXssMatchSetResponse' {} a -> s {changeToken = a} :: UpdateXssMatchSetResponse)

-- | The response's http status code.
updateXssMatchSetResponse_httpStatus :: Lens.Lens' UpdateXssMatchSetResponse Prelude.Int
updateXssMatchSetResponse_httpStatus = Lens.lens (\UpdateXssMatchSetResponse' {httpStatus} -> httpStatus) (\s@UpdateXssMatchSetResponse' {} a -> s {httpStatus = a} :: UpdateXssMatchSetResponse)

instance Prelude.NFData UpdateXssMatchSetResponse where
  rnf UpdateXssMatchSetResponse' {..} =
    Prelude.rnf changeToken
      `Prelude.seq` Prelude.rnf httpStatus
