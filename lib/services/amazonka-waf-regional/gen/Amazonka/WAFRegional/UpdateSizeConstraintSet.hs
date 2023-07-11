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
-- Module      : Amazonka.WAFRegional.UpdateSizeConstraintSet
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
-- Inserts or deletes SizeConstraint objects (filters) in a
-- SizeConstraintSet. For each @SizeConstraint@ object, you specify the
-- following values:
--
-- -   Whether to insert or delete the object from the array. If you want
--     to change a @SizeConstraintSetUpdate@ object, you delete the
--     existing object and add a new one.
--
-- -   The part of a web request that you want AWS WAF to evaluate, such as
--     the length of a query string or the length of the @User-Agent@
--     header.
--
-- -   Whether to perform any transformations on the request, such as
--     converting it to lowercase, before checking its length. Note that
--     transformations of the request body are not supported because the
--     AWS resource forwards only the first @8192@ bytes of your request to
--     AWS WAF.
--
--     You can only specify a single type of TextTransformation.
--
-- -   A @ComparisonOperator@ used for evaluating the selected part of the
--     request against the specified @Size@, such as equals, greater than,
--     less than, and so on.
--
-- -   The length, in bytes, that you want AWS WAF to watch for in selected
--     part of the request. The length is computed after applying the
--     transformation.
--
-- For example, you can add a @SizeConstraintSetUpdate@ object that matches
-- web requests in which the length of the @User-Agent@ header is greater
-- than 100 bytes. You can then configure AWS WAF to block those requests.
--
-- To create and configure a @SizeConstraintSet@, perform the following
-- steps:
--
-- 1.  Create a @SizeConstraintSet.@ For more information, see
--     CreateSizeConstraintSet.
--
-- 2.  Use GetChangeToken to get the change token that you provide in the
--     @ChangeToken@ parameter of an @UpdateSizeConstraintSet@ request.
--
-- 3.  Submit an @UpdateSizeConstraintSet@ request to specify the part of
--     the request that you want AWS WAF to inspect (for example, the
--     header or the URI) and the value that you want AWS WAF to watch for.
--
-- For more information about how to use the AWS WAF API to allow or block
-- HTTP requests, see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide>.
module Amazonka.WAFRegional.UpdateSizeConstraintSet
  ( -- * Creating a Request
    UpdateSizeConstraintSet (..),
    newUpdateSizeConstraintSet,

    -- * Request Lenses
    updateSizeConstraintSet_sizeConstraintSetId,
    updateSizeConstraintSet_changeToken,
    updateSizeConstraintSet_updates,

    -- * Destructuring the Response
    UpdateSizeConstraintSetResponse (..),
    newUpdateSizeConstraintSetResponse,

    -- * Response Lenses
    updateSizeConstraintSetResponse_changeToken,
    updateSizeConstraintSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFRegional.Types

-- | /See:/ 'newUpdateSizeConstraintSet' smart constructor.
data UpdateSizeConstraintSet = UpdateSizeConstraintSet'
  { -- | The @SizeConstraintSetId@ of the SizeConstraintSet that you want to
    -- update. @SizeConstraintSetId@ is returned by CreateSizeConstraintSet and
    -- by ListSizeConstraintSets.
    sizeConstraintSetId :: Prelude.Text,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Prelude.Text,
    -- | An array of @SizeConstraintSetUpdate@ objects that you want to insert
    -- into or delete from a SizeConstraintSet. For more information, see the
    -- applicable data types:
    --
    -- -   SizeConstraintSetUpdate: Contains @Action@ and @SizeConstraint@
    --
    -- -   SizeConstraint: Contains @FieldToMatch@, @TextTransformation@,
    --     @ComparisonOperator@, and @Size@
    --
    -- -   FieldToMatch: Contains @Data@ and @Type@
    updates :: Prelude.NonEmpty SizeConstraintSetUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSizeConstraintSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sizeConstraintSetId', 'updateSizeConstraintSet_sizeConstraintSetId' - The @SizeConstraintSetId@ of the SizeConstraintSet that you want to
-- update. @SizeConstraintSetId@ is returned by CreateSizeConstraintSet and
-- by ListSizeConstraintSets.
--
-- 'changeToken', 'updateSizeConstraintSet_changeToken' - The value returned by the most recent call to GetChangeToken.
--
-- 'updates', 'updateSizeConstraintSet_updates' - An array of @SizeConstraintSetUpdate@ objects that you want to insert
-- into or delete from a SizeConstraintSet. For more information, see the
-- applicable data types:
--
-- -   SizeConstraintSetUpdate: Contains @Action@ and @SizeConstraint@
--
-- -   SizeConstraint: Contains @FieldToMatch@, @TextTransformation@,
--     @ComparisonOperator@, and @Size@
--
-- -   FieldToMatch: Contains @Data@ and @Type@
newUpdateSizeConstraintSet ::
  -- | 'sizeConstraintSetId'
  Prelude.Text ->
  -- | 'changeToken'
  Prelude.Text ->
  -- | 'updates'
  Prelude.NonEmpty SizeConstraintSetUpdate ->
  UpdateSizeConstraintSet
newUpdateSizeConstraintSet
  pSizeConstraintSetId_
  pChangeToken_
  pUpdates_ =
    UpdateSizeConstraintSet'
      { sizeConstraintSetId =
          pSizeConstraintSetId_,
        changeToken = pChangeToken_,
        updates = Lens.coerced Lens.# pUpdates_
      }

-- | The @SizeConstraintSetId@ of the SizeConstraintSet that you want to
-- update. @SizeConstraintSetId@ is returned by CreateSizeConstraintSet and
-- by ListSizeConstraintSets.
updateSizeConstraintSet_sizeConstraintSetId :: Lens.Lens' UpdateSizeConstraintSet Prelude.Text
updateSizeConstraintSet_sizeConstraintSetId = Lens.lens (\UpdateSizeConstraintSet' {sizeConstraintSetId} -> sizeConstraintSetId) (\s@UpdateSizeConstraintSet' {} a -> s {sizeConstraintSetId = a} :: UpdateSizeConstraintSet)

-- | The value returned by the most recent call to GetChangeToken.
updateSizeConstraintSet_changeToken :: Lens.Lens' UpdateSizeConstraintSet Prelude.Text
updateSizeConstraintSet_changeToken = Lens.lens (\UpdateSizeConstraintSet' {changeToken} -> changeToken) (\s@UpdateSizeConstraintSet' {} a -> s {changeToken = a} :: UpdateSizeConstraintSet)

-- | An array of @SizeConstraintSetUpdate@ objects that you want to insert
-- into or delete from a SizeConstraintSet. For more information, see the
-- applicable data types:
--
-- -   SizeConstraintSetUpdate: Contains @Action@ and @SizeConstraint@
--
-- -   SizeConstraint: Contains @FieldToMatch@, @TextTransformation@,
--     @ComparisonOperator@, and @Size@
--
-- -   FieldToMatch: Contains @Data@ and @Type@
updateSizeConstraintSet_updates :: Lens.Lens' UpdateSizeConstraintSet (Prelude.NonEmpty SizeConstraintSetUpdate)
updateSizeConstraintSet_updates = Lens.lens (\UpdateSizeConstraintSet' {updates} -> updates) (\s@UpdateSizeConstraintSet' {} a -> s {updates = a} :: UpdateSizeConstraintSet) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateSizeConstraintSet where
  type
    AWSResponse UpdateSizeConstraintSet =
      UpdateSizeConstraintSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSizeConstraintSetResponse'
            Prelude.<$> (x Data..?> "ChangeToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSizeConstraintSet where
  hashWithSalt _salt UpdateSizeConstraintSet' {..} =
    _salt
      `Prelude.hashWithSalt` sizeConstraintSetId
      `Prelude.hashWithSalt` changeToken
      `Prelude.hashWithSalt` updates

instance Prelude.NFData UpdateSizeConstraintSet where
  rnf UpdateSizeConstraintSet' {..} =
    Prelude.rnf sizeConstraintSetId
      `Prelude.seq` Prelude.rnf changeToken
      `Prelude.seq` Prelude.rnf updates

instance Data.ToHeaders UpdateSizeConstraintSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_Regional_20161128.UpdateSizeConstraintSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateSizeConstraintSet where
  toJSON UpdateSizeConstraintSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SizeConstraintSetId" Data..= sizeConstraintSetId),
            Prelude.Just ("ChangeToken" Data..= changeToken),
            Prelude.Just ("Updates" Data..= updates)
          ]
      )

instance Data.ToPath UpdateSizeConstraintSet where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateSizeConstraintSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSizeConstraintSetResponse' smart constructor.
data UpdateSizeConstraintSetResponse = UpdateSizeConstraintSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @UpdateSizeConstraintSet@
    -- request. You can also use this value to query the status of the request.
    -- For more information, see GetChangeTokenStatus.
    changeToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSizeConstraintSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeToken', 'updateSizeConstraintSetResponse_changeToken' - The @ChangeToken@ that you used to submit the @UpdateSizeConstraintSet@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
--
-- 'httpStatus', 'updateSizeConstraintSetResponse_httpStatus' - The response's http status code.
newUpdateSizeConstraintSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSizeConstraintSetResponse
newUpdateSizeConstraintSetResponse pHttpStatus_ =
  UpdateSizeConstraintSetResponse'
    { changeToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ChangeToken@ that you used to submit the @UpdateSizeConstraintSet@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
updateSizeConstraintSetResponse_changeToken :: Lens.Lens' UpdateSizeConstraintSetResponse (Prelude.Maybe Prelude.Text)
updateSizeConstraintSetResponse_changeToken = Lens.lens (\UpdateSizeConstraintSetResponse' {changeToken} -> changeToken) (\s@UpdateSizeConstraintSetResponse' {} a -> s {changeToken = a} :: UpdateSizeConstraintSetResponse)

-- | The response's http status code.
updateSizeConstraintSetResponse_httpStatus :: Lens.Lens' UpdateSizeConstraintSetResponse Prelude.Int
updateSizeConstraintSetResponse_httpStatus = Lens.lens (\UpdateSizeConstraintSetResponse' {httpStatus} -> httpStatus) (\s@UpdateSizeConstraintSetResponse' {} a -> s {httpStatus = a} :: UpdateSizeConstraintSetResponse)

instance
  Prelude.NFData
    UpdateSizeConstraintSetResponse
  where
  rnf UpdateSizeConstraintSetResponse' {..} =
    Prelude.rnf changeToken
      `Prelude.seq` Prelude.rnf httpStatus
