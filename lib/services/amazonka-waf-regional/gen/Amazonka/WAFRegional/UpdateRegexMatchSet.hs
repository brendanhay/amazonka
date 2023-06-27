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
-- Module      : Amazonka.WAFRegional.UpdateRegexMatchSet
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- Inserts or deletes RegexMatchTuple objects (filters) in a RegexMatchSet.
-- For each @RegexMatchSetUpdate@ object, you specify the following values:
--
-- -   Whether to insert or delete the object from the array. If you want
--     to change a @RegexMatchSetUpdate@ object, you delete the existing
--     object and add a new one.
--
-- -   The part of a web request that you want AWS WAF to inspectupdate,
--     such as a query string or the value of the @User-Agent@ header.
--
-- -   The identifier of the pattern (a regular expression) that you want
--     AWS WAF to look for. For more information, see RegexPatternSet.
--
-- -   Whether to perform any conversions on the request, such as
--     converting it to lowercase, before inspecting it for the specified
--     string.
--
-- For example, you can create a @RegexPatternSet@ that matches any
-- requests with @User-Agent@ headers that contain the string
-- @B[a\@]dB[o0]t@. You can then configure AWS WAF to reject those
-- requests.
--
-- To create and configure a @RegexMatchSet@, perform the following steps:
--
-- 1.  Create a @RegexMatchSet.@ For more information, see
--     CreateRegexMatchSet.
--
-- 2.  Use GetChangeToken to get the change token that you provide in the
--     @ChangeToken@ parameter of an @UpdateRegexMatchSet@ request.
--
-- 3.  Submit an @UpdateRegexMatchSet@ request to specify the part of the
--     request that you want AWS WAF to inspect (for example, the header or
--     the URI) and the identifier of the @RegexPatternSet@ that contain
--     the regular expression patters you want AWS WAF to watch for.
--
-- For more information about how to use the AWS WAF API to allow or block
-- HTTP requests, see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide>.
module Amazonka.WAFRegional.UpdateRegexMatchSet
  ( -- * Creating a Request
    UpdateRegexMatchSet (..),
    newUpdateRegexMatchSet,

    -- * Request Lenses
    updateRegexMatchSet_regexMatchSetId,
    updateRegexMatchSet_updates,
    updateRegexMatchSet_changeToken,

    -- * Destructuring the Response
    UpdateRegexMatchSetResponse (..),
    newUpdateRegexMatchSetResponse,

    -- * Response Lenses
    updateRegexMatchSetResponse_changeToken,
    updateRegexMatchSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFRegional.Types

-- | /See:/ 'newUpdateRegexMatchSet' smart constructor.
data UpdateRegexMatchSet = UpdateRegexMatchSet'
  { -- | The @RegexMatchSetId@ of the RegexMatchSet that you want to update.
    -- @RegexMatchSetId@ is returned by CreateRegexMatchSet and by
    -- ListRegexMatchSets.
    regexMatchSetId :: Prelude.Text,
    -- | An array of @RegexMatchSetUpdate@ objects that you want to insert into
    -- or delete from a RegexMatchSet. For more information, see
    -- RegexMatchTuple.
    updates :: Prelude.NonEmpty RegexMatchSetUpdate,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRegexMatchSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regexMatchSetId', 'updateRegexMatchSet_regexMatchSetId' - The @RegexMatchSetId@ of the RegexMatchSet that you want to update.
-- @RegexMatchSetId@ is returned by CreateRegexMatchSet and by
-- ListRegexMatchSets.
--
-- 'updates', 'updateRegexMatchSet_updates' - An array of @RegexMatchSetUpdate@ objects that you want to insert into
-- or delete from a RegexMatchSet. For more information, see
-- RegexMatchTuple.
--
-- 'changeToken', 'updateRegexMatchSet_changeToken' - The value returned by the most recent call to GetChangeToken.
newUpdateRegexMatchSet ::
  -- | 'regexMatchSetId'
  Prelude.Text ->
  -- | 'updates'
  Prelude.NonEmpty RegexMatchSetUpdate ->
  -- | 'changeToken'
  Prelude.Text ->
  UpdateRegexMatchSet
newUpdateRegexMatchSet
  pRegexMatchSetId_
  pUpdates_
  pChangeToken_ =
    UpdateRegexMatchSet'
      { regexMatchSetId =
          pRegexMatchSetId_,
        updates = Lens.coerced Lens.# pUpdates_,
        changeToken = pChangeToken_
      }

-- | The @RegexMatchSetId@ of the RegexMatchSet that you want to update.
-- @RegexMatchSetId@ is returned by CreateRegexMatchSet and by
-- ListRegexMatchSets.
updateRegexMatchSet_regexMatchSetId :: Lens.Lens' UpdateRegexMatchSet Prelude.Text
updateRegexMatchSet_regexMatchSetId = Lens.lens (\UpdateRegexMatchSet' {regexMatchSetId} -> regexMatchSetId) (\s@UpdateRegexMatchSet' {} a -> s {regexMatchSetId = a} :: UpdateRegexMatchSet)

-- | An array of @RegexMatchSetUpdate@ objects that you want to insert into
-- or delete from a RegexMatchSet. For more information, see
-- RegexMatchTuple.
updateRegexMatchSet_updates :: Lens.Lens' UpdateRegexMatchSet (Prelude.NonEmpty RegexMatchSetUpdate)
updateRegexMatchSet_updates = Lens.lens (\UpdateRegexMatchSet' {updates} -> updates) (\s@UpdateRegexMatchSet' {} a -> s {updates = a} :: UpdateRegexMatchSet) Prelude.. Lens.coerced

-- | The value returned by the most recent call to GetChangeToken.
updateRegexMatchSet_changeToken :: Lens.Lens' UpdateRegexMatchSet Prelude.Text
updateRegexMatchSet_changeToken = Lens.lens (\UpdateRegexMatchSet' {changeToken} -> changeToken) (\s@UpdateRegexMatchSet' {} a -> s {changeToken = a} :: UpdateRegexMatchSet)

instance Core.AWSRequest UpdateRegexMatchSet where
  type
    AWSResponse UpdateRegexMatchSet =
      UpdateRegexMatchSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRegexMatchSetResponse'
            Prelude.<$> (x Data..?> "ChangeToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRegexMatchSet where
  hashWithSalt _salt UpdateRegexMatchSet' {..} =
    _salt
      `Prelude.hashWithSalt` regexMatchSetId
      `Prelude.hashWithSalt` updates
      `Prelude.hashWithSalt` changeToken

instance Prelude.NFData UpdateRegexMatchSet where
  rnf UpdateRegexMatchSet' {..} =
    Prelude.rnf regexMatchSetId
      `Prelude.seq` Prelude.rnf updates
      `Prelude.seq` Prelude.rnf changeToken

instance Data.ToHeaders UpdateRegexMatchSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_Regional_20161128.UpdateRegexMatchSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRegexMatchSet where
  toJSON UpdateRegexMatchSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("RegexMatchSetId" Data..= regexMatchSetId),
            Prelude.Just ("Updates" Data..= updates),
            Prelude.Just ("ChangeToken" Data..= changeToken)
          ]
      )

instance Data.ToPath UpdateRegexMatchSet where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateRegexMatchSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRegexMatchSetResponse' smart constructor.
data UpdateRegexMatchSetResponse = UpdateRegexMatchSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @UpdateRegexMatchSet@
    -- request. You can also use this value to query the status of the request.
    -- For more information, see GetChangeTokenStatus.
    changeToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRegexMatchSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeToken', 'updateRegexMatchSetResponse_changeToken' - The @ChangeToken@ that you used to submit the @UpdateRegexMatchSet@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
--
-- 'httpStatus', 'updateRegexMatchSetResponse_httpStatus' - The response's http status code.
newUpdateRegexMatchSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRegexMatchSetResponse
newUpdateRegexMatchSetResponse pHttpStatus_ =
  UpdateRegexMatchSetResponse'
    { changeToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ChangeToken@ that you used to submit the @UpdateRegexMatchSet@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
updateRegexMatchSetResponse_changeToken :: Lens.Lens' UpdateRegexMatchSetResponse (Prelude.Maybe Prelude.Text)
updateRegexMatchSetResponse_changeToken = Lens.lens (\UpdateRegexMatchSetResponse' {changeToken} -> changeToken) (\s@UpdateRegexMatchSetResponse' {} a -> s {changeToken = a} :: UpdateRegexMatchSetResponse)

-- | The response's http status code.
updateRegexMatchSetResponse_httpStatus :: Lens.Lens' UpdateRegexMatchSetResponse Prelude.Int
updateRegexMatchSetResponse_httpStatus = Lens.lens (\UpdateRegexMatchSetResponse' {httpStatus} -> httpStatus) (\s@UpdateRegexMatchSetResponse' {} a -> s {httpStatus = a} :: UpdateRegexMatchSetResponse)

instance Prelude.NFData UpdateRegexMatchSetResponse where
  rnf UpdateRegexMatchSetResponse' {..} =
    Prelude.rnf changeToken
      `Prelude.seq` Prelude.rnf httpStatus
