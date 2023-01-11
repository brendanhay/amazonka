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
-- Module      : Amazonka.WAF.UpdateRegexPatternSet
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
-- Inserts or deletes @RegexPatternString@ objects in a RegexPatternSet.
-- For each @RegexPatternString@ object, you specify the following values:
--
-- -   Whether to insert or delete the @RegexPatternString@.
--
-- -   The regular expression pattern that you want to insert or delete.
--     For more information, see RegexPatternSet.
--
-- For example, you can create a @RegexPatternString@ such as
-- @B[a\@]dB[o0]t@. AWS WAF will match this @RegexPatternString@ to:
--
-- -   BadBot
--
-- -   BadB0t
--
-- -   B\@dBot
--
-- -   B\@dB0t
--
-- To create and configure a @RegexPatternSet@, perform the following
-- steps:
--
-- 1.  Create a @RegexPatternSet.@ For more information, see
--     CreateRegexPatternSet.
--
-- 2.  Use GetChangeToken to get the change token that you provide in the
--     @ChangeToken@ parameter of an @UpdateRegexPatternSet@ request.
--
-- 3.  Submit an @UpdateRegexPatternSet@ request to specify the regular
--     expression pattern that you want AWS WAF to watch for.
--
-- For more information about how to use the AWS WAF API to allow or block
-- HTTP requests, see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide>.
module Amazonka.WAF.UpdateRegexPatternSet
  ( -- * Creating a Request
    UpdateRegexPatternSet (..),
    newUpdateRegexPatternSet,

    -- * Request Lenses
    updateRegexPatternSet_regexPatternSetId,
    updateRegexPatternSet_updates,
    updateRegexPatternSet_changeToken,

    -- * Destructuring the Response
    UpdateRegexPatternSetResponse (..),
    newUpdateRegexPatternSetResponse,

    -- * Response Lenses
    updateRegexPatternSetResponse_changeToken,
    updateRegexPatternSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAF.Types

-- | /See:/ 'newUpdateRegexPatternSet' smart constructor.
data UpdateRegexPatternSet = UpdateRegexPatternSet'
  { -- | The @RegexPatternSetId@ of the RegexPatternSet that you want to update.
    -- @RegexPatternSetId@ is returned by CreateRegexPatternSet and by
    -- ListRegexPatternSets.
    regexPatternSetId :: Prelude.Text,
    -- | An array of @RegexPatternSetUpdate@ objects that you want to insert into
    -- or delete from a RegexPatternSet.
    updates :: Prelude.NonEmpty RegexPatternSetUpdate,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRegexPatternSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regexPatternSetId', 'updateRegexPatternSet_regexPatternSetId' - The @RegexPatternSetId@ of the RegexPatternSet that you want to update.
-- @RegexPatternSetId@ is returned by CreateRegexPatternSet and by
-- ListRegexPatternSets.
--
-- 'updates', 'updateRegexPatternSet_updates' - An array of @RegexPatternSetUpdate@ objects that you want to insert into
-- or delete from a RegexPatternSet.
--
-- 'changeToken', 'updateRegexPatternSet_changeToken' - The value returned by the most recent call to GetChangeToken.
newUpdateRegexPatternSet ::
  -- | 'regexPatternSetId'
  Prelude.Text ->
  -- | 'updates'
  Prelude.NonEmpty RegexPatternSetUpdate ->
  -- | 'changeToken'
  Prelude.Text ->
  UpdateRegexPatternSet
newUpdateRegexPatternSet
  pRegexPatternSetId_
  pUpdates_
  pChangeToken_ =
    UpdateRegexPatternSet'
      { regexPatternSetId =
          pRegexPatternSetId_,
        updates = Lens.coerced Lens.# pUpdates_,
        changeToken = pChangeToken_
      }

-- | The @RegexPatternSetId@ of the RegexPatternSet that you want to update.
-- @RegexPatternSetId@ is returned by CreateRegexPatternSet and by
-- ListRegexPatternSets.
updateRegexPatternSet_regexPatternSetId :: Lens.Lens' UpdateRegexPatternSet Prelude.Text
updateRegexPatternSet_regexPatternSetId = Lens.lens (\UpdateRegexPatternSet' {regexPatternSetId} -> regexPatternSetId) (\s@UpdateRegexPatternSet' {} a -> s {regexPatternSetId = a} :: UpdateRegexPatternSet)

-- | An array of @RegexPatternSetUpdate@ objects that you want to insert into
-- or delete from a RegexPatternSet.
updateRegexPatternSet_updates :: Lens.Lens' UpdateRegexPatternSet (Prelude.NonEmpty RegexPatternSetUpdate)
updateRegexPatternSet_updates = Lens.lens (\UpdateRegexPatternSet' {updates} -> updates) (\s@UpdateRegexPatternSet' {} a -> s {updates = a} :: UpdateRegexPatternSet) Prelude.. Lens.coerced

-- | The value returned by the most recent call to GetChangeToken.
updateRegexPatternSet_changeToken :: Lens.Lens' UpdateRegexPatternSet Prelude.Text
updateRegexPatternSet_changeToken = Lens.lens (\UpdateRegexPatternSet' {changeToken} -> changeToken) (\s@UpdateRegexPatternSet' {} a -> s {changeToken = a} :: UpdateRegexPatternSet)

instance Core.AWSRequest UpdateRegexPatternSet where
  type
    AWSResponse UpdateRegexPatternSet =
      UpdateRegexPatternSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRegexPatternSetResponse'
            Prelude.<$> (x Data..?> "ChangeToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRegexPatternSet where
  hashWithSalt _salt UpdateRegexPatternSet' {..} =
    _salt `Prelude.hashWithSalt` regexPatternSetId
      `Prelude.hashWithSalt` updates
      `Prelude.hashWithSalt` changeToken

instance Prelude.NFData UpdateRegexPatternSet where
  rnf UpdateRegexPatternSet' {..} =
    Prelude.rnf regexPatternSetId
      `Prelude.seq` Prelude.rnf updates
      `Prelude.seq` Prelude.rnf changeToken

instance Data.ToHeaders UpdateRegexPatternSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20150824.UpdateRegexPatternSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRegexPatternSet where
  toJSON UpdateRegexPatternSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("RegexPatternSetId" Data..= regexPatternSetId),
            Prelude.Just ("Updates" Data..= updates),
            Prelude.Just ("ChangeToken" Data..= changeToken)
          ]
      )

instance Data.ToPath UpdateRegexPatternSet where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateRegexPatternSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRegexPatternSetResponse' smart constructor.
data UpdateRegexPatternSetResponse = UpdateRegexPatternSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @UpdateRegexPatternSet@
    -- request. You can also use this value to query the status of the request.
    -- For more information, see GetChangeTokenStatus.
    changeToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRegexPatternSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeToken', 'updateRegexPatternSetResponse_changeToken' - The @ChangeToken@ that you used to submit the @UpdateRegexPatternSet@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
--
-- 'httpStatus', 'updateRegexPatternSetResponse_httpStatus' - The response's http status code.
newUpdateRegexPatternSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRegexPatternSetResponse
newUpdateRegexPatternSetResponse pHttpStatus_ =
  UpdateRegexPatternSetResponse'
    { changeToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ChangeToken@ that you used to submit the @UpdateRegexPatternSet@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
updateRegexPatternSetResponse_changeToken :: Lens.Lens' UpdateRegexPatternSetResponse (Prelude.Maybe Prelude.Text)
updateRegexPatternSetResponse_changeToken = Lens.lens (\UpdateRegexPatternSetResponse' {changeToken} -> changeToken) (\s@UpdateRegexPatternSetResponse' {} a -> s {changeToken = a} :: UpdateRegexPatternSetResponse)

-- | The response's http status code.
updateRegexPatternSetResponse_httpStatus :: Lens.Lens' UpdateRegexPatternSetResponse Prelude.Int
updateRegexPatternSetResponse_httpStatus = Lens.lens (\UpdateRegexPatternSetResponse' {httpStatus} -> httpStatus) (\s@UpdateRegexPatternSetResponse' {} a -> s {httpStatus = a} :: UpdateRegexPatternSetResponse)

instance Prelude.NFData UpdateRegexPatternSetResponse where
  rnf UpdateRegexPatternSetResponse' {..} =
    Prelude.rnf changeToken
      `Prelude.seq` Prelude.rnf httpStatus
