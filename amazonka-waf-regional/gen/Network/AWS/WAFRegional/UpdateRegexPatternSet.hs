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
-- Module      : Network.AWS.WAFRegional.UpdateRegexPatternSet
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
module Network.AWS.WAFRegional.UpdateRegexPatternSet
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAFRegional.Types

-- | /See:/ 'newUpdateRegexPatternSet' smart constructor.
data UpdateRegexPatternSet = UpdateRegexPatternSet'
  { -- | The @RegexPatternSetId@ of the RegexPatternSet that you want to update.
    -- @RegexPatternSetId@ is returned by CreateRegexPatternSet and by
    -- ListRegexPatternSets.
    regexPatternSetId :: Core.Text,
    -- | An array of @RegexPatternSetUpdate@ objects that you want to insert into
    -- or delete from a RegexPatternSet.
    updates :: Core.NonEmpty RegexPatternSetUpdate,
    -- | The value returned by the most recent call to GetChangeToken.
    changeToken :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'updates'
  Core.NonEmpty RegexPatternSetUpdate ->
  -- | 'changeToken'
  Core.Text ->
  UpdateRegexPatternSet
newUpdateRegexPatternSet
  pRegexPatternSetId_
  pUpdates_
  pChangeToken_ =
    UpdateRegexPatternSet'
      { regexPatternSetId =
          pRegexPatternSetId_,
        updates = Lens._Coerce Lens.# pUpdates_,
        changeToken = pChangeToken_
      }

-- | The @RegexPatternSetId@ of the RegexPatternSet that you want to update.
-- @RegexPatternSetId@ is returned by CreateRegexPatternSet and by
-- ListRegexPatternSets.
updateRegexPatternSet_regexPatternSetId :: Lens.Lens' UpdateRegexPatternSet Core.Text
updateRegexPatternSet_regexPatternSetId = Lens.lens (\UpdateRegexPatternSet' {regexPatternSetId} -> regexPatternSetId) (\s@UpdateRegexPatternSet' {} a -> s {regexPatternSetId = a} :: UpdateRegexPatternSet)

-- | An array of @RegexPatternSetUpdate@ objects that you want to insert into
-- or delete from a RegexPatternSet.
updateRegexPatternSet_updates :: Lens.Lens' UpdateRegexPatternSet (Core.NonEmpty RegexPatternSetUpdate)
updateRegexPatternSet_updates = Lens.lens (\UpdateRegexPatternSet' {updates} -> updates) (\s@UpdateRegexPatternSet' {} a -> s {updates = a} :: UpdateRegexPatternSet) Core.. Lens._Coerce

-- | The value returned by the most recent call to GetChangeToken.
updateRegexPatternSet_changeToken :: Lens.Lens' UpdateRegexPatternSet Core.Text
updateRegexPatternSet_changeToken = Lens.lens (\UpdateRegexPatternSet' {changeToken} -> changeToken) (\s@UpdateRegexPatternSet' {} a -> s {changeToken = a} :: UpdateRegexPatternSet)

instance Core.AWSRequest UpdateRegexPatternSet where
  type
    AWSResponse UpdateRegexPatternSet =
      UpdateRegexPatternSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRegexPatternSetResponse'
            Core.<$> (x Core..?> "ChangeToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateRegexPatternSet

instance Core.NFData UpdateRegexPatternSet

instance Core.ToHeaders UpdateRegexPatternSet where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_Regional_20161128.UpdateRegexPatternSet" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateRegexPatternSet where
  toJSON UpdateRegexPatternSet' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("RegexPatternSetId" Core..= regexPatternSetId),
            Core.Just ("Updates" Core..= updates),
            Core.Just ("ChangeToken" Core..= changeToken)
          ]
      )

instance Core.ToPath UpdateRegexPatternSet where
  toPath = Core.const "/"

instance Core.ToQuery UpdateRegexPatternSet where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateRegexPatternSetResponse' smart constructor.
data UpdateRegexPatternSetResponse = UpdateRegexPatternSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @UpdateRegexPatternSet@
    -- request. You can also use this value to query the status of the request.
    -- For more information, see GetChangeTokenStatus.
    changeToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateRegexPatternSetResponse
newUpdateRegexPatternSetResponse pHttpStatus_ =
  UpdateRegexPatternSetResponse'
    { changeToken =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ChangeToken@ that you used to submit the @UpdateRegexPatternSet@
-- request. You can also use this value to query the status of the request.
-- For more information, see GetChangeTokenStatus.
updateRegexPatternSetResponse_changeToken :: Lens.Lens' UpdateRegexPatternSetResponse (Core.Maybe Core.Text)
updateRegexPatternSetResponse_changeToken = Lens.lens (\UpdateRegexPatternSetResponse' {changeToken} -> changeToken) (\s@UpdateRegexPatternSetResponse' {} a -> s {changeToken = a} :: UpdateRegexPatternSetResponse)

-- | The response's http status code.
updateRegexPatternSetResponse_httpStatus :: Lens.Lens' UpdateRegexPatternSetResponse Core.Int
updateRegexPatternSetResponse_httpStatus = Lens.lens (\UpdateRegexPatternSetResponse' {httpStatus} -> httpStatus) (\s@UpdateRegexPatternSetResponse' {} a -> s {httpStatus = a} :: UpdateRegexPatternSetResponse)

instance Core.NFData UpdateRegexPatternSetResponse
