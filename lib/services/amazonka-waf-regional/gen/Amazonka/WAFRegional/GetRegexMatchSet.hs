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
-- Module      : Amazonka.WAFRegional.GetRegexMatchSet
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
-- Returns the RegexMatchSet specified by @RegexMatchSetId@.
module Amazonka.WAFRegional.GetRegexMatchSet
  ( -- * Creating a Request
    GetRegexMatchSet (..),
    newGetRegexMatchSet,

    -- * Request Lenses
    getRegexMatchSet_regexMatchSetId,

    -- * Destructuring the Response
    GetRegexMatchSetResponse (..),
    newGetRegexMatchSetResponse,

    -- * Response Lenses
    getRegexMatchSetResponse_regexMatchSet,
    getRegexMatchSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFRegional.Types

-- | /See:/ 'newGetRegexMatchSet' smart constructor.
data GetRegexMatchSet = GetRegexMatchSet'
  { -- | The @RegexMatchSetId@ of the RegexMatchSet that you want to get.
    -- @RegexMatchSetId@ is returned by CreateRegexMatchSet and by
    -- ListRegexMatchSets.
    regexMatchSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRegexMatchSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regexMatchSetId', 'getRegexMatchSet_regexMatchSetId' - The @RegexMatchSetId@ of the RegexMatchSet that you want to get.
-- @RegexMatchSetId@ is returned by CreateRegexMatchSet and by
-- ListRegexMatchSets.
newGetRegexMatchSet ::
  -- | 'regexMatchSetId'
  Prelude.Text ->
  GetRegexMatchSet
newGetRegexMatchSet pRegexMatchSetId_ =
  GetRegexMatchSet'
    { regexMatchSetId =
        pRegexMatchSetId_
    }

-- | The @RegexMatchSetId@ of the RegexMatchSet that you want to get.
-- @RegexMatchSetId@ is returned by CreateRegexMatchSet and by
-- ListRegexMatchSets.
getRegexMatchSet_regexMatchSetId :: Lens.Lens' GetRegexMatchSet Prelude.Text
getRegexMatchSet_regexMatchSetId = Lens.lens (\GetRegexMatchSet' {regexMatchSetId} -> regexMatchSetId) (\s@GetRegexMatchSet' {} a -> s {regexMatchSetId = a} :: GetRegexMatchSet)

instance Core.AWSRequest GetRegexMatchSet where
  type
    AWSResponse GetRegexMatchSet =
      GetRegexMatchSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRegexMatchSetResponse'
            Prelude.<$> (x Data..?> "RegexMatchSet")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRegexMatchSet where
  hashWithSalt _salt GetRegexMatchSet' {..} =
    _salt `Prelude.hashWithSalt` regexMatchSetId

instance Prelude.NFData GetRegexMatchSet where
  rnf GetRegexMatchSet' {..} =
    Prelude.rnf regexMatchSetId

instance Data.ToHeaders GetRegexMatchSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_Regional_20161128.GetRegexMatchSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetRegexMatchSet where
  toJSON GetRegexMatchSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("RegexMatchSetId" Data..= regexMatchSetId)
          ]
      )

instance Data.ToPath GetRegexMatchSet where
  toPath = Prelude.const "/"

instance Data.ToQuery GetRegexMatchSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRegexMatchSetResponse' smart constructor.
data GetRegexMatchSetResponse = GetRegexMatchSetResponse'
  { -- | Information about the RegexMatchSet that you specified in the
    -- @GetRegexMatchSet@ request. For more information, see RegexMatchTuple.
    regexMatchSet :: Prelude.Maybe RegexMatchSet,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRegexMatchSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regexMatchSet', 'getRegexMatchSetResponse_regexMatchSet' - Information about the RegexMatchSet that you specified in the
-- @GetRegexMatchSet@ request. For more information, see RegexMatchTuple.
--
-- 'httpStatus', 'getRegexMatchSetResponse_httpStatus' - The response's http status code.
newGetRegexMatchSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRegexMatchSetResponse
newGetRegexMatchSetResponse pHttpStatus_ =
  GetRegexMatchSetResponse'
    { regexMatchSet =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the RegexMatchSet that you specified in the
-- @GetRegexMatchSet@ request. For more information, see RegexMatchTuple.
getRegexMatchSetResponse_regexMatchSet :: Lens.Lens' GetRegexMatchSetResponse (Prelude.Maybe RegexMatchSet)
getRegexMatchSetResponse_regexMatchSet = Lens.lens (\GetRegexMatchSetResponse' {regexMatchSet} -> regexMatchSet) (\s@GetRegexMatchSetResponse' {} a -> s {regexMatchSet = a} :: GetRegexMatchSetResponse)

-- | The response's http status code.
getRegexMatchSetResponse_httpStatus :: Lens.Lens' GetRegexMatchSetResponse Prelude.Int
getRegexMatchSetResponse_httpStatus = Lens.lens (\GetRegexMatchSetResponse' {httpStatus} -> httpStatus) (\s@GetRegexMatchSetResponse' {} a -> s {httpStatus = a} :: GetRegexMatchSetResponse)

instance Prelude.NFData GetRegexMatchSetResponse where
  rnf GetRegexMatchSetResponse' {..} =
    Prelude.rnf regexMatchSet
      `Prelude.seq` Prelude.rnf httpStatus
