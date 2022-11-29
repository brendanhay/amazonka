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
-- Module      : Amazonka.WAF.GetRegexPatternSet
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
-- Returns the RegexPatternSet specified by @RegexPatternSetId@.
module Amazonka.WAF.GetRegexPatternSet
  ( -- * Creating a Request
    GetRegexPatternSet (..),
    newGetRegexPatternSet,

    -- * Request Lenses
    getRegexPatternSet_regexPatternSetId,

    -- * Destructuring the Response
    GetRegexPatternSetResponse (..),
    newGetRegexPatternSetResponse,

    -- * Response Lenses
    getRegexPatternSetResponse_regexPatternSet,
    getRegexPatternSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAF.Types

-- | /See:/ 'newGetRegexPatternSet' smart constructor.
data GetRegexPatternSet = GetRegexPatternSet'
  { -- | The @RegexPatternSetId@ of the RegexPatternSet that you want to get.
    -- @RegexPatternSetId@ is returned by CreateRegexPatternSet and by
    -- ListRegexPatternSets.
    regexPatternSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRegexPatternSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regexPatternSetId', 'getRegexPatternSet_regexPatternSetId' - The @RegexPatternSetId@ of the RegexPatternSet that you want to get.
-- @RegexPatternSetId@ is returned by CreateRegexPatternSet and by
-- ListRegexPatternSets.
newGetRegexPatternSet ::
  -- | 'regexPatternSetId'
  Prelude.Text ->
  GetRegexPatternSet
newGetRegexPatternSet pRegexPatternSetId_ =
  GetRegexPatternSet'
    { regexPatternSetId =
        pRegexPatternSetId_
    }

-- | The @RegexPatternSetId@ of the RegexPatternSet that you want to get.
-- @RegexPatternSetId@ is returned by CreateRegexPatternSet and by
-- ListRegexPatternSets.
getRegexPatternSet_regexPatternSetId :: Lens.Lens' GetRegexPatternSet Prelude.Text
getRegexPatternSet_regexPatternSetId = Lens.lens (\GetRegexPatternSet' {regexPatternSetId} -> regexPatternSetId) (\s@GetRegexPatternSet' {} a -> s {regexPatternSetId = a} :: GetRegexPatternSet)

instance Core.AWSRequest GetRegexPatternSet where
  type
    AWSResponse GetRegexPatternSet =
      GetRegexPatternSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRegexPatternSetResponse'
            Prelude.<$> (x Core..?> "RegexPatternSet")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRegexPatternSet where
  hashWithSalt _salt GetRegexPatternSet' {..} =
    _salt `Prelude.hashWithSalt` regexPatternSetId

instance Prelude.NFData GetRegexPatternSet where
  rnf GetRegexPatternSet' {..} =
    Prelude.rnf regexPatternSetId

instance Core.ToHeaders GetRegexPatternSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_20150824.GetRegexPatternSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetRegexPatternSet where
  toJSON GetRegexPatternSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("RegexPatternSetId" Core..= regexPatternSetId)
          ]
      )

instance Core.ToPath GetRegexPatternSet where
  toPath = Prelude.const "/"

instance Core.ToQuery GetRegexPatternSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRegexPatternSetResponse' smart constructor.
data GetRegexPatternSetResponse = GetRegexPatternSetResponse'
  { -- | Information about the RegexPatternSet that you specified in the
    -- @GetRegexPatternSet@ request, including the identifier of the pattern
    -- set and the regular expression patterns you want AWS WAF to search for.
    regexPatternSet :: Prelude.Maybe RegexPatternSet,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRegexPatternSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regexPatternSet', 'getRegexPatternSetResponse_regexPatternSet' - Information about the RegexPatternSet that you specified in the
-- @GetRegexPatternSet@ request, including the identifier of the pattern
-- set and the regular expression patterns you want AWS WAF to search for.
--
-- 'httpStatus', 'getRegexPatternSetResponse_httpStatus' - The response's http status code.
newGetRegexPatternSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRegexPatternSetResponse
newGetRegexPatternSetResponse pHttpStatus_ =
  GetRegexPatternSetResponse'
    { regexPatternSet =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the RegexPatternSet that you specified in the
-- @GetRegexPatternSet@ request, including the identifier of the pattern
-- set and the regular expression patterns you want AWS WAF to search for.
getRegexPatternSetResponse_regexPatternSet :: Lens.Lens' GetRegexPatternSetResponse (Prelude.Maybe RegexPatternSet)
getRegexPatternSetResponse_regexPatternSet = Lens.lens (\GetRegexPatternSetResponse' {regexPatternSet} -> regexPatternSet) (\s@GetRegexPatternSetResponse' {} a -> s {regexPatternSet = a} :: GetRegexPatternSetResponse)

-- | The response's http status code.
getRegexPatternSetResponse_httpStatus :: Lens.Lens' GetRegexPatternSetResponse Prelude.Int
getRegexPatternSetResponse_httpStatus = Lens.lens (\GetRegexPatternSetResponse' {httpStatus} -> httpStatus) (\s@GetRegexPatternSetResponse' {} a -> s {httpStatus = a} :: GetRegexPatternSetResponse)

instance Prelude.NFData GetRegexPatternSetResponse where
  rnf GetRegexPatternSetResponse' {..} =
    Prelude.rnf regexPatternSet
      `Prelude.seq` Prelude.rnf httpStatus
