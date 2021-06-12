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
-- Module      : Network.AWS.WAFRegional.GetRegexPatternSet
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
-- Returns the RegexPatternSet specified by @RegexPatternSetId@.
module Network.AWS.WAFRegional.GetRegexPatternSet
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAFRegional.Types

-- | /See:/ 'newGetRegexPatternSet' smart constructor.
data GetRegexPatternSet = GetRegexPatternSet'
  { -- | The @RegexPatternSetId@ of the RegexPatternSet that you want to get.
    -- @RegexPatternSetId@ is returned by CreateRegexPatternSet and by
    -- ListRegexPatternSets.
    regexPatternSetId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetRegexPatternSet
newGetRegexPatternSet pRegexPatternSetId_ =
  GetRegexPatternSet'
    { regexPatternSetId =
        pRegexPatternSetId_
    }

-- | The @RegexPatternSetId@ of the RegexPatternSet that you want to get.
-- @RegexPatternSetId@ is returned by CreateRegexPatternSet and by
-- ListRegexPatternSets.
getRegexPatternSet_regexPatternSetId :: Lens.Lens' GetRegexPatternSet Core.Text
getRegexPatternSet_regexPatternSetId = Lens.lens (\GetRegexPatternSet' {regexPatternSetId} -> regexPatternSetId) (\s@GetRegexPatternSet' {} a -> s {regexPatternSetId = a} :: GetRegexPatternSet)

instance Core.AWSRequest GetRegexPatternSet where
  type
    AWSResponse GetRegexPatternSet =
      GetRegexPatternSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRegexPatternSetResponse'
            Core.<$> (x Core..?> "RegexPatternSet")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetRegexPatternSet

instance Core.NFData GetRegexPatternSet

instance Core.ToHeaders GetRegexPatternSet where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_Regional_20161128.GetRegexPatternSet" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetRegexPatternSet where
  toJSON GetRegexPatternSet' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("RegexPatternSetId" Core..= regexPatternSetId)
          ]
      )

instance Core.ToPath GetRegexPatternSet where
  toPath = Core.const "/"

instance Core.ToQuery GetRegexPatternSet where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetRegexPatternSetResponse' smart constructor.
data GetRegexPatternSetResponse = GetRegexPatternSetResponse'
  { -- | Information about the RegexPatternSet that you specified in the
    -- @GetRegexPatternSet@ request, including the identifier of the pattern
    -- set and the regular expression patterns you want AWS WAF to search for.
    regexPatternSet :: Core.Maybe RegexPatternSet,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetRegexPatternSetResponse
newGetRegexPatternSetResponse pHttpStatus_ =
  GetRegexPatternSetResponse'
    { regexPatternSet =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the RegexPatternSet that you specified in the
-- @GetRegexPatternSet@ request, including the identifier of the pattern
-- set and the regular expression patterns you want AWS WAF to search for.
getRegexPatternSetResponse_regexPatternSet :: Lens.Lens' GetRegexPatternSetResponse (Core.Maybe RegexPatternSet)
getRegexPatternSetResponse_regexPatternSet = Lens.lens (\GetRegexPatternSetResponse' {regexPatternSet} -> regexPatternSet) (\s@GetRegexPatternSetResponse' {} a -> s {regexPatternSet = a} :: GetRegexPatternSetResponse)

-- | The response's http status code.
getRegexPatternSetResponse_httpStatus :: Lens.Lens' GetRegexPatternSetResponse Core.Int
getRegexPatternSetResponse_httpStatus = Lens.lens (\GetRegexPatternSetResponse' {httpStatus} -> httpStatus) (\s@GetRegexPatternSetResponse' {} a -> s {httpStatus = a} :: GetRegexPatternSetResponse)

instance Core.NFData GetRegexPatternSetResponse
