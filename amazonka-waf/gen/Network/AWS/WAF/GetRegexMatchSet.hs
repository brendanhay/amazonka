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
-- Module      : Network.AWS.WAF.GetRegexMatchSet
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
-- Returns the RegexMatchSet specified by @RegexMatchSetId@.
module Network.AWS.WAF.GetRegexMatchSet
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | /See:/ 'newGetRegexMatchSet' smart constructor.
data GetRegexMatchSet = GetRegexMatchSet'
  { -- | The @RegexMatchSetId@ of the RegexMatchSet that you want to get.
    -- @RegexMatchSetId@ is returned by CreateRegexMatchSet and by
    -- ListRegexMatchSets.
    regexMatchSetId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetRegexMatchSet
newGetRegexMatchSet pRegexMatchSetId_ =
  GetRegexMatchSet'
    { regexMatchSetId =
        pRegexMatchSetId_
    }

-- | The @RegexMatchSetId@ of the RegexMatchSet that you want to get.
-- @RegexMatchSetId@ is returned by CreateRegexMatchSet and by
-- ListRegexMatchSets.
getRegexMatchSet_regexMatchSetId :: Lens.Lens' GetRegexMatchSet Core.Text
getRegexMatchSet_regexMatchSetId = Lens.lens (\GetRegexMatchSet' {regexMatchSetId} -> regexMatchSetId) (\s@GetRegexMatchSet' {} a -> s {regexMatchSetId = a} :: GetRegexMatchSet)

instance Core.AWSRequest GetRegexMatchSet where
  type
    AWSResponse GetRegexMatchSet =
      GetRegexMatchSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRegexMatchSetResponse'
            Core.<$> (x Core..?> "RegexMatchSet")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetRegexMatchSet

instance Core.NFData GetRegexMatchSet

instance Core.ToHeaders GetRegexMatchSet where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_20150824.GetRegexMatchSet" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetRegexMatchSet where
  toJSON GetRegexMatchSet' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("RegexMatchSetId" Core..= regexMatchSetId)
          ]
      )

instance Core.ToPath GetRegexMatchSet where
  toPath = Core.const "/"

instance Core.ToQuery GetRegexMatchSet where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetRegexMatchSetResponse' smart constructor.
data GetRegexMatchSetResponse = GetRegexMatchSetResponse'
  { -- | Information about the RegexMatchSet that you specified in the
    -- @GetRegexMatchSet@ request. For more information, see RegexMatchTuple.
    regexMatchSet :: Core.Maybe RegexMatchSet,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetRegexMatchSetResponse
newGetRegexMatchSetResponse pHttpStatus_ =
  GetRegexMatchSetResponse'
    { regexMatchSet =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the RegexMatchSet that you specified in the
-- @GetRegexMatchSet@ request. For more information, see RegexMatchTuple.
getRegexMatchSetResponse_regexMatchSet :: Lens.Lens' GetRegexMatchSetResponse (Core.Maybe RegexMatchSet)
getRegexMatchSetResponse_regexMatchSet = Lens.lens (\GetRegexMatchSetResponse' {regexMatchSet} -> regexMatchSet) (\s@GetRegexMatchSetResponse' {} a -> s {regexMatchSet = a} :: GetRegexMatchSetResponse)

-- | The response's http status code.
getRegexMatchSetResponse_httpStatus :: Lens.Lens' GetRegexMatchSetResponse Core.Int
getRegexMatchSetResponse_httpStatus = Lens.lens (\GetRegexMatchSetResponse' {httpStatus} -> httpStatus) (\s@GetRegexMatchSetResponse' {} a -> s {httpStatus = a} :: GetRegexMatchSetResponse)

instance Core.NFData GetRegexMatchSetResponse
