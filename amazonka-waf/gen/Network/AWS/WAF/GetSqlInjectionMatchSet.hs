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
-- Module      : Network.AWS.WAF.GetSqlInjectionMatchSet
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
-- Returns the SqlInjectionMatchSet that is specified by
-- @SqlInjectionMatchSetId@.
module Network.AWS.WAF.GetSqlInjectionMatchSet
  ( -- * Creating a Request
    GetSqlInjectionMatchSet (..),
    newGetSqlInjectionMatchSet,

    -- * Request Lenses
    getSqlInjectionMatchSet_sqlInjectionMatchSetId,

    -- * Destructuring the Response
    GetSqlInjectionMatchSetResponse (..),
    newGetSqlInjectionMatchSetResponse,

    -- * Response Lenses
    getSqlInjectionMatchSetResponse_sqlInjectionMatchSet,
    getSqlInjectionMatchSetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | A request to get a SqlInjectionMatchSet.
--
-- /See:/ 'newGetSqlInjectionMatchSet' smart constructor.
data GetSqlInjectionMatchSet = GetSqlInjectionMatchSet'
  { -- | The @SqlInjectionMatchSetId@ of the SqlInjectionMatchSet that you want
    -- to get. @SqlInjectionMatchSetId@ is returned by
    -- CreateSqlInjectionMatchSet and by ListSqlInjectionMatchSets.
    sqlInjectionMatchSetId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSqlInjectionMatchSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sqlInjectionMatchSetId', 'getSqlInjectionMatchSet_sqlInjectionMatchSetId' - The @SqlInjectionMatchSetId@ of the SqlInjectionMatchSet that you want
-- to get. @SqlInjectionMatchSetId@ is returned by
-- CreateSqlInjectionMatchSet and by ListSqlInjectionMatchSets.
newGetSqlInjectionMatchSet ::
  -- | 'sqlInjectionMatchSetId'
  Core.Text ->
  GetSqlInjectionMatchSet
newGetSqlInjectionMatchSet pSqlInjectionMatchSetId_ =
  GetSqlInjectionMatchSet'
    { sqlInjectionMatchSetId =
        pSqlInjectionMatchSetId_
    }

-- | The @SqlInjectionMatchSetId@ of the SqlInjectionMatchSet that you want
-- to get. @SqlInjectionMatchSetId@ is returned by
-- CreateSqlInjectionMatchSet and by ListSqlInjectionMatchSets.
getSqlInjectionMatchSet_sqlInjectionMatchSetId :: Lens.Lens' GetSqlInjectionMatchSet Core.Text
getSqlInjectionMatchSet_sqlInjectionMatchSetId = Lens.lens (\GetSqlInjectionMatchSet' {sqlInjectionMatchSetId} -> sqlInjectionMatchSetId) (\s@GetSqlInjectionMatchSet' {} a -> s {sqlInjectionMatchSetId = a} :: GetSqlInjectionMatchSet)

instance Core.AWSRequest GetSqlInjectionMatchSet where
  type
    AWSResponse GetSqlInjectionMatchSet =
      GetSqlInjectionMatchSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSqlInjectionMatchSetResponse'
            Core.<$> (x Core..?> "SqlInjectionMatchSet")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetSqlInjectionMatchSet

instance Core.NFData GetSqlInjectionMatchSet

instance Core.ToHeaders GetSqlInjectionMatchSet where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_20150824.GetSqlInjectionMatchSet" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetSqlInjectionMatchSet where
  toJSON GetSqlInjectionMatchSet' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "SqlInjectionMatchSetId"
                  Core..= sqlInjectionMatchSetId
              )
          ]
      )

instance Core.ToPath GetSqlInjectionMatchSet where
  toPath = Core.const "/"

instance Core.ToQuery GetSqlInjectionMatchSet where
  toQuery = Core.const Core.mempty

-- | The response to a GetSqlInjectionMatchSet request.
--
-- /See:/ 'newGetSqlInjectionMatchSetResponse' smart constructor.
data GetSqlInjectionMatchSetResponse = GetSqlInjectionMatchSetResponse'
  { -- | Information about the SqlInjectionMatchSet that you specified in the
    -- @GetSqlInjectionMatchSet@ request. For more information, see the
    -- following topics:
    --
    -- -   SqlInjectionMatchSet: Contains @Name@, @SqlInjectionMatchSetId@, and
    --     an array of @SqlInjectionMatchTuple@ objects
    --
    -- -   SqlInjectionMatchTuple: Each @SqlInjectionMatchTuple@ object
    --     contains @FieldToMatch@ and @TextTransformation@
    --
    -- -   FieldToMatch: Contains @Data@ and @Type@
    sqlInjectionMatchSet :: Core.Maybe SqlInjectionMatchSet,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSqlInjectionMatchSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sqlInjectionMatchSet', 'getSqlInjectionMatchSetResponse_sqlInjectionMatchSet' - Information about the SqlInjectionMatchSet that you specified in the
-- @GetSqlInjectionMatchSet@ request. For more information, see the
-- following topics:
--
-- -   SqlInjectionMatchSet: Contains @Name@, @SqlInjectionMatchSetId@, and
--     an array of @SqlInjectionMatchTuple@ objects
--
-- -   SqlInjectionMatchTuple: Each @SqlInjectionMatchTuple@ object
--     contains @FieldToMatch@ and @TextTransformation@
--
-- -   FieldToMatch: Contains @Data@ and @Type@
--
-- 'httpStatus', 'getSqlInjectionMatchSetResponse_httpStatus' - The response's http status code.
newGetSqlInjectionMatchSetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetSqlInjectionMatchSetResponse
newGetSqlInjectionMatchSetResponse pHttpStatus_ =
  GetSqlInjectionMatchSetResponse'
    { sqlInjectionMatchSet =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the SqlInjectionMatchSet that you specified in the
-- @GetSqlInjectionMatchSet@ request. For more information, see the
-- following topics:
--
-- -   SqlInjectionMatchSet: Contains @Name@, @SqlInjectionMatchSetId@, and
--     an array of @SqlInjectionMatchTuple@ objects
--
-- -   SqlInjectionMatchTuple: Each @SqlInjectionMatchTuple@ object
--     contains @FieldToMatch@ and @TextTransformation@
--
-- -   FieldToMatch: Contains @Data@ and @Type@
getSqlInjectionMatchSetResponse_sqlInjectionMatchSet :: Lens.Lens' GetSqlInjectionMatchSetResponse (Core.Maybe SqlInjectionMatchSet)
getSqlInjectionMatchSetResponse_sqlInjectionMatchSet = Lens.lens (\GetSqlInjectionMatchSetResponse' {sqlInjectionMatchSet} -> sqlInjectionMatchSet) (\s@GetSqlInjectionMatchSetResponse' {} a -> s {sqlInjectionMatchSet = a} :: GetSqlInjectionMatchSetResponse)

-- | The response's http status code.
getSqlInjectionMatchSetResponse_httpStatus :: Lens.Lens' GetSqlInjectionMatchSetResponse Core.Int
getSqlInjectionMatchSetResponse_httpStatus = Lens.lens (\GetSqlInjectionMatchSetResponse' {httpStatus} -> httpStatus) (\s@GetSqlInjectionMatchSetResponse' {} a -> s {httpStatus = a} :: GetSqlInjectionMatchSetResponse)

instance Core.NFData GetSqlInjectionMatchSetResponse
