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
-- Module      : Network.AWS.WAFRegional.GetByteMatchSet
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
-- Returns the ByteMatchSet specified by @ByteMatchSetId@.
module Network.AWS.WAFRegional.GetByteMatchSet
  ( -- * Creating a Request
    GetByteMatchSet (..),
    newGetByteMatchSet,

    -- * Request Lenses
    getByteMatchSet_byteMatchSetId,

    -- * Destructuring the Response
    GetByteMatchSetResponse (..),
    newGetByteMatchSetResponse,

    -- * Response Lenses
    getByteMatchSetResponse_byteMatchSet,
    getByteMatchSetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAFRegional.Types

-- | /See:/ 'newGetByteMatchSet' smart constructor.
data GetByteMatchSet = GetByteMatchSet'
  { -- | The @ByteMatchSetId@ of the ByteMatchSet that you want to get.
    -- @ByteMatchSetId@ is returned by CreateByteMatchSet and by
    -- ListByteMatchSets.
    byteMatchSetId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetByteMatchSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'byteMatchSetId', 'getByteMatchSet_byteMatchSetId' - The @ByteMatchSetId@ of the ByteMatchSet that you want to get.
-- @ByteMatchSetId@ is returned by CreateByteMatchSet and by
-- ListByteMatchSets.
newGetByteMatchSet ::
  -- | 'byteMatchSetId'
  Core.Text ->
  GetByteMatchSet
newGetByteMatchSet pByteMatchSetId_ =
  GetByteMatchSet' {byteMatchSetId = pByteMatchSetId_}

-- | The @ByteMatchSetId@ of the ByteMatchSet that you want to get.
-- @ByteMatchSetId@ is returned by CreateByteMatchSet and by
-- ListByteMatchSets.
getByteMatchSet_byteMatchSetId :: Lens.Lens' GetByteMatchSet Core.Text
getByteMatchSet_byteMatchSetId = Lens.lens (\GetByteMatchSet' {byteMatchSetId} -> byteMatchSetId) (\s@GetByteMatchSet' {} a -> s {byteMatchSetId = a} :: GetByteMatchSet)

instance Core.AWSRequest GetByteMatchSet where
  type
    AWSResponse GetByteMatchSet =
      GetByteMatchSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetByteMatchSetResponse'
            Core.<$> (x Core..?> "ByteMatchSet")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetByteMatchSet

instance Core.NFData GetByteMatchSet

instance Core.ToHeaders GetByteMatchSet where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_Regional_20161128.GetByteMatchSet" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetByteMatchSet where
  toJSON GetByteMatchSet' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ByteMatchSetId" Core..= byteMatchSetId)
          ]
      )

instance Core.ToPath GetByteMatchSet where
  toPath = Core.const "/"

instance Core.ToQuery GetByteMatchSet where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetByteMatchSetResponse' smart constructor.
data GetByteMatchSetResponse = GetByteMatchSetResponse'
  { -- | Information about the ByteMatchSet that you specified in the
    -- @GetByteMatchSet@ request. For more information, see the following
    -- topics:
    --
    -- -   ByteMatchSet: Contains @ByteMatchSetId@, @ByteMatchTuples@, and
    --     @Name@
    --
    -- -   @ByteMatchTuples@: Contains an array of ByteMatchTuple objects. Each
    --     @ByteMatchTuple@ object contains FieldToMatch,
    --     @PositionalConstraint@, @TargetString@, and @TextTransformation@
    --
    -- -   FieldToMatch: Contains @Data@ and @Type@
    byteMatchSet :: Core.Maybe ByteMatchSet,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetByteMatchSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'byteMatchSet', 'getByteMatchSetResponse_byteMatchSet' - Information about the ByteMatchSet that you specified in the
-- @GetByteMatchSet@ request. For more information, see the following
-- topics:
--
-- -   ByteMatchSet: Contains @ByteMatchSetId@, @ByteMatchTuples@, and
--     @Name@
--
-- -   @ByteMatchTuples@: Contains an array of ByteMatchTuple objects. Each
--     @ByteMatchTuple@ object contains FieldToMatch,
--     @PositionalConstraint@, @TargetString@, and @TextTransformation@
--
-- -   FieldToMatch: Contains @Data@ and @Type@
--
-- 'httpStatus', 'getByteMatchSetResponse_httpStatus' - The response's http status code.
newGetByteMatchSetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetByteMatchSetResponse
newGetByteMatchSetResponse pHttpStatus_ =
  GetByteMatchSetResponse'
    { byteMatchSet =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the ByteMatchSet that you specified in the
-- @GetByteMatchSet@ request. For more information, see the following
-- topics:
--
-- -   ByteMatchSet: Contains @ByteMatchSetId@, @ByteMatchTuples@, and
--     @Name@
--
-- -   @ByteMatchTuples@: Contains an array of ByteMatchTuple objects. Each
--     @ByteMatchTuple@ object contains FieldToMatch,
--     @PositionalConstraint@, @TargetString@, and @TextTransformation@
--
-- -   FieldToMatch: Contains @Data@ and @Type@
getByteMatchSetResponse_byteMatchSet :: Lens.Lens' GetByteMatchSetResponse (Core.Maybe ByteMatchSet)
getByteMatchSetResponse_byteMatchSet = Lens.lens (\GetByteMatchSetResponse' {byteMatchSet} -> byteMatchSet) (\s@GetByteMatchSetResponse' {} a -> s {byteMatchSet = a} :: GetByteMatchSetResponse)

-- | The response's http status code.
getByteMatchSetResponse_httpStatus :: Lens.Lens' GetByteMatchSetResponse Core.Int
getByteMatchSetResponse_httpStatus = Lens.lens (\GetByteMatchSetResponse' {httpStatus} -> httpStatus) (\s@GetByteMatchSetResponse' {} a -> s {httpStatus = a} :: GetByteMatchSetResponse)

instance Core.NFData GetByteMatchSetResponse
