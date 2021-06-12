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
-- Module      : Network.AWS.WAF.GetSizeConstraintSet
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
-- Returns the SizeConstraintSet specified by @SizeConstraintSetId@.
module Network.AWS.WAF.GetSizeConstraintSet
  ( -- * Creating a Request
    GetSizeConstraintSet (..),
    newGetSizeConstraintSet,

    -- * Request Lenses
    getSizeConstraintSet_sizeConstraintSetId,

    -- * Destructuring the Response
    GetSizeConstraintSetResponse (..),
    newGetSizeConstraintSetResponse,

    -- * Response Lenses
    getSizeConstraintSetResponse_sizeConstraintSet,
    getSizeConstraintSetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | /See:/ 'newGetSizeConstraintSet' smart constructor.
data GetSizeConstraintSet = GetSizeConstraintSet'
  { -- | The @SizeConstraintSetId@ of the SizeConstraintSet that you want to get.
    -- @SizeConstraintSetId@ is returned by CreateSizeConstraintSet and by
    -- ListSizeConstraintSets.
    sizeConstraintSetId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSizeConstraintSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sizeConstraintSetId', 'getSizeConstraintSet_sizeConstraintSetId' - The @SizeConstraintSetId@ of the SizeConstraintSet that you want to get.
-- @SizeConstraintSetId@ is returned by CreateSizeConstraintSet and by
-- ListSizeConstraintSets.
newGetSizeConstraintSet ::
  -- | 'sizeConstraintSetId'
  Core.Text ->
  GetSizeConstraintSet
newGetSizeConstraintSet pSizeConstraintSetId_ =
  GetSizeConstraintSet'
    { sizeConstraintSetId =
        pSizeConstraintSetId_
    }

-- | The @SizeConstraintSetId@ of the SizeConstraintSet that you want to get.
-- @SizeConstraintSetId@ is returned by CreateSizeConstraintSet and by
-- ListSizeConstraintSets.
getSizeConstraintSet_sizeConstraintSetId :: Lens.Lens' GetSizeConstraintSet Core.Text
getSizeConstraintSet_sizeConstraintSetId = Lens.lens (\GetSizeConstraintSet' {sizeConstraintSetId} -> sizeConstraintSetId) (\s@GetSizeConstraintSet' {} a -> s {sizeConstraintSetId = a} :: GetSizeConstraintSet)

instance Core.AWSRequest GetSizeConstraintSet where
  type
    AWSResponse GetSizeConstraintSet =
      GetSizeConstraintSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSizeConstraintSetResponse'
            Core.<$> (x Core..?> "SizeConstraintSet")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetSizeConstraintSet

instance Core.NFData GetSizeConstraintSet

instance Core.ToHeaders GetSizeConstraintSet where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_20150824.GetSizeConstraintSet" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetSizeConstraintSet where
  toJSON GetSizeConstraintSet' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("SizeConstraintSetId" Core..= sizeConstraintSetId)
          ]
      )

instance Core.ToPath GetSizeConstraintSet where
  toPath = Core.const "/"

instance Core.ToQuery GetSizeConstraintSet where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetSizeConstraintSetResponse' smart constructor.
data GetSizeConstraintSetResponse = GetSizeConstraintSetResponse'
  { -- | Information about the SizeConstraintSet that you specified in the
    -- @GetSizeConstraintSet@ request. For more information, see the following
    -- topics:
    --
    -- -   SizeConstraintSet: Contains @SizeConstraintSetId@,
    --     @SizeConstraints@, and @Name@
    --
    -- -   @SizeConstraints@: Contains an array of SizeConstraint objects. Each
    --     @SizeConstraint@ object contains FieldToMatch, @TextTransformation@,
    --     @ComparisonOperator@, and @Size@
    --
    -- -   FieldToMatch: Contains @Data@ and @Type@
    sizeConstraintSet :: Core.Maybe SizeConstraintSet,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSizeConstraintSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sizeConstraintSet', 'getSizeConstraintSetResponse_sizeConstraintSet' - Information about the SizeConstraintSet that you specified in the
-- @GetSizeConstraintSet@ request. For more information, see the following
-- topics:
--
-- -   SizeConstraintSet: Contains @SizeConstraintSetId@,
--     @SizeConstraints@, and @Name@
--
-- -   @SizeConstraints@: Contains an array of SizeConstraint objects. Each
--     @SizeConstraint@ object contains FieldToMatch, @TextTransformation@,
--     @ComparisonOperator@, and @Size@
--
-- -   FieldToMatch: Contains @Data@ and @Type@
--
-- 'httpStatus', 'getSizeConstraintSetResponse_httpStatus' - The response's http status code.
newGetSizeConstraintSetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetSizeConstraintSetResponse
newGetSizeConstraintSetResponse pHttpStatus_ =
  GetSizeConstraintSetResponse'
    { sizeConstraintSet =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the SizeConstraintSet that you specified in the
-- @GetSizeConstraintSet@ request. For more information, see the following
-- topics:
--
-- -   SizeConstraintSet: Contains @SizeConstraintSetId@,
--     @SizeConstraints@, and @Name@
--
-- -   @SizeConstraints@: Contains an array of SizeConstraint objects. Each
--     @SizeConstraint@ object contains FieldToMatch, @TextTransformation@,
--     @ComparisonOperator@, and @Size@
--
-- -   FieldToMatch: Contains @Data@ and @Type@
getSizeConstraintSetResponse_sizeConstraintSet :: Lens.Lens' GetSizeConstraintSetResponse (Core.Maybe SizeConstraintSet)
getSizeConstraintSetResponse_sizeConstraintSet = Lens.lens (\GetSizeConstraintSetResponse' {sizeConstraintSet} -> sizeConstraintSet) (\s@GetSizeConstraintSetResponse' {} a -> s {sizeConstraintSet = a} :: GetSizeConstraintSetResponse)

-- | The response's http status code.
getSizeConstraintSetResponse_httpStatus :: Lens.Lens' GetSizeConstraintSetResponse Core.Int
getSizeConstraintSetResponse_httpStatus = Lens.lens (\GetSizeConstraintSetResponse' {httpStatus} -> httpStatus) (\s@GetSizeConstraintSetResponse' {} a -> s {httpStatus = a} :: GetSizeConstraintSetResponse)

instance Core.NFData GetSizeConstraintSetResponse
