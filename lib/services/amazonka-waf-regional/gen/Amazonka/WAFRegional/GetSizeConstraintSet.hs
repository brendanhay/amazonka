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
-- Module      : Amazonka.WAFRegional.GetSizeConstraintSet
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
-- Returns the SizeConstraintSet specified by @SizeConstraintSetId@.
module Amazonka.WAFRegional.GetSizeConstraintSet
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFRegional.Types

-- | /See:/ 'newGetSizeConstraintSet' smart constructor.
data GetSizeConstraintSet = GetSizeConstraintSet'
  { -- | The @SizeConstraintSetId@ of the SizeConstraintSet that you want to get.
    -- @SizeConstraintSetId@ is returned by CreateSizeConstraintSet and by
    -- ListSizeConstraintSets.
    sizeConstraintSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetSizeConstraintSet
newGetSizeConstraintSet pSizeConstraintSetId_ =
  GetSizeConstraintSet'
    { sizeConstraintSetId =
        pSizeConstraintSetId_
    }

-- | The @SizeConstraintSetId@ of the SizeConstraintSet that you want to get.
-- @SizeConstraintSetId@ is returned by CreateSizeConstraintSet and by
-- ListSizeConstraintSets.
getSizeConstraintSet_sizeConstraintSetId :: Lens.Lens' GetSizeConstraintSet Prelude.Text
getSizeConstraintSet_sizeConstraintSetId = Lens.lens (\GetSizeConstraintSet' {sizeConstraintSetId} -> sizeConstraintSetId) (\s@GetSizeConstraintSet' {} a -> s {sizeConstraintSetId = a} :: GetSizeConstraintSet)

instance Core.AWSRequest GetSizeConstraintSet where
  type
    AWSResponse GetSizeConstraintSet =
      GetSizeConstraintSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSizeConstraintSetResponse'
            Prelude.<$> (x Core..?> "SizeConstraintSet")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSizeConstraintSet where
  hashWithSalt _salt GetSizeConstraintSet' {..} =
    _salt `Prelude.hashWithSalt` sizeConstraintSetId

instance Prelude.NFData GetSizeConstraintSet where
  rnf GetSizeConstraintSet' {..} =
    Prelude.rnf sizeConstraintSetId

instance Core.ToHeaders GetSizeConstraintSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_Regional_20161128.GetSizeConstraintSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetSizeConstraintSet where
  toJSON GetSizeConstraintSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SizeConstraintSetId" Core..= sizeConstraintSetId)
          ]
      )

instance Core.ToPath GetSizeConstraintSet where
  toPath = Prelude.const "/"

instance Core.ToQuery GetSizeConstraintSet where
  toQuery = Prelude.const Prelude.mempty

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
    sizeConstraintSet :: Prelude.Maybe SizeConstraintSet,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetSizeConstraintSetResponse
newGetSizeConstraintSetResponse pHttpStatus_ =
  GetSizeConstraintSetResponse'
    { sizeConstraintSet =
        Prelude.Nothing,
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
getSizeConstraintSetResponse_sizeConstraintSet :: Lens.Lens' GetSizeConstraintSetResponse (Prelude.Maybe SizeConstraintSet)
getSizeConstraintSetResponse_sizeConstraintSet = Lens.lens (\GetSizeConstraintSetResponse' {sizeConstraintSet} -> sizeConstraintSet) (\s@GetSizeConstraintSetResponse' {} a -> s {sizeConstraintSet = a} :: GetSizeConstraintSetResponse)

-- | The response's http status code.
getSizeConstraintSetResponse_httpStatus :: Lens.Lens' GetSizeConstraintSetResponse Prelude.Int
getSizeConstraintSetResponse_httpStatus = Lens.lens (\GetSizeConstraintSetResponse' {httpStatus} -> httpStatus) (\s@GetSizeConstraintSetResponse' {} a -> s {httpStatus = a} :: GetSizeConstraintSetResponse)

instance Prelude.NFData GetSizeConstraintSetResponse where
  rnf GetSizeConstraintSetResponse' {..} =
    Prelude.rnf sizeConstraintSet
      `Prelude.seq` Prelude.rnf httpStatus
