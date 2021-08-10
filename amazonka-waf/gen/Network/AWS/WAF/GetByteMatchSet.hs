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
-- Module      : Network.AWS.WAF.GetByteMatchSet
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
module Network.AWS.WAF.GetByteMatchSet
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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | /See:/ 'newGetByteMatchSet' smart constructor.
data GetByteMatchSet = GetByteMatchSet'
  { -- | The @ByteMatchSetId@ of the ByteMatchSet that you want to get.
    -- @ByteMatchSetId@ is returned by CreateByteMatchSet and by
    -- ListByteMatchSets.
    byteMatchSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetByteMatchSet
newGetByteMatchSet pByteMatchSetId_ =
  GetByteMatchSet' {byteMatchSetId = pByteMatchSetId_}

-- | The @ByteMatchSetId@ of the ByteMatchSet that you want to get.
-- @ByteMatchSetId@ is returned by CreateByteMatchSet and by
-- ListByteMatchSets.
getByteMatchSet_byteMatchSetId :: Lens.Lens' GetByteMatchSet Prelude.Text
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
            Prelude.<$> (x Core..?> "ByteMatchSet")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetByteMatchSet

instance Prelude.NFData GetByteMatchSet

instance Core.ToHeaders GetByteMatchSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_20150824.GetByteMatchSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetByteMatchSet where
  toJSON GetByteMatchSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ByteMatchSetId" Core..= byteMatchSetId)
          ]
      )

instance Core.ToPath GetByteMatchSet where
  toPath = Prelude.const "/"

instance Core.ToQuery GetByteMatchSet where
  toQuery = Prelude.const Prelude.mempty

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
    byteMatchSet :: Prelude.Maybe ByteMatchSet,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetByteMatchSetResponse
newGetByteMatchSetResponse pHttpStatus_ =
  GetByteMatchSetResponse'
    { byteMatchSet =
        Prelude.Nothing,
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
getByteMatchSetResponse_byteMatchSet :: Lens.Lens' GetByteMatchSetResponse (Prelude.Maybe ByteMatchSet)
getByteMatchSetResponse_byteMatchSet = Lens.lens (\GetByteMatchSetResponse' {byteMatchSet} -> byteMatchSet) (\s@GetByteMatchSetResponse' {} a -> s {byteMatchSet = a} :: GetByteMatchSetResponse)

-- | The response's http status code.
getByteMatchSetResponse_httpStatus :: Lens.Lens' GetByteMatchSetResponse Prelude.Int
getByteMatchSetResponse_httpStatus = Lens.lens (\GetByteMatchSetResponse' {httpStatus} -> httpStatus) (\s@GetByteMatchSetResponse' {} a -> s {httpStatus = a} :: GetByteMatchSetResponse)

instance Prelude.NFData GetByteMatchSetResponse
