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
-- Module      : Network.AWS.WAF.GetXssMatchSet
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
-- Returns the XssMatchSet that is specified by @XssMatchSetId@.
module Network.AWS.WAF.GetXssMatchSet
  ( -- * Creating a Request
    GetXssMatchSet (..),
    newGetXssMatchSet,

    -- * Request Lenses
    getXssMatchSet_xssMatchSetId,

    -- * Destructuring the Response
    GetXssMatchSetResponse (..),
    newGetXssMatchSetResponse,

    -- * Response Lenses
    getXssMatchSetResponse_xssMatchSet,
    getXssMatchSetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | A request to get an XssMatchSet.
--
-- /See:/ 'newGetXssMatchSet' smart constructor.
data GetXssMatchSet = GetXssMatchSet'
  { -- | The @XssMatchSetId@ of the XssMatchSet that you want to get.
    -- @XssMatchSetId@ is returned by CreateXssMatchSet and by
    -- ListXssMatchSets.
    xssMatchSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetXssMatchSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'xssMatchSetId', 'getXssMatchSet_xssMatchSetId' - The @XssMatchSetId@ of the XssMatchSet that you want to get.
-- @XssMatchSetId@ is returned by CreateXssMatchSet and by
-- ListXssMatchSets.
newGetXssMatchSet ::
  -- | 'xssMatchSetId'
  Prelude.Text ->
  GetXssMatchSet
newGetXssMatchSet pXssMatchSetId_ =
  GetXssMatchSet' {xssMatchSetId = pXssMatchSetId_}

-- | The @XssMatchSetId@ of the XssMatchSet that you want to get.
-- @XssMatchSetId@ is returned by CreateXssMatchSet and by
-- ListXssMatchSets.
getXssMatchSet_xssMatchSetId :: Lens.Lens' GetXssMatchSet Prelude.Text
getXssMatchSet_xssMatchSetId = Lens.lens (\GetXssMatchSet' {xssMatchSetId} -> xssMatchSetId) (\s@GetXssMatchSet' {} a -> s {xssMatchSetId = a} :: GetXssMatchSet)

instance Core.AWSRequest GetXssMatchSet where
  type
    AWSResponse GetXssMatchSet =
      GetXssMatchSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetXssMatchSetResponse'
            Prelude.<$> (x Core..?> "XssMatchSet")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetXssMatchSet

instance Prelude.NFData GetXssMatchSet

instance Core.ToHeaders GetXssMatchSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_20150824.GetXssMatchSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetXssMatchSet where
  toJSON GetXssMatchSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("XssMatchSetId" Core..= xssMatchSetId)
          ]
      )

instance Core.ToPath GetXssMatchSet where
  toPath = Prelude.const "/"

instance Core.ToQuery GetXssMatchSet where
  toQuery = Prelude.const Prelude.mempty

-- | The response to a GetXssMatchSet request.
--
-- /See:/ 'newGetXssMatchSetResponse' smart constructor.
data GetXssMatchSetResponse = GetXssMatchSetResponse'
  { -- | Information about the XssMatchSet that you specified in the
    -- @GetXssMatchSet@ request. For more information, see the following
    -- topics:
    --
    -- -   XssMatchSet: Contains @Name@, @XssMatchSetId@, and an array of
    --     @XssMatchTuple@ objects
    --
    -- -   XssMatchTuple: Each @XssMatchTuple@ object contains @FieldToMatch@
    --     and @TextTransformation@
    --
    -- -   FieldToMatch: Contains @Data@ and @Type@
    xssMatchSet :: Prelude.Maybe XssMatchSet,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetXssMatchSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'xssMatchSet', 'getXssMatchSetResponse_xssMatchSet' - Information about the XssMatchSet that you specified in the
-- @GetXssMatchSet@ request. For more information, see the following
-- topics:
--
-- -   XssMatchSet: Contains @Name@, @XssMatchSetId@, and an array of
--     @XssMatchTuple@ objects
--
-- -   XssMatchTuple: Each @XssMatchTuple@ object contains @FieldToMatch@
--     and @TextTransformation@
--
-- -   FieldToMatch: Contains @Data@ and @Type@
--
-- 'httpStatus', 'getXssMatchSetResponse_httpStatus' - The response's http status code.
newGetXssMatchSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetXssMatchSetResponse
newGetXssMatchSetResponse pHttpStatus_ =
  GetXssMatchSetResponse'
    { xssMatchSet =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the XssMatchSet that you specified in the
-- @GetXssMatchSet@ request. For more information, see the following
-- topics:
--
-- -   XssMatchSet: Contains @Name@, @XssMatchSetId@, and an array of
--     @XssMatchTuple@ objects
--
-- -   XssMatchTuple: Each @XssMatchTuple@ object contains @FieldToMatch@
--     and @TextTransformation@
--
-- -   FieldToMatch: Contains @Data@ and @Type@
getXssMatchSetResponse_xssMatchSet :: Lens.Lens' GetXssMatchSetResponse (Prelude.Maybe XssMatchSet)
getXssMatchSetResponse_xssMatchSet = Lens.lens (\GetXssMatchSetResponse' {xssMatchSet} -> xssMatchSet) (\s@GetXssMatchSetResponse' {} a -> s {xssMatchSet = a} :: GetXssMatchSetResponse)

-- | The response's http status code.
getXssMatchSetResponse_httpStatus :: Lens.Lens' GetXssMatchSetResponse Prelude.Int
getXssMatchSetResponse_httpStatus = Lens.lens (\GetXssMatchSetResponse' {httpStatus} -> httpStatus) (\s@GetXssMatchSetResponse' {} a -> s {httpStatus = a} :: GetXssMatchSetResponse)

instance Prelude.NFData GetXssMatchSetResponse
