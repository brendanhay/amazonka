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
-- Module      : Amazonka.WAFRegional.GetXssMatchSet
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
-- Returns the XssMatchSet that is specified by @XssMatchSetId@.
module Amazonka.WAFRegional.GetXssMatchSet
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFRegional.Types

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetXssMatchSetResponse'
            Prelude.<$> (x Data..?> "XssMatchSet")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetXssMatchSet where
  hashWithSalt _salt GetXssMatchSet' {..} =
    _salt `Prelude.hashWithSalt` xssMatchSetId

instance Prelude.NFData GetXssMatchSet where
  rnf GetXssMatchSet' {..} = Prelude.rnf xssMatchSetId

instance Data.ToHeaders GetXssMatchSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_Regional_20161128.GetXssMatchSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetXssMatchSet where
  toJSON GetXssMatchSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("XssMatchSetId" Data..= xssMatchSetId)
          ]
      )

instance Data.ToPath GetXssMatchSet where
  toPath = Prelude.const "/"

instance Data.ToQuery GetXssMatchSet where
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

instance Prelude.NFData GetXssMatchSetResponse where
  rnf GetXssMatchSetResponse' {..} =
    Prelude.rnf xssMatchSet
      `Prelude.seq` Prelude.rnf httpStatus
