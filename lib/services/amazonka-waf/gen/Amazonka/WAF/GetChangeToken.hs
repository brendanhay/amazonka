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
-- Module      : Amazonka.WAF.GetChangeToken
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
-- When you want to create, update, or delete AWS WAF objects, get a change
-- token and include the change token in the create, update, or delete
-- request. Change tokens ensure that your application doesn\'t submit
-- conflicting requests to AWS WAF.
--
-- Each create, update, or delete request must use a unique change token.
-- If your application submits a @GetChangeToken@ request and then submits
-- a second @GetChangeToken@ request before submitting a create, update, or
-- delete request, the second @GetChangeToken@ request returns the same
-- value as the first @GetChangeToken@ request.
--
-- When you use a change token in a create, update, or delete request, the
-- status of the change token changes to @PENDING@, which indicates that
-- AWS WAF is propagating the change to all AWS WAF servers. Use
-- @GetChangeTokenStatus@ to determine the status of your change token.
module Amazonka.WAF.GetChangeToken
  ( -- * Creating a Request
    GetChangeToken (..),
    newGetChangeToken,

    -- * Destructuring the Response
    GetChangeTokenResponse (..),
    newGetChangeTokenResponse,

    -- * Response Lenses
    getChangeTokenResponse_changeToken,
    getChangeTokenResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAF.Types

-- | /See:/ 'newGetChangeToken' smart constructor.
data GetChangeToken = GetChangeToken'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetChangeToken' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetChangeToken ::
  GetChangeToken
newGetChangeToken = GetChangeToken'

instance Core.AWSRequest GetChangeToken where
  type
    AWSResponse GetChangeToken =
      GetChangeTokenResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetChangeTokenResponse'
            Prelude.<$> (x Data..?> "ChangeToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetChangeToken where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetChangeToken where
  rnf _ = ()

instance Data.ToHeaders GetChangeToken where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20150824.GetChangeToken" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetChangeToken where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath GetChangeToken where
  toPath = Prelude.const "/"

instance Data.ToQuery GetChangeToken where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetChangeTokenResponse' smart constructor.
data GetChangeTokenResponse = GetChangeTokenResponse'
  { -- | The @ChangeToken@ that you used in the request. Use this value in a
    -- @GetChangeTokenStatus@ request to get the current status of the request.
    changeToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetChangeTokenResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeToken', 'getChangeTokenResponse_changeToken' - The @ChangeToken@ that you used in the request. Use this value in a
-- @GetChangeTokenStatus@ request to get the current status of the request.
--
-- 'httpStatus', 'getChangeTokenResponse_httpStatus' - The response's http status code.
newGetChangeTokenResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetChangeTokenResponse
newGetChangeTokenResponse pHttpStatus_ =
  GetChangeTokenResponse'
    { changeToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ChangeToken@ that you used in the request. Use this value in a
-- @GetChangeTokenStatus@ request to get the current status of the request.
getChangeTokenResponse_changeToken :: Lens.Lens' GetChangeTokenResponse (Prelude.Maybe Prelude.Text)
getChangeTokenResponse_changeToken = Lens.lens (\GetChangeTokenResponse' {changeToken} -> changeToken) (\s@GetChangeTokenResponse' {} a -> s {changeToken = a} :: GetChangeTokenResponse)

-- | The response's http status code.
getChangeTokenResponse_httpStatus :: Lens.Lens' GetChangeTokenResponse Prelude.Int
getChangeTokenResponse_httpStatus = Lens.lens (\GetChangeTokenResponse' {httpStatus} -> httpStatus) (\s@GetChangeTokenResponse' {} a -> s {httpStatus = a} :: GetChangeTokenResponse)

instance Prelude.NFData GetChangeTokenResponse where
  rnf GetChangeTokenResponse' {..} =
    Prelude.rnf changeToken `Prelude.seq`
      Prelude.rnf httpStatus
