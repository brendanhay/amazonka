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
-- Module      : Amazonka.WAFRegional.GetChangeTokenStatus
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
-- Returns the status of a @ChangeToken@ that you got by calling
-- GetChangeToken. @ChangeTokenStatus@ is one of the following values:
--
-- -   @PROVISIONED@: You requested the change token by calling
--     @GetChangeToken@, but you haven\'t used it yet in a call to create,
--     update, or delete an AWS WAF object.
--
-- -   @PENDING@: AWS WAF is propagating the create, update, or delete
--     request to all AWS WAF servers.
--
-- -   @INSYNC@: Propagation is complete.
module Amazonka.WAFRegional.GetChangeTokenStatus
  ( -- * Creating a Request
    GetChangeTokenStatus (..),
    newGetChangeTokenStatus,

    -- * Request Lenses
    getChangeTokenStatus_changeToken,

    -- * Destructuring the Response
    GetChangeTokenStatusResponse (..),
    newGetChangeTokenStatusResponse,

    -- * Response Lenses
    getChangeTokenStatusResponse_changeTokenStatus,
    getChangeTokenStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFRegional.Types

-- | /See:/ 'newGetChangeTokenStatus' smart constructor.
data GetChangeTokenStatus = GetChangeTokenStatus'
  { -- | The change token for which you want to get the status. This change token
    -- was previously returned in the @GetChangeToken@ response.
    changeToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetChangeTokenStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeToken', 'getChangeTokenStatus_changeToken' - The change token for which you want to get the status. This change token
-- was previously returned in the @GetChangeToken@ response.
newGetChangeTokenStatus ::
  -- | 'changeToken'
  Prelude.Text ->
  GetChangeTokenStatus
newGetChangeTokenStatus pChangeToken_ =
  GetChangeTokenStatus' {changeToken = pChangeToken_}

-- | The change token for which you want to get the status. This change token
-- was previously returned in the @GetChangeToken@ response.
getChangeTokenStatus_changeToken :: Lens.Lens' GetChangeTokenStatus Prelude.Text
getChangeTokenStatus_changeToken = Lens.lens (\GetChangeTokenStatus' {changeToken} -> changeToken) (\s@GetChangeTokenStatus' {} a -> s {changeToken = a} :: GetChangeTokenStatus)

instance Core.AWSRequest GetChangeTokenStatus where
  type
    AWSResponse GetChangeTokenStatus =
      GetChangeTokenStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetChangeTokenStatusResponse'
            Prelude.<$> (x Core..?> "ChangeTokenStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetChangeTokenStatus where
  hashWithSalt _salt GetChangeTokenStatus' {..} =
    _salt `Prelude.hashWithSalt` changeToken

instance Prelude.NFData GetChangeTokenStatus where
  rnf GetChangeTokenStatus' {..} =
    Prelude.rnf changeToken

instance Core.ToHeaders GetChangeTokenStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_Regional_20161128.GetChangeTokenStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetChangeTokenStatus where
  toJSON GetChangeTokenStatus' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ChangeToken" Core..= changeToken)]
      )

instance Core.ToPath GetChangeTokenStatus where
  toPath = Prelude.const "/"

instance Core.ToQuery GetChangeTokenStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetChangeTokenStatusResponse' smart constructor.
data GetChangeTokenStatusResponse = GetChangeTokenStatusResponse'
  { -- | The status of the change token.
    changeTokenStatus :: Prelude.Maybe ChangeTokenStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetChangeTokenStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeTokenStatus', 'getChangeTokenStatusResponse_changeTokenStatus' - The status of the change token.
--
-- 'httpStatus', 'getChangeTokenStatusResponse_httpStatus' - The response's http status code.
newGetChangeTokenStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetChangeTokenStatusResponse
newGetChangeTokenStatusResponse pHttpStatus_ =
  GetChangeTokenStatusResponse'
    { changeTokenStatus =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the change token.
getChangeTokenStatusResponse_changeTokenStatus :: Lens.Lens' GetChangeTokenStatusResponse (Prelude.Maybe ChangeTokenStatus)
getChangeTokenStatusResponse_changeTokenStatus = Lens.lens (\GetChangeTokenStatusResponse' {changeTokenStatus} -> changeTokenStatus) (\s@GetChangeTokenStatusResponse' {} a -> s {changeTokenStatus = a} :: GetChangeTokenStatusResponse)

-- | The response's http status code.
getChangeTokenStatusResponse_httpStatus :: Lens.Lens' GetChangeTokenStatusResponse Prelude.Int
getChangeTokenStatusResponse_httpStatus = Lens.lens (\GetChangeTokenStatusResponse' {httpStatus} -> httpStatus) (\s@GetChangeTokenStatusResponse' {} a -> s {httpStatus = a} :: GetChangeTokenStatusResponse)

instance Prelude.NFData GetChangeTokenStatusResponse where
  rnf GetChangeTokenStatusResponse' {..} =
    Prelude.rnf changeTokenStatus
      `Prelude.seq` Prelude.rnf httpStatus
