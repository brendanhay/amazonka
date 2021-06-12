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
-- Module      : Network.AWS.WAF.GetChangeTokenStatus
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
module Network.AWS.WAF.GetChangeTokenStatus
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | /See:/ 'newGetChangeTokenStatus' smart constructor.
data GetChangeTokenStatus = GetChangeTokenStatus'
  { -- | The change token for which you want to get the status. This change token
    -- was previously returned in the @GetChangeToken@ response.
    changeToken :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetChangeTokenStatus
newGetChangeTokenStatus pChangeToken_ =
  GetChangeTokenStatus' {changeToken = pChangeToken_}

-- | The change token for which you want to get the status. This change token
-- was previously returned in the @GetChangeToken@ response.
getChangeTokenStatus_changeToken :: Lens.Lens' GetChangeTokenStatus Core.Text
getChangeTokenStatus_changeToken = Lens.lens (\GetChangeTokenStatus' {changeToken} -> changeToken) (\s@GetChangeTokenStatus' {} a -> s {changeToken = a} :: GetChangeTokenStatus)

instance Core.AWSRequest GetChangeTokenStatus where
  type
    AWSResponse GetChangeTokenStatus =
      GetChangeTokenStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetChangeTokenStatusResponse'
            Core.<$> (x Core..?> "ChangeTokenStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetChangeTokenStatus

instance Core.NFData GetChangeTokenStatus

instance Core.ToHeaders GetChangeTokenStatus where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_20150824.GetChangeTokenStatus" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetChangeTokenStatus where
  toJSON GetChangeTokenStatus' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ChangeToken" Core..= changeToken)]
      )

instance Core.ToPath GetChangeTokenStatus where
  toPath = Core.const "/"

instance Core.ToQuery GetChangeTokenStatus where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetChangeTokenStatusResponse' smart constructor.
data GetChangeTokenStatusResponse = GetChangeTokenStatusResponse'
  { -- | The status of the change token.
    changeTokenStatus :: Core.Maybe ChangeTokenStatus,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetChangeTokenStatusResponse
newGetChangeTokenStatusResponse pHttpStatus_ =
  GetChangeTokenStatusResponse'
    { changeTokenStatus =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the change token.
getChangeTokenStatusResponse_changeTokenStatus :: Lens.Lens' GetChangeTokenStatusResponse (Core.Maybe ChangeTokenStatus)
getChangeTokenStatusResponse_changeTokenStatus = Lens.lens (\GetChangeTokenStatusResponse' {changeTokenStatus} -> changeTokenStatus) (\s@GetChangeTokenStatusResponse' {} a -> s {changeTokenStatus = a} :: GetChangeTokenStatusResponse)

-- | The response's http status code.
getChangeTokenStatusResponse_httpStatus :: Lens.Lens' GetChangeTokenStatusResponse Core.Int
getChangeTokenStatusResponse_httpStatus = Lens.lens (\GetChangeTokenStatusResponse' {httpStatus} -> httpStatus) (\s@GetChangeTokenStatusResponse' {} a -> s {httpStatus = a} :: GetChangeTokenStatusResponse)

instance Core.NFData GetChangeTokenStatusResponse
