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
-- Module      : Network.AWS.Route53Domains.RetrieveDomainAuthCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns the AuthCode for the domain. To transfer a domain
-- to another registrar, you provide this value to the new registrar.
module Network.AWS.Route53Domains.RetrieveDomainAuthCode
  ( -- * Creating a Request
    RetrieveDomainAuthCode (..),
    newRetrieveDomainAuthCode,

    -- * Request Lenses
    retrieveDomainAuthCode_domainName,

    -- * Destructuring the Response
    RetrieveDomainAuthCodeResponse (..),
    newRetrieveDomainAuthCodeResponse,

    -- * Response Lenses
    retrieveDomainAuthCodeResponse_httpStatus,
    retrieveDomainAuthCodeResponse_authCode,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53Domains.Types

-- | A request for the authorization code for the specified domain. To
-- transfer a domain to another registrar, you provide this value to the
-- new registrar.
--
-- /See:/ 'newRetrieveDomainAuthCode' smart constructor.
data RetrieveDomainAuthCode = RetrieveDomainAuthCode'
  { -- | The name of the domain that you want to get an authorization code for.
    domainName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RetrieveDomainAuthCode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'retrieveDomainAuthCode_domainName' - The name of the domain that you want to get an authorization code for.
newRetrieveDomainAuthCode ::
  -- | 'domainName'
  Core.Text ->
  RetrieveDomainAuthCode
newRetrieveDomainAuthCode pDomainName_ =
  RetrieveDomainAuthCode' {domainName = pDomainName_}

-- | The name of the domain that you want to get an authorization code for.
retrieveDomainAuthCode_domainName :: Lens.Lens' RetrieveDomainAuthCode Core.Text
retrieveDomainAuthCode_domainName = Lens.lens (\RetrieveDomainAuthCode' {domainName} -> domainName) (\s@RetrieveDomainAuthCode' {} a -> s {domainName = a} :: RetrieveDomainAuthCode)

instance Core.AWSRequest RetrieveDomainAuthCode where
  type
    AWSResponse RetrieveDomainAuthCode =
      RetrieveDomainAuthCodeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RetrieveDomainAuthCodeResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "AuthCode")
      )

instance Core.Hashable RetrieveDomainAuthCode

instance Core.NFData RetrieveDomainAuthCode

instance Core.ToHeaders RetrieveDomainAuthCode where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53Domains_v20140515.RetrieveDomainAuthCode" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RetrieveDomainAuthCode where
  toJSON RetrieveDomainAuthCode' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("DomainName" Core..= domainName)]
      )

instance Core.ToPath RetrieveDomainAuthCode where
  toPath = Core.const "/"

instance Core.ToQuery RetrieveDomainAuthCode where
  toQuery = Core.const Core.mempty

-- | The RetrieveDomainAuthCode response includes the following element.
--
-- /See:/ 'newRetrieveDomainAuthCodeResponse' smart constructor.
data RetrieveDomainAuthCodeResponse = RetrieveDomainAuthCodeResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The authorization code for the domain.
    authCode :: Core.Sensitive Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'RetrieveDomainAuthCodeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'retrieveDomainAuthCodeResponse_httpStatus' - The response's http status code.
--
-- 'authCode', 'retrieveDomainAuthCodeResponse_authCode' - The authorization code for the domain.
newRetrieveDomainAuthCodeResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'authCode'
  Core.Text ->
  RetrieveDomainAuthCodeResponse
newRetrieveDomainAuthCodeResponse
  pHttpStatus_
  pAuthCode_ =
    RetrieveDomainAuthCodeResponse'
      { httpStatus =
          pHttpStatus_,
        authCode =
          Core._Sensitive Lens.# pAuthCode_
      }

-- | The response's http status code.
retrieveDomainAuthCodeResponse_httpStatus :: Lens.Lens' RetrieveDomainAuthCodeResponse Core.Int
retrieveDomainAuthCodeResponse_httpStatus = Lens.lens (\RetrieveDomainAuthCodeResponse' {httpStatus} -> httpStatus) (\s@RetrieveDomainAuthCodeResponse' {} a -> s {httpStatus = a} :: RetrieveDomainAuthCodeResponse)

-- | The authorization code for the domain.
retrieveDomainAuthCodeResponse_authCode :: Lens.Lens' RetrieveDomainAuthCodeResponse Core.Text
retrieveDomainAuthCodeResponse_authCode = Lens.lens (\RetrieveDomainAuthCodeResponse' {authCode} -> authCode) (\s@RetrieveDomainAuthCodeResponse' {} a -> s {authCode = a} :: RetrieveDomainAuthCodeResponse) Core.. Core._Sensitive

instance Core.NFData RetrieveDomainAuthCodeResponse
