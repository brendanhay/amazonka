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
-- Module      : Amazonka.Route53Domains.RetrieveDomainAuthCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns the authorization code for the domain. To
-- transfer a domain to another registrar, you provide this value to the
-- new registrar.
module Amazonka.Route53Domains.RetrieveDomainAuthCode
  ( -- * Creating a Request
    RetrieveDomainAuthCode (..),
    newRetrieveDomainAuthCode,

    -- * Request Lenses
    retrieveDomainAuthCode_domainName,

    -- * Destructuring the Response
    RetrieveDomainAuthCodeResponse (..),
    newRetrieveDomainAuthCodeResponse,

    -- * Response Lenses
    retrieveDomainAuthCodeResponse_authCode,
    retrieveDomainAuthCodeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Domains.Types

-- | A request for the authorization code for the specified domain. To
-- transfer a domain to another registrar, you provide this value to the
-- new registrar.
--
-- /See:/ 'newRetrieveDomainAuthCode' smart constructor.
data RetrieveDomainAuthCode = RetrieveDomainAuthCode'
  { -- | The name of the domain that you want to get an authorization code for.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  RetrieveDomainAuthCode
newRetrieveDomainAuthCode pDomainName_ =
  RetrieveDomainAuthCode' {domainName = pDomainName_}

-- | The name of the domain that you want to get an authorization code for.
retrieveDomainAuthCode_domainName :: Lens.Lens' RetrieveDomainAuthCode Prelude.Text
retrieveDomainAuthCode_domainName = Lens.lens (\RetrieveDomainAuthCode' {domainName} -> domainName) (\s@RetrieveDomainAuthCode' {} a -> s {domainName = a} :: RetrieveDomainAuthCode)

instance Core.AWSRequest RetrieveDomainAuthCode where
  type
    AWSResponse RetrieveDomainAuthCode =
      RetrieveDomainAuthCodeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RetrieveDomainAuthCodeResponse'
            Prelude.<$> (x Data..?> "AuthCode")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RetrieveDomainAuthCode where
  hashWithSalt _salt RetrieveDomainAuthCode' {..} =
    _salt `Prelude.hashWithSalt` domainName

instance Prelude.NFData RetrieveDomainAuthCode where
  rnf RetrieveDomainAuthCode' {..} =
    Prelude.rnf domainName

instance Data.ToHeaders RetrieveDomainAuthCode where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Domains_v20140515.RetrieveDomainAuthCode" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RetrieveDomainAuthCode where
  toJSON RetrieveDomainAuthCode' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("DomainName" Data..= domainName)]
      )

instance Data.ToPath RetrieveDomainAuthCode where
  toPath = Prelude.const "/"

instance Data.ToQuery RetrieveDomainAuthCode where
  toQuery = Prelude.const Prelude.mempty

-- | The RetrieveDomainAuthCode response includes the following element.
--
-- /See:/ 'newRetrieveDomainAuthCodeResponse' smart constructor.
data RetrieveDomainAuthCodeResponse = RetrieveDomainAuthCodeResponse'
  { -- | The authorization code for the domain.
    authCode :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RetrieveDomainAuthCodeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authCode', 'retrieveDomainAuthCodeResponse_authCode' - The authorization code for the domain.
--
-- 'httpStatus', 'retrieveDomainAuthCodeResponse_httpStatus' - The response's http status code.
newRetrieveDomainAuthCodeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RetrieveDomainAuthCodeResponse
newRetrieveDomainAuthCodeResponse pHttpStatus_ =
  RetrieveDomainAuthCodeResponse'
    { authCode =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The authorization code for the domain.
retrieveDomainAuthCodeResponse_authCode :: Lens.Lens' RetrieveDomainAuthCodeResponse (Prelude.Maybe Prelude.Text)
retrieveDomainAuthCodeResponse_authCode = Lens.lens (\RetrieveDomainAuthCodeResponse' {authCode} -> authCode) (\s@RetrieveDomainAuthCodeResponse' {} a -> s {authCode = a} :: RetrieveDomainAuthCodeResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The response's http status code.
retrieveDomainAuthCodeResponse_httpStatus :: Lens.Lens' RetrieveDomainAuthCodeResponse Prelude.Int
retrieveDomainAuthCodeResponse_httpStatus = Lens.lens (\RetrieveDomainAuthCodeResponse' {httpStatus} -> httpStatus) (\s@RetrieveDomainAuthCodeResponse' {} a -> s {httpStatus = a} :: RetrieveDomainAuthCodeResponse)

instance
  Prelude.NFData
    RetrieveDomainAuthCodeResponse
  where
  rnf RetrieveDomainAuthCodeResponse' {..} =
    Prelude.rnf authCode `Prelude.seq`
      Prelude.rnf httpStatus
