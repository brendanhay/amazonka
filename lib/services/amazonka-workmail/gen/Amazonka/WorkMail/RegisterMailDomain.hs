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
-- Module      : Amazonka.WorkMail.RegisterMailDomain
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a new domain in WorkMail and SES, and configures it for use by
-- WorkMail. Emails received by SES for this domain are routed to the
-- specified WorkMail organization, and WorkMail has permanent permission
-- to use the specified domain for sending your users\' emails.
module Amazonka.WorkMail.RegisterMailDomain
  ( -- * Creating a Request
    RegisterMailDomain (..),
    newRegisterMailDomain,

    -- * Request Lenses
    registerMailDomain_clientToken,
    registerMailDomain_organizationId,
    registerMailDomain_domainName,

    -- * Destructuring the Response
    RegisterMailDomainResponse (..),
    newRegisterMailDomainResponse,

    -- * Response Lenses
    registerMailDomainResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newRegisterMailDomain' smart constructor.
data RegisterMailDomain = RegisterMailDomain'
  { -- | Idempotency token used when retrying requests.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The WorkMail organization under which you\'re creating the domain.
    organizationId :: Prelude.Text,
    -- | The name of the mail domain to create in WorkMail and SES.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterMailDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'registerMailDomain_clientToken' - Idempotency token used when retrying requests.
--
-- 'organizationId', 'registerMailDomain_organizationId' - The WorkMail organization under which you\'re creating the domain.
--
-- 'domainName', 'registerMailDomain_domainName' - The name of the mail domain to create in WorkMail and SES.
newRegisterMailDomain ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'domainName'
  Prelude.Text ->
  RegisterMailDomain
newRegisterMailDomain pOrganizationId_ pDomainName_ =
  RegisterMailDomain'
    { clientToken = Prelude.Nothing,
      organizationId = pOrganizationId_,
      domainName = pDomainName_
    }

-- | Idempotency token used when retrying requests.
registerMailDomain_clientToken :: Lens.Lens' RegisterMailDomain (Prelude.Maybe Prelude.Text)
registerMailDomain_clientToken = Lens.lens (\RegisterMailDomain' {clientToken} -> clientToken) (\s@RegisterMailDomain' {} a -> s {clientToken = a} :: RegisterMailDomain)

-- | The WorkMail organization under which you\'re creating the domain.
registerMailDomain_organizationId :: Lens.Lens' RegisterMailDomain Prelude.Text
registerMailDomain_organizationId = Lens.lens (\RegisterMailDomain' {organizationId} -> organizationId) (\s@RegisterMailDomain' {} a -> s {organizationId = a} :: RegisterMailDomain)

-- | The name of the mail domain to create in WorkMail and SES.
registerMailDomain_domainName :: Lens.Lens' RegisterMailDomain Prelude.Text
registerMailDomain_domainName = Lens.lens (\RegisterMailDomain' {domainName} -> domainName) (\s@RegisterMailDomain' {} a -> s {domainName = a} :: RegisterMailDomain)

instance Core.AWSRequest RegisterMailDomain where
  type
    AWSResponse RegisterMailDomain =
      RegisterMailDomainResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RegisterMailDomainResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterMailDomain where
  hashWithSalt _salt RegisterMailDomain' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData RegisterMailDomain where
  rnf RegisterMailDomain' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf domainName

instance Core.ToHeaders RegisterMailDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkMailService.RegisterMailDomain" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RegisterMailDomain where
  toJSON RegisterMailDomain' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ClientToken" Core..=) Prelude.<$> clientToken,
            Prelude.Just
              ("OrganizationId" Core..= organizationId),
            Prelude.Just ("DomainName" Core..= domainName)
          ]
      )

instance Core.ToPath RegisterMailDomain where
  toPath = Prelude.const "/"

instance Core.ToQuery RegisterMailDomain where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterMailDomainResponse' smart constructor.
data RegisterMailDomainResponse = RegisterMailDomainResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterMailDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'registerMailDomainResponse_httpStatus' - The response's http status code.
newRegisterMailDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterMailDomainResponse
newRegisterMailDomainResponse pHttpStatus_ =
  RegisterMailDomainResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
registerMailDomainResponse_httpStatus :: Lens.Lens' RegisterMailDomainResponse Prelude.Int
registerMailDomainResponse_httpStatus = Lens.lens (\RegisterMailDomainResponse' {httpStatus} -> httpStatus) (\s@RegisterMailDomainResponse' {} a -> s {httpStatus = a} :: RegisterMailDomainResponse)

instance Prelude.NFData RegisterMailDomainResponse where
  rnf RegisterMailDomainResponse' {..} =
    Prelude.rnf httpStatus
