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
-- Module      : Amazonka.WorkMail.DeregisterMailDomain
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a domain from WorkMail, stops email routing to WorkMail, and
-- removes the authorization allowing WorkMail use. SES keeps the domain
-- because other applications may use it. You must first remove any email
-- address used by WorkMail entities before you remove the domain.
module Amazonka.WorkMail.DeregisterMailDomain
  ( -- * Creating a Request
    DeregisterMailDomain (..),
    newDeregisterMailDomain,

    -- * Request Lenses
    deregisterMailDomain_organizationId,
    deregisterMailDomain_domainName,

    -- * Destructuring the Response
    DeregisterMailDomainResponse (..),
    newDeregisterMailDomainResponse,

    -- * Response Lenses
    deregisterMailDomainResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newDeregisterMailDomain' smart constructor.
data DeregisterMailDomain = DeregisterMailDomain'
  { -- | The WorkMail organization for which the domain will be deregistered.
    organizationId :: Prelude.Text,
    -- | The domain to deregister in WorkMail and SES.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterMailDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'deregisterMailDomain_organizationId' - The WorkMail organization for which the domain will be deregistered.
--
-- 'domainName', 'deregisterMailDomain_domainName' - The domain to deregister in WorkMail and SES.
newDeregisterMailDomain ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'domainName'
  Prelude.Text ->
  DeregisterMailDomain
newDeregisterMailDomain pOrganizationId_ pDomainName_ =
  DeregisterMailDomain'
    { organizationId =
        pOrganizationId_,
      domainName = pDomainName_
    }

-- | The WorkMail organization for which the domain will be deregistered.
deregisterMailDomain_organizationId :: Lens.Lens' DeregisterMailDomain Prelude.Text
deregisterMailDomain_organizationId = Lens.lens (\DeregisterMailDomain' {organizationId} -> organizationId) (\s@DeregisterMailDomain' {} a -> s {organizationId = a} :: DeregisterMailDomain)

-- | The domain to deregister in WorkMail and SES.
deregisterMailDomain_domainName :: Lens.Lens' DeregisterMailDomain Prelude.Text
deregisterMailDomain_domainName = Lens.lens (\DeregisterMailDomain' {domainName} -> domainName) (\s@DeregisterMailDomain' {} a -> s {domainName = a} :: DeregisterMailDomain)

instance Core.AWSRequest DeregisterMailDomain where
  type
    AWSResponse DeregisterMailDomain =
      DeregisterMailDomainResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeregisterMailDomainResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeregisterMailDomain where
  hashWithSalt _salt DeregisterMailDomain' {..} =
    _salt `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData DeregisterMailDomain where
  rnf DeregisterMailDomain' {..} =
    Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders DeregisterMailDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.DeregisterMailDomain" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeregisterMailDomain where
  toJSON DeregisterMailDomain' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just ("DomainName" Data..= domainName)
          ]
      )

instance Data.ToPath DeregisterMailDomain where
  toPath = Prelude.const "/"

instance Data.ToQuery DeregisterMailDomain where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterMailDomainResponse' smart constructor.
data DeregisterMailDomainResponse = DeregisterMailDomainResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterMailDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deregisterMailDomainResponse_httpStatus' - The response's http status code.
newDeregisterMailDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeregisterMailDomainResponse
newDeregisterMailDomainResponse pHttpStatus_ =
  DeregisterMailDomainResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deregisterMailDomainResponse_httpStatus :: Lens.Lens' DeregisterMailDomainResponse Prelude.Int
deregisterMailDomainResponse_httpStatus = Lens.lens (\DeregisterMailDomainResponse' {httpStatus} -> httpStatus) (\s@DeregisterMailDomainResponse' {} a -> s {httpStatus = a} :: DeregisterMailDomainResponse)

instance Prelude.NFData DeregisterMailDomainResponse where
  rnf DeregisterMailDomainResponse' {..} =
    Prelude.rnf httpStatus
