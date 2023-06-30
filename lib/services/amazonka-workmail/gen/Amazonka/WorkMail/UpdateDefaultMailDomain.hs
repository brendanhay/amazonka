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
-- Module      : Amazonka.WorkMail.UpdateDefaultMailDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the default mail domain for an organization. The default mail
-- domain is used by the WorkMail AWS Console to suggest an email address
-- when enabling a mail user. You can only have one default domain.
module Amazonka.WorkMail.UpdateDefaultMailDomain
  ( -- * Creating a Request
    UpdateDefaultMailDomain (..),
    newUpdateDefaultMailDomain,

    -- * Request Lenses
    updateDefaultMailDomain_organizationId,
    updateDefaultMailDomain_domainName,

    -- * Destructuring the Response
    UpdateDefaultMailDomainResponse (..),
    newUpdateDefaultMailDomainResponse,

    -- * Response Lenses
    updateDefaultMailDomainResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newUpdateDefaultMailDomain' smart constructor.
data UpdateDefaultMailDomain = UpdateDefaultMailDomain'
  { -- | The WorkMail organization for which to list domains.
    organizationId :: Prelude.Text,
    -- | The domain name that will become the default domain.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDefaultMailDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'updateDefaultMailDomain_organizationId' - The WorkMail organization for which to list domains.
--
-- 'domainName', 'updateDefaultMailDomain_domainName' - The domain name that will become the default domain.
newUpdateDefaultMailDomain ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'domainName'
  Prelude.Text ->
  UpdateDefaultMailDomain
newUpdateDefaultMailDomain
  pOrganizationId_
  pDomainName_ =
    UpdateDefaultMailDomain'
      { organizationId =
          pOrganizationId_,
        domainName = pDomainName_
      }

-- | The WorkMail organization for which to list domains.
updateDefaultMailDomain_organizationId :: Lens.Lens' UpdateDefaultMailDomain Prelude.Text
updateDefaultMailDomain_organizationId = Lens.lens (\UpdateDefaultMailDomain' {organizationId} -> organizationId) (\s@UpdateDefaultMailDomain' {} a -> s {organizationId = a} :: UpdateDefaultMailDomain)

-- | The domain name that will become the default domain.
updateDefaultMailDomain_domainName :: Lens.Lens' UpdateDefaultMailDomain Prelude.Text
updateDefaultMailDomain_domainName = Lens.lens (\UpdateDefaultMailDomain' {domainName} -> domainName) (\s@UpdateDefaultMailDomain' {} a -> s {domainName = a} :: UpdateDefaultMailDomain)

instance Core.AWSRequest UpdateDefaultMailDomain where
  type
    AWSResponse UpdateDefaultMailDomain =
      UpdateDefaultMailDomainResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDefaultMailDomainResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDefaultMailDomain where
  hashWithSalt _salt UpdateDefaultMailDomain' {..} =
    _salt
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData UpdateDefaultMailDomain where
  rnf UpdateDefaultMailDomain' {..} =
    Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders UpdateDefaultMailDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.UpdateDefaultMailDomain" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDefaultMailDomain where
  toJSON UpdateDefaultMailDomain' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just ("DomainName" Data..= domainName)
          ]
      )

instance Data.ToPath UpdateDefaultMailDomain where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateDefaultMailDomain where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDefaultMailDomainResponse' smart constructor.
data UpdateDefaultMailDomainResponse = UpdateDefaultMailDomainResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDefaultMailDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDefaultMailDomainResponse_httpStatus' - The response's http status code.
newUpdateDefaultMailDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDefaultMailDomainResponse
newUpdateDefaultMailDomainResponse pHttpStatus_ =
  UpdateDefaultMailDomainResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateDefaultMailDomainResponse_httpStatus :: Lens.Lens' UpdateDefaultMailDomainResponse Prelude.Int
updateDefaultMailDomainResponse_httpStatus = Lens.lens (\UpdateDefaultMailDomainResponse' {httpStatus} -> httpStatus) (\s@UpdateDefaultMailDomainResponse' {} a -> s {httpStatus = a} :: UpdateDefaultMailDomainResponse)

instance
  Prelude.NFData
    UpdateDefaultMailDomainResponse
  where
  rnf UpdateDefaultMailDomainResponse' {..} =
    Prelude.rnf httpStatus
