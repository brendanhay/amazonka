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
-- Module      : Amazonka.Route53Domains.UpdateDomainContact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation updates the contact information for a particular domain.
-- You must specify information for at least one contact: registrant,
-- administrator, or technical.
--
-- If the update is successful, this method returns an operation ID that
-- you can use to track the progress and completion of the operation. If
-- the request is not completed successfully, the domain registrant will be
-- notified by email.
module Amazonka.Route53Domains.UpdateDomainContact
  ( -- * Creating a Request
    UpdateDomainContact (..),
    newUpdateDomainContact,

    -- * Request Lenses
    updateDomainContact_adminContact,
    updateDomainContact_consent,
    updateDomainContact_registrantContact,
    updateDomainContact_techContact,
    updateDomainContact_domainName,

    -- * Destructuring the Response
    UpdateDomainContactResponse (..),
    newUpdateDomainContactResponse,

    -- * Response Lenses
    updateDomainContactResponse_operationId,
    updateDomainContactResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Domains.Types

-- | The UpdateDomainContact request includes the following elements.
--
-- /See:/ 'newUpdateDomainContact' smart constructor.
data UpdateDomainContact = UpdateDomainContact'
  { -- | Provides detailed contact information.
    adminContact :: Prelude.Maybe (Data.Sensitive ContactDetail),
    -- | Customer\'s consent for the owner change request.
    consent :: Prelude.Maybe Consent,
    -- | Provides detailed contact information.
    registrantContact :: Prelude.Maybe (Data.Sensitive ContactDetail),
    -- | Provides detailed contact information.
    techContact :: Prelude.Maybe (Data.Sensitive ContactDetail),
    -- | The name of the domain that you want to update contact information for.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDomainContact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adminContact', 'updateDomainContact_adminContact' - Provides detailed contact information.
--
-- 'consent', 'updateDomainContact_consent' - Customer\'s consent for the owner change request.
--
-- 'registrantContact', 'updateDomainContact_registrantContact' - Provides detailed contact information.
--
-- 'techContact', 'updateDomainContact_techContact' - Provides detailed contact information.
--
-- 'domainName', 'updateDomainContact_domainName' - The name of the domain that you want to update contact information for.
newUpdateDomainContact ::
  -- | 'domainName'
  Prelude.Text ->
  UpdateDomainContact
newUpdateDomainContact pDomainName_ =
  UpdateDomainContact'
    { adminContact =
        Prelude.Nothing,
      consent = Prelude.Nothing,
      registrantContact = Prelude.Nothing,
      techContact = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | Provides detailed contact information.
updateDomainContact_adminContact :: Lens.Lens' UpdateDomainContact (Prelude.Maybe ContactDetail)
updateDomainContact_adminContact = Lens.lens (\UpdateDomainContact' {adminContact} -> adminContact) (\s@UpdateDomainContact' {} a -> s {adminContact = a} :: UpdateDomainContact) Prelude.. Lens.mapping Data._Sensitive

-- | Customer\'s consent for the owner change request.
updateDomainContact_consent :: Lens.Lens' UpdateDomainContact (Prelude.Maybe Consent)
updateDomainContact_consent = Lens.lens (\UpdateDomainContact' {consent} -> consent) (\s@UpdateDomainContact' {} a -> s {consent = a} :: UpdateDomainContact)

-- | Provides detailed contact information.
updateDomainContact_registrantContact :: Lens.Lens' UpdateDomainContact (Prelude.Maybe ContactDetail)
updateDomainContact_registrantContact = Lens.lens (\UpdateDomainContact' {registrantContact} -> registrantContact) (\s@UpdateDomainContact' {} a -> s {registrantContact = a} :: UpdateDomainContact) Prelude.. Lens.mapping Data._Sensitive

-- | Provides detailed contact information.
updateDomainContact_techContact :: Lens.Lens' UpdateDomainContact (Prelude.Maybe ContactDetail)
updateDomainContact_techContact = Lens.lens (\UpdateDomainContact' {techContact} -> techContact) (\s@UpdateDomainContact' {} a -> s {techContact = a} :: UpdateDomainContact) Prelude.. Lens.mapping Data._Sensitive

-- | The name of the domain that you want to update contact information for.
updateDomainContact_domainName :: Lens.Lens' UpdateDomainContact Prelude.Text
updateDomainContact_domainName = Lens.lens (\UpdateDomainContact' {domainName} -> domainName) (\s@UpdateDomainContact' {} a -> s {domainName = a} :: UpdateDomainContact)

instance Core.AWSRequest UpdateDomainContact where
  type
    AWSResponse UpdateDomainContact =
      UpdateDomainContactResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDomainContactResponse'
            Prelude.<$> (x Data..?> "OperationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDomainContact where
  hashWithSalt _salt UpdateDomainContact' {..} =
    _salt
      `Prelude.hashWithSalt` adminContact
      `Prelude.hashWithSalt` consent
      `Prelude.hashWithSalt` registrantContact
      `Prelude.hashWithSalt` techContact
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData UpdateDomainContact where
  rnf UpdateDomainContact' {..} =
    Prelude.rnf adminContact
      `Prelude.seq` Prelude.rnf consent
      `Prelude.seq` Prelude.rnf registrantContact
      `Prelude.seq` Prelude.rnf techContact
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders UpdateDomainContact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Domains_v20140515.UpdateDomainContact" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDomainContact where
  toJSON UpdateDomainContact' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdminContact" Data..=) Prelude.<$> adminContact,
            ("Consent" Data..=) Prelude.<$> consent,
            ("RegistrantContact" Data..=)
              Prelude.<$> registrantContact,
            ("TechContact" Data..=) Prelude.<$> techContact,
            Prelude.Just ("DomainName" Data..= domainName)
          ]
      )

instance Data.ToPath UpdateDomainContact where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateDomainContact where
  toQuery = Prelude.const Prelude.mempty

-- | The UpdateDomainContact response includes the following element.
--
-- /See:/ 'newUpdateDomainContactResponse' smart constructor.
data UpdateDomainContactResponse = UpdateDomainContactResponse'
  { -- | Identifier for tracking the progress of the request. To query the
    -- operation status, use
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDomainContactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'updateDomainContactResponse_operationId' - Identifier for tracking the progress of the request. To query the
-- operation status, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
--
-- 'httpStatus', 'updateDomainContactResponse_httpStatus' - The response's http status code.
newUpdateDomainContactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDomainContactResponse
newUpdateDomainContactResponse pHttpStatus_ =
  UpdateDomainContactResponse'
    { operationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Identifier for tracking the progress of the request. To query the
-- operation status, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
updateDomainContactResponse_operationId :: Lens.Lens' UpdateDomainContactResponse (Prelude.Maybe Prelude.Text)
updateDomainContactResponse_operationId = Lens.lens (\UpdateDomainContactResponse' {operationId} -> operationId) (\s@UpdateDomainContactResponse' {} a -> s {operationId = a} :: UpdateDomainContactResponse)

-- | The response's http status code.
updateDomainContactResponse_httpStatus :: Lens.Lens' UpdateDomainContactResponse Prelude.Int
updateDomainContactResponse_httpStatus = Lens.lens (\UpdateDomainContactResponse' {httpStatus} -> httpStatus) (\s@UpdateDomainContactResponse' {} a -> s {httpStatus = a} :: UpdateDomainContactResponse)

instance Prelude.NFData UpdateDomainContactResponse where
  rnf UpdateDomainContactResponse' {..} =
    Prelude.rnf operationId
      `Prelude.seq` Prelude.rnf httpStatus
