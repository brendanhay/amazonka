{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Route53Domains.UpdateDomainContact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation updates the contact information for a particular domain.
-- You must specify information for at least one contact: registrant,
-- administrator, or technical.
--
-- If the update is successful, this method returns an operation ID that
-- you can use to track the progress and completion of the action. If the
-- request is not completed successfully, the domain registrant will be
-- notified by email.
module Network.AWS.Route53Domains.UpdateDomainContact
  ( -- * Creating a Request
    UpdateDomainContact (..),
    newUpdateDomainContact,

    -- * Request Lenses
    updateDomainContact_registrantContact,
    updateDomainContact_techContact,
    updateDomainContact_adminContact,
    updateDomainContact_domainName,

    -- * Destructuring the Response
    UpdateDomainContactResponse (..),
    newUpdateDomainContactResponse,

    -- * Response Lenses
    updateDomainContactResponse_httpStatus,
    updateDomainContactResponse_operationId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53Domains.Types

-- | The UpdateDomainContact request includes the following elements.
--
-- /See:/ 'newUpdateDomainContact' smart constructor.
data UpdateDomainContact = UpdateDomainContact'
  { -- | Provides detailed contact information.
    registrantContact :: Prelude.Maybe (Prelude.Sensitive ContactDetail),
    -- | Provides detailed contact information.
    techContact :: Prelude.Maybe (Prelude.Sensitive ContactDetail),
    -- | Provides detailed contact information.
    adminContact :: Prelude.Maybe (Prelude.Sensitive ContactDetail),
    -- | The name of the domain that you want to update contact information for.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateDomainContact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registrantContact', 'updateDomainContact_registrantContact' - Provides detailed contact information.
--
-- 'techContact', 'updateDomainContact_techContact' - Provides detailed contact information.
--
-- 'adminContact', 'updateDomainContact_adminContact' - Provides detailed contact information.
--
-- 'domainName', 'updateDomainContact_domainName' - The name of the domain that you want to update contact information for.
newUpdateDomainContact ::
  -- | 'domainName'
  Prelude.Text ->
  UpdateDomainContact
newUpdateDomainContact pDomainName_ =
  UpdateDomainContact'
    { registrantContact =
        Prelude.Nothing,
      techContact = Prelude.Nothing,
      adminContact = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | Provides detailed contact information.
updateDomainContact_registrantContact :: Lens.Lens' UpdateDomainContact (Prelude.Maybe ContactDetail)
updateDomainContact_registrantContact = Lens.lens (\UpdateDomainContact' {registrantContact} -> registrantContact) (\s@UpdateDomainContact' {} a -> s {registrantContact = a} :: UpdateDomainContact) Prelude.. Lens.mapping Prelude._Sensitive

-- | Provides detailed contact information.
updateDomainContact_techContact :: Lens.Lens' UpdateDomainContact (Prelude.Maybe ContactDetail)
updateDomainContact_techContact = Lens.lens (\UpdateDomainContact' {techContact} -> techContact) (\s@UpdateDomainContact' {} a -> s {techContact = a} :: UpdateDomainContact) Prelude.. Lens.mapping Prelude._Sensitive

-- | Provides detailed contact information.
updateDomainContact_adminContact :: Lens.Lens' UpdateDomainContact (Prelude.Maybe ContactDetail)
updateDomainContact_adminContact = Lens.lens (\UpdateDomainContact' {adminContact} -> adminContact) (\s@UpdateDomainContact' {} a -> s {adminContact = a} :: UpdateDomainContact) Prelude.. Lens.mapping Prelude._Sensitive

-- | The name of the domain that you want to update contact information for.
updateDomainContact_domainName :: Lens.Lens' UpdateDomainContact Prelude.Text
updateDomainContact_domainName = Lens.lens (\UpdateDomainContact' {domainName} -> domainName) (\s@UpdateDomainContact' {} a -> s {domainName = a} :: UpdateDomainContact)

instance Prelude.AWSRequest UpdateDomainContact where
  type
    Rs UpdateDomainContact =
      UpdateDomainContactResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDomainContactResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "OperationId")
      )

instance Prelude.Hashable UpdateDomainContact

instance Prelude.NFData UpdateDomainContact

instance Prelude.ToHeaders UpdateDomainContact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Route53Domains_v20140515.UpdateDomainContact" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateDomainContact where
  toJSON UpdateDomainContact' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("RegistrantContact" Prelude..=)
              Prelude.<$> registrantContact,
            ("TechContact" Prelude..=) Prelude.<$> techContact,
            ("AdminContact" Prelude..=) Prelude.<$> adminContact,
            Prelude.Just ("DomainName" Prelude..= domainName)
          ]
      )

instance Prelude.ToPath UpdateDomainContact where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateDomainContact where
  toQuery = Prelude.const Prelude.mempty

-- | The UpdateDomainContact response includes the following element.
--
-- /See:/ 'newUpdateDomainContactResponse' smart constructor.
data UpdateDomainContactResponse = UpdateDomainContactResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Identifier for tracking the progress of the request. To query the
    -- operation status, use
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
    operationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateDomainContactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDomainContactResponse_httpStatus' - The response's http status code.
--
-- 'operationId', 'updateDomainContactResponse_operationId' - Identifier for tracking the progress of the request. To query the
-- operation status, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
newUpdateDomainContactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'operationId'
  Prelude.Text ->
  UpdateDomainContactResponse
newUpdateDomainContactResponse
  pHttpStatus_
  pOperationId_ =
    UpdateDomainContactResponse'
      { httpStatus =
          pHttpStatus_,
        operationId = pOperationId_
      }

-- | The response's http status code.
updateDomainContactResponse_httpStatus :: Lens.Lens' UpdateDomainContactResponse Prelude.Int
updateDomainContactResponse_httpStatus = Lens.lens (\UpdateDomainContactResponse' {httpStatus} -> httpStatus) (\s@UpdateDomainContactResponse' {} a -> s {httpStatus = a} :: UpdateDomainContactResponse)

-- | Identifier for tracking the progress of the request. To query the
-- operation status, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
updateDomainContactResponse_operationId :: Lens.Lens' UpdateDomainContactResponse Prelude.Text
updateDomainContactResponse_operationId = Lens.lens (\UpdateDomainContactResponse' {operationId} -> operationId) (\s@UpdateDomainContactResponse' {} a -> s {operationId = a} :: UpdateDomainContactResponse)

instance Prelude.NFData UpdateDomainContactResponse
