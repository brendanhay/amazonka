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
-- Module      : Amazonka.SSMSAP.RegisterApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Register an SAP application with AWS Systems Manager for SAP. You must
-- meet the following requirements before registering.
--
-- The SAP application you want to register with AWS Systems Manager for
-- SAP is running on Amazon EC2.
--
-- AWS Systems Manager Agent must be setup on an Amazon EC2 instance along
-- with the required IAM permissions.
--
-- Amazon EC2 instance(s) must have access to the secrets created in AWS
-- Secrets Manager to manage SAP applications and components.
module Amazonka.SSMSAP.RegisterApplication
  ( -- * Creating a Request
    RegisterApplication (..),
    newRegisterApplication,

    -- * Request Lenses
    registerApplication_sapInstanceNumber,
    registerApplication_sid,
    registerApplication_tags,
    registerApplication_applicationId,
    registerApplication_applicationType,
    registerApplication_instances,
    registerApplication_credentials,

    -- * Destructuring the Response
    RegisterApplicationResponse (..),
    newRegisterApplicationResponse,

    -- * Response Lenses
    registerApplicationResponse_application,
    registerApplicationResponse_operationId,
    registerApplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMSAP.Types

-- | /See:/ 'newRegisterApplication' smart constructor.
data RegisterApplication = RegisterApplication'
  { -- | The SAP instance number of the application.
    sapInstanceNumber :: Prelude.Maybe Prelude.Text,
    -- | The System ID of the application.
    sid :: Prelude.Maybe Prelude.Text,
    -- | The tags to be attached to the SAP application.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ID of the application.
    applicationId :: Prelude.Text,
    -- | The type of the application.
    applicationType :: ApplicationType,
    -- | The Amazon EC2 instances on which your SAP application is running.
    instances :: Prelude.NonEmpty Prelude.Text,
    -- | The credentials of the SAP application.
    credentials :: Prelude.NonEmpty ApplicationCredential
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sapInstanceNumber', 'registerApplication_sapInstanceNumber' - The SAP instance number of the application.
--
-- 'sid', 'registerApplication_sid' - The System ID of the application.
--
-- 'tags', 'registerApplication_tags' - The tags to be attached to the SAP application.
--
-- 'applicationId', 'registerApplication_applicationId' - The ID of the application.
--
-- 'applicationType', 'registerApplication_applicationType' - The type of the application.
--
-- 'instances', 'registerApplication_instances' - The Amazon EC2 instances on which your SAP application is running.
--
-- 'credentials', 'registerApplication_credentials' - The credentials of the SAP application.
newRegisterApplication ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'applicationType'
  ApplicationType ->
  -- | 'instances'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'credentials'
  Prelude.NonEmpty ApplicationCredential ->
  RegisterApplication
newRegisterApplication
  pApplicationId_
  pApplicationType_
  pInstances_
  pCredentials_ =
    RegisterApplication'
      { sapInstanceNumber =
          Prelude.Nothing,
        sid = Prelude.Nothing,
        tags = Prelude.Nothing,
        applicationId = pApplicationId_,
        applicationType = pApplicationType_,
        instances = Lens.coerced Lens.# pInstances_,
        credentials = Lens.coerced Lens.# pCredentials_
      }

-- | The SAP instance number of the application.
registerApplication_sapInstanceNumber :: Lens.Lens' RegisterApplication (Prelude.Maybe Prelude.Text)
registerApplication_sapInstanceNumber = Lens.lens (\RegisterApplication' {sapInstanceNumber} -> sapInstanceNumber) (\s@RegisterApplication' {} a -> s {sapInstanceNumber = a} :: RegisterApplication)

-- | The System ID of the application.
registerApplication_sid :: Lens.Lens' RegisterApplication (Prelude.Maybe Prelude.Text)
registerApplication_sid = Lens.lens (\RegisterApplication' {sid} -> sid) (\s@RegisterApplication' {} a -> s {sid = a} :: RegisterApplication)

-- | The tags to be attached to the SAP application.
registerApplication_tags :: Lens.Lens' RegisterApplication (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
registerApplication_tags = Lens.lens (\RegisterApplication' {tags} -> tags) (\s@RegisterApplication' {} a -> s {tags = a} :: RegisterApplication) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the application.
registerApplication_applicationId :: Lens.Lens' RegisterApplication Prelude.Text
registerApplication_applicationId = Lens.lens (\RegisterApplication' {applicationId} -> applicationId) (\s@RegisterApplication' {} a -> s {applicationId = a} :: RegisterApplication)

-- | The type of the application.
registerApplication_applicationType :: Lens.Lens' RegisterApplication ApplicationType
registerApplication_applicationType = Lens.lens (\RegisterApplication' {applicationType} -> applicationType) (\s@RegisterApplication' {} a -> s {applicationType = a} :: RegisterApplication)

-- | The Amazon EC2 instances on which your SAP application is running.
registerApplication_instances :: Lens.Lens' RegisterApplication (Prelude.NonEmpty Prelude.Text)
registerApplication_instances = Lens.lens (\RegisterApplication' {instances} -> instances) (\s@RegisterApplication' {} a -> s {instances = a} :: RegisterApplication) Prelude.. Lens.coerced

-- | The credentials of the SAP application.
registerApplication_credentials :: Lens.Lens' RegisterApplication (Prelude.NonEmpty ApplicationCredential)
registerApplication_credentials = Lens.lens (\RegisterApplication' {credentials} -> credentials) (\s@RegisterApplication' {} a -> s {credentials = a} :: RegisterApplication) Prelude.. Lens.coerced

instance Core.AWSRequest RegisterApplication where
  type
    AWSResponse RegisterApplication =
      RegisterApplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterApplicationResponse'
            Prelude.<$> (x Data..?> "Application")
            Prelude.<*> (x Data..?> "OperationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterApplication where
  hashWithSalt _salt RegisterApplication' {..} =
    _salt
      `Prelude.hashWithSalt` sapInstanceNumber
      `Prelude.hashWithSalt` sid
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` applicationType
      `Prelude.hashWithSalt` instances
      `Prelude.hashWithSalt` credentials

instance Prelude.NFData RegisterApplication where
  rnf RegisterApplication' {..} =
    Prelude.rnf sapInstanceNumber
      `Prelude.seq` Prelude.rnf sid
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf applicationType
      `Prelude.seq` Prelude.rnf instances
      `Prelude.seq` Prelude.rnf credentials

instance Data.ToHeaders RegisterApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RegisterApplication where
  toJSON RegisterApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SapInstanceNumber" Data..=)
              Prelude.<$> sapInstanceNumber,
            ("Sid" Data..=) Prelude.<$> sid,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("ApplicationId" Data..= applicationId),
            Prelude.Just
              ("ApplicationType" Data..= applicationType),
            Prelude.Just ("Instances" Data..= instances),
            Prelude.Just ("Credentials" Data..= credentials)
          ]
      )

instance Data.ToPath RegisterApplication where
  toPath = Prelude.const "/register-application"

instance Data.ToQuery RegisterApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterApplicationResponse' smart constructor.
data RegisterApplicationResponse = RegisterApplicationResponse'
  { -- | The application registered with AWS Systems Manager for SAP.
    application :: Prelude.Maybe Application,
    -- | The ID of the operation.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'application', 'registerApplicationResponse_application' - The application registered with AWS Systems Manager for SAP.
--
-- 'operationId', 'registerApplicationResponse_operationId' - The ID of the operation.
--
-- 'httpStatus', 'registerApplicationResponse_httpStatus' - The response's http status code.
newRegisterApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterApplicationResponse
newRegisterApplicationResponse pHttpStatus_ =
  RegisterApplicationResponse'
    { application =
        Prelude.Nothing,
      operationId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The application registered with AWS Systems Manager for SAP.
registerApplicationResponse_application :: Lens.Lens' RegisterApplicationResponse (Prelude.Maybe Application)
registerApplicationResponse_application = Lens.lens (\RegisterApplicationResponse' {application} -> application) (\s@RegisterApplicationResponse' {} a -> s {application = a} :: RegisterApplicationResponse)

-- | The ID of the operation.
registerApplicationResponse_operationId :: Lens.Lens' RegisterApplicationResponse (Prelude.Maybe Prelude.Text)
registerApplicationResponse_operationId = Lens.lens (\RegisterApplicationResponse' {operationId} -> operationId) (\s@RegisterApplicationResponse' {} a -> s {operationId = a} :: RegisterApplicationResponse)

-- | The response's http status code.
registerApplicationResponse_httpStatus :: Lens.Lens' RegisterApplicationResponse Prelude.Int
registerApplicationResponse_httpStatus = Lens.lens (\RegisterApplicationResponse' {httpStatus} -> httpStatus) (\s@RegisterApplicationResponse' {} a -> s {httpStatus = a} :: RegisterApplicationResponse)

instance Prelude.NFData RegisterApplicationResponse where
  rnf RegisterApplicationResponse' {..} =
    Prelude.rnf application
      `Prelude.seq` Prelude.rnf operationId
      `Prelude.seq` Prelude.rnf httpStatus
