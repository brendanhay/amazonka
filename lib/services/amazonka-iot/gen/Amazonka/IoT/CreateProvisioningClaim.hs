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
-- Module      : Amazonka.IoT.CreateProvisioningClaim
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a provisioning claim.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions CreateProvisioningClaim>
-- action.
module Amazonka.IoT.CreateProvisioningClaim
  ( -- * Creating a Request
    CreateProvisioningClaim (..),
    newCreateProvisioningClaim,

    -- * Request Lenses
    createProvisioningClaim_templateName,

    -- * Destructuring the Response
    CreateProvisioningClaimResponse (..),
    newCreateProvisioningClaimResponse,

    -- * Response Lenses
    createProvisioningClaimResponse_certificateId,
    createProvisioningClaimResponse_certificatePem,
    createProvisioningClaimResponse_expiration,
    createProvisioningClaimResponse_keyPair,
    createProvisioningClaimResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateProvisioningClaim' smart constructor.
data CreateProvisioningClaim = CreateProvisioningClaim'
  { -- | The name of the provisioning template to use.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProvisioningClaim' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'createProvisioningClaim_templateName' - The name of the provisioning template to use.
newCreateProvisioningClaim ::
  -- | 'templateName'
  Prelude.Text ->
  CreateProvisioningClaim
newCreateProvisioningClaim pTemplateName_ =
  CreateProvisioningClaim'
    { templateName =
        pTemplateName_
    }

-- | The name of the provisioning template to use.
createProvisioningClaim_templateName :: Lens.Lens' CreateProvisioningClaim Prelude.Text
createProvisioningClaim_templateName = Lens.lens (\CreateProvisioningClaim' {templateName} -> templateName) (\s@CreateProvisioningClaim' {} a -> s {templateName = a} :: CreateProvisioningClaim)

instance Core.AWSRequest CreateProvisioningClaim where
  type
    AWSResponse CreateProvisioningClaim =
      CreateProvisioningClaimResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProvisioningClaimResponse'
            Prelude.<$> (x Data..?> "certificateId")
            Prelude.<*> (x Data..?> "certificatePem")
            Prelude.<*> (x Data..?> "expiration")
            Prelude.<*> (x Data..?> "keyPair")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateProvisioningClaim where
  hashWithSalt _salt CreateProvisioningClaim' {..} =
    _salt `Prelude.hashWithSalt` templateName

instance Prelude.NFData CreateProvisioningClaim where
  rnf CreateProvisioningClaim' {..} =
    Prelude.rnf templateName

instance Data.ToHeaders CreateProvisioningClaim where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateProvisioningClaim where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath CreateProvisioningClaim where
  toPath CreateProvisioningClaim' {..} =
    Prelude.mconcat
      [ "/provisioning-templates/",
        Data.toBS templateName,
        "/provisioning-claim"
      ]

instance Data.ToQuery CreateProvisioningClaim where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateProvisioningClaimResponse' smart constructor.
data CreateProvisioningClaimResponse = CreateProvisioningClaimResponse'
  { -- | The ID of the certificate.
    certificateId :: Prelude.Maybe Prelude.Text,
    -- | The provisioning claim certificate.
    certificatePem :: Prelude.Maybe Prelude.Text,
    -- | The provisioning claim expiration time.
    expiration :: Prelude.Maybe Data.POSIX,
    -- | The provisioning claim key pair.
    keyPair :: Prelude.Maybe KeyPair,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProvisioningClaimResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateId', 'createProvisioningClaimResponse_certificateId' - The ID of the certificate.
--
-- 'certificatePem', 'createProvisioningClaimResponse_certificatePem' - The provisioning claim certificate.
--
-- 'expiration', 'createProvisioningClaimResponse_expiration' - The provisioning claim expiration time.
--
-- 'keyPair', 'createProvisioningClaimResponse_keyPair' - The provisioning claim key pair.
--
-- 'httpStatus', 'createProvisioningClaimResponse_httpStatus' - The response's http status code.
newCreateProvisioningClaimResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateProvisioningClaimResponse
newCreateProvisioningClaimResponse pHttpStatus_ =
  CreateProvisioningClaimResponse'
    { certificateId =
        Prelude.Nothing,
      certificatePem = Prelude.Nothing,
      expiration = Prelude.Nothing,
      keyPair = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the certificate.
createProvisioningClaimResponse_certificateId :: Lens.Lens' CreateProvisioningClaimResponse (Prelude.Maybe Prelude.Text)
createProvisioningClaimResponse_certificateId = Lens.lens (\CreateProvisioningClaimResponse' {certificateId} -> certificateId) (\s@CreateProvisioningClaimResponse' {} a -> s {certificateId = a} :: CreateProvisioningClaimResponse)

-- | The provisioning claim certificate.
createProvisioningClaimResponse_certificatePem :: Lens.Lens' CreateProvisioningClaimResponse (Prelude.Maybe Prelude.Text)
createProvisioningClaimResponse_certificatePem = Lens.lens (\CreateProvisioningClaimResponse' {certificatePem} -> certificatePem) (\s@CreateProvisioningClaimResponse' {} a -> s {certificatePem = a} :: CreateProvisioningClaimResponse)

-- | The provisioning claim expiration time.
createProvisioningClaimResponse_expiration :: Lens.Lens' CreateProvisioningClaimResponse (Prelude.Maybe Prelude.UTCTime)
createProvisioningClaimResponse_expiration = Lens.lens (\CreateProvisioningClaimResponse' {expiration} -> expiration) (\s@CreateProvisioningClaimResponse' {} a -> s {expiration = a} :: CreateProvisioningClaimResponse) Prelude.. Lens.mapping Data._Time

-- | The provisioning claim key pair.
createProvisioningClaimResponse_keyPair :: Lens.Lens' CreateProvisioningClaimResponse (Prelude.Maybe KeyPair)
createProvisioningClaimResponse_keyPair = Lens.lens (\CreateProvisioningClaimResponse' {keyPair} -> keyPair) (\s@CreateProvisioningClaimResponse' {} a -> s {keyPair = a} :: CreateProvisioningClaimResponse)

-- | The response's http status code.
createProvisioningClaimResponse_httpStatus :: Lens.Lens' CreateProvisioningClaimResponse Prelude.Int
createProvisioningClaimResponse_httpStatus = Lens.lens (\CreateProvisioningClaimResponse' {httpStatus} -> httpStatus) (\s@CreateProvisioningClaimResponse' {} a -> s {httpStatus = a} :: CreateProvisioningClaimResponse)

instance
  Prelude.NFData
    CreateProvisioningClaimResponse
  where
  rnf CreateProvisioningClaimResponse' {..} =
    Prelude.rnf certificateId
      `Prelude.seq` Prelude.rnf certificatePem
      `Prelude.seq` Prelude.rnf expiration
      `Prelude.seq` Prelude.rnf keyPair
      `Prelude.seq` Prelude.rnf httpStatus
