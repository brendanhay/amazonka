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
-- Module      : Amazonka.IoT.AttachThingPrincipal
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches the specified principal to the specified thing. A principal can
-- be X.509 certificates, Amazon Cognito identities or federated
-- identities.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions AttachThingPrincipal>
-- action.
module Amazonka.IoT.AttachThingPrincipal
  ( -- * Creating a Request
    AttachThingPrincipal (..),
    newAttachThingPrincipal,

    -- * Request Lenses
    attachThingPrincipal_thingName,
    attachThingPrincipal_principal,

    -- * Destructuring the Response
    AttachThingPrincipalResponse (..),
    newAttachThingPrincipalResponse,

    -- * Response Lenses
    attachThingPrincipalResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the AttachThingPrincipal operation.
--
-- /See:/ 'newAttachThingPrincipal' smart constructor.
data AttachThingPrincipal = AttachThingPrincipal'
  { -- | The name of the thing.
    thingName :: Prelude.Text,
    -- | The principal, which can be a certificate ARN (as returned from the
    -- CreateCertificate operation) or an Amazon Cognito ID.
    principal :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachThingPrincipal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thingName', 'attachThingPrincipal_thingName' - The name of the thing.
--
-- 'principal', 'attachThingPrincipal_principal' - The principal, which can be a certificate ARN (as returned from the
-- CreateCertificate operation) or an Amazon Cognito ID.
newAttachThingPrincipal ::
  -- | 'thingName'
  Prelude.Text ->
  -- | 'principal'
  Prelude.Text ->
  AttachThingPrincipal
newAttachThingPrincipal pThingName_ pPrincipal_ =
  AttachThingPrincipal'
    { thingName = pThingName_,
      principal = pPrincipal_
    }

-- | The name of the thing.
attachThingPrincipal_thingName :: Lens.Lens' AttachThingPrincipal Prelude.Text
attachThingPrincipal_thingName = Lens.lens (\AttachThingPrincipal' {thingName} -> thingName) (\s@AttachThingPrincipal' {} a -> s {thingName = a} :: AttachThingPrincipal)

-- | The principal, which can be a certificate ARN (as returned from the
-- CreateCertificate operation) or an Amazon Cognito ID.
attachThingPrincipal_principal :: Lens.Lens' AttachThingPrincipal Prelude.Text
attachThingPrincipal_principal = Lens.lens (\AttachThingPrincipal' {principal} -> principal) (\s@AttachThingPrincipal' {} a -> s {principal = a} :: AttachThingPrincipal)

instance Core.AWSRequest AttachThingPrincipal where
  type
    AWSResponse AttachThingPrincipal =
      AttachThingPrincipalResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AttachThingPrincipalResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AttachThingPrincipal where
  hashWithSalt _salt AttachThingPrincipal' {..} =
    _salt `Prelude.hashWithSalt` thingName
      `Prelude.hashWithSalt` principal

instance Prelude.NFData AttachThingPrincipal where
  rnf AttachThingPrincipal' {..} =
    Prelude.rnf thingName
      `Prelude.seq` Prelude.rnf principal

instance Data.ToHeaders AttachThingPrincipal where
  toHeaders AttachThingPrincipal' {..} =
    Prelude.mconcat
      ["x-amzn-principal" Data.=# principal]

instance Data.ToJSON AttachThingPrincipal where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath AttachThingPrincipal where
  toPath AttachThingPrincipal' {..} =
    Prelude.mconcat
      ["/things/", Data.toBS thingName, "/principals"]

instance Data.ToQuery AttachThingPrincipal where
  toQuery = Prelude.const Prelude.mempty

-- | The output from the AttachThingPrincipal operation.
--
-- /See:/ 'newAttachThingPrincipalResponse' smart constructor.
data AttachThingPrincipalResponse = AttachThingPrincipalResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachThingPrincipalResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'attachThingPrincipalResponse_httpStatus' - The response's http status code.
newAttachThingPrincipalResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AttachThingPrincipalResponse
newAttachThingPrincipalResponse pHttpStatus_ =
  AttachThingPrincipalResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
attachThingPrincipalResponse_httpStatus :: Lens.Lens' AttachThingPrincipalResponse Prelude.Int
attachThingPrincipalResponse_httpStatus = Lens.lens (\AttachThingPrincipalResponse' {httpStatus} -> httpStatus) (\s@AttachThingPrincipalResponse' {} a -> s {httpStatus = a} :: AttachThingPrincipalResponse)

instance Prelude.NFData AttachThingPrincipalResponse where
  rnf AttachThingPrincipalResponse' {..} =
    Prelude.rnf httpStatus
