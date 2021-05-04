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
-- Module      : Network.AWS.IoT.AttachThingPrincipal
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches the specified principal to the specified thing. A principal can
-- be X.509 certificates, IAM users, groups, and roles, Amazon Cognito
-- identities or federated identities.
module Network.AWS.IoT.AttachThingPrincipal
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest AttachThingPrincipal where
  type
    Rs AttachThingPrincipal =
      AttachThingPrincipalResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AttachThingPrincipalResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AttachThingPrincipal

instance Prelude.NFData AttachThingPrincipal

instance Prelude.ToHeaders AttachThingPrincipal where
  toHeaders AttachThingPrincipal' {..} =
    Prelude.mconcat
      ["x-amzn-principal" Prelude.=# principal]

instance Prelude.ToJSON AttachThingPrincipal where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath AttachThingPrincipal where
  toPath AttachThingPrincipal' {..} =
    Prelude.mconcat
      ["/things/", Prelude.toBS thingName, "/principals"]

instance Prelude.ToQuery AttachThingPrincipal where
  toQuery = Prelude.const Prelude.mempty

-- | The output from the AttachThingPrincipal operation.
--
-- /See:/ 'newAttachThingPrincipalResponse' smart constructor.
data AttachThingPrincipalResponse = AttachThingPrincipalResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData AttachThingPrincipalResponse
