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
-- Module      : Network.AWS.IoT.DetachThingPrincipal
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches the specified principal from the specified thing. A principal
-- can be X.509 certificates, IAM users, groups, and roles, Amazon Cognito
-- identities or federated identities.
--
-- This call is asynchronous. It might take several seconds for the
-- detachment to propagate.
module Network.AWS.IoT.DetachThingPrincipal
  ( -- * Creating a Request
    DetachThingPrincipal (..),
    newDetachThingPrincipal,

    -- * Request Lenses
    detachThingPrincipal_thingName,
    detachThingPrincipal_principal,

    -- * Destructuring the Response
    DetachThingPrincipalResponse (..),
    newDetachThingPrincipalResponse,

    -- * Response Lenses
    detachThingPrincipalResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DetachThingPrincipal operation.
--
-- /See:/ 'newDetachThingPrincipal' smart constructor.
data DetachThingPrincipal = DetachThingPrincipal'
  { -- | The name of the thing.
    thingName :: Core.Text,
    -- | If the principal is a certificate, this value must be ARN of the
    -- certificate. If the principal is an Amazon Cognito identity, this value
    -- must be the ID of the Amazon Cognito identity.
    principal :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DetachThingPrincipal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thingName', 'detachThingPrincipal_thingName' - The name of the thing.
--
-- 'principal', 'detachThingPrincipal_principal' - If the principal is a certificate, this value must be ARN of the
-- certificate. If the principal is an Amazon Cognito identity, this value
-- must be the ID of the Amazon Cognito identity.
newDetachThingPrincipal ::
  -- | 'thingName'
  Core.Text ->
  -- | 'principal'
  Core.Text ->
  DetachThingPrincipal
newDetachThingPrincipal pThingName_ pPrincipal_ =
  DetachThingPrincipal'
    { thingName = pThingName_,
      principal = pPrincipal_
    }

-- | The name of the thing.
detachThingPrincipal_thingName :: Lens.Lens' DetachThingPrincipal Core.Text
detachThingPrincipal_thingName = Lens.lens (\DetachThingPrincipal' {thingName} -> thingName) (\s@DetachThingPrincipal' {} a -> s {thingName = a} :: DetachThingPrincipal)

-- | If the principal is a certificate, this value must be ARN of the
-- certificate. If the principal is an Amazon Cognito identity, this value
-- must be the ID of the Amazon Cognito identity.
detachThingPrincipal_principal :: Lens.Lens' DetachThingPrincipal Core.Text
detachThingPrincipal_principal = Lens.lens (\DetachThingPrincipal' {principal} -> principal) (\s@DetachThingPrincipal' {} a -> s {principal = a} :: DetachThingPrincipal)

instance Core.AWSRequest DetachThingPrincipal where
  type
    AWSResponse DetachThingPrincipal =
      DetachThingPrincipalResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DetachThingPrincipalResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DetachThingPrincipal

instance Core.NFData DetachThingPrincipal

instance Core.ToHeaders DetachThingPrincipal where
  toHeaders DetachThingPrincipal' {..} =
    Core.mconcat ["x-amzn-principal" Core.=# principal]

instance Core.ToPath DetachThingPrincipal where
  toPath DetachThingPrincipal' {..} =
    Core.mconcat
      ["/things/", Core.toBS thingName, "/principals"]

instance Core.ToQuery DetachThingPrincipal where
  toQuery = Core.const Core.mempty

-- | The output from the DetachThingPrincipal operation.
--
-- /See:/ 'newDetachThingPrincipalResponse' smart constructor.
data DetachThingPrincipalResponse = DetachThingPrincipalResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DetachThingPrincipalResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'detachThingPrincipalResponse_httpStatus' - The response's http status code.
newDetachThingPrincipalResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DetachThingPrincipalResponse
newDetachThingPrincipalResponse pHttpStatus_ =
  DetachThingPrincipalResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
detachThingPrincipalResponse_httpStatus :: Lens.Lens' DetachThingPrincipalResponse Core.Int
detachThingPrincipalResponse_httpStatus = Lens.lens (\DetachThingPrincipalResponse' {httpStatus} -> httpStatus) (\s@DetachThingPrincipalResponse' {} a -> s {httpStatus = a} :: DetachThingPrincipalResponse)

instance Core.NFData DetachThingPrincipalResponse
