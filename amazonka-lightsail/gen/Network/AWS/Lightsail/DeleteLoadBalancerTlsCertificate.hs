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
-- Module      : Network.AWS.Lightsail.DeleteLoadBalancerTlsCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an SSL\/TLS certificate associated with a Lightsail load
-- balancer.
--
-- The @DeleteLoadBalancerTlsCertificate@ operation supports tag-based
-- access control via resource tags applied to the resource identified by
-- @load balancer name@. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.DeleteLoadBalancerTlsCertificate
  ( -- * Creating a Request
    DeleteLoadBalancerTlsCertificate (..),
    newDeleteLoadBalancerTlsCertificate,

    -- * Request Lenses
    deleteLoadBalancerTlsCertificate_force,
    deleteLoadBalancerTlsCertificate_loadBalancerName,
    deleteLoadBalancerTlsCertificate_certificateName,

    -- * Destructuring the Response
    DeleteLoadBalancerTlsCertificateResponse (..),
    newDeleteLoadBalancerTlsCertificateResponse,

    -- * Response Lenses
    deleteLoadBalancerTlsCertificateResponse_operations,
    deleteLoadBalancerTlsCertificateResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteLoadBalancerTlsCertificate' smart constructor.
data DeleteLoadBalancerTlsCertificate = DeleteLoadBalancerTlsCertificate'
  { -- | When @true@, forces the deletion of an SSL\/TLS certificate.
    --
    -- There can be two certificates associated with a Lightsail load balancer:
    -- the primary and the backup. The @force@ parameter is required when the
    -- primary SSL\/TLS certificate is in use by an instance attached to the
    -- load balancer.
    force :: Prelude.Maybe Prelude.Bool,
    -- | The load balancer name.
    loadBalancerName :: Prelude.Text,
    -- | The SSL\/TLS certificate name.
    certificateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteLoadBalancerTlsCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'force', 'deleteLoadBalancerTlsCertificate_force' - When @true@, forces the deletion of an SSL\/TLS certificate.
--
-- There can be two certificates associated with a Lightsail load balancer:
-- the primary and the backup. The @force@ parameter is required when the
-- primary SSL\/TLS certificate is in use by an instance attached to the
-- load balancer.
--
-- 'loadBalancerName', 'deleteLoadBalancerTlsCertificate_loadBalancerName' - The load balancer name.
--
-- 'certificateName', 'deleteLoadBalancerTlsCertificate_certificateName' - The SSL\/TLS certificate name.
newDeleteLoadBalancerTlsCertificate ::
  -- | 'loadBalancerName'
  Prelude.Text ->
  -- | 'certificateName'
  Prelude.Text ->
  DeleteLoadBalancerTlsCertificate
newDeleteLoadBalancerTlsCertificate
  pLoadBalancerName_
  pCertificateName_ =
    DeleteLoadBalancerTlsCertificate'
      { force =
          Prelude.Nothing,
        loadBalancerName = pLoadBalancerName_,
        certificateName = pCertificateName_
      }

-- | When @true@, forces the deletion of an SSL\/TLS certificate.
--
-- There can be two certificates associated with a Lightsail load balancer:
-- the primary and the backup. The @force@ parameter is required when the
-- primary SSL\/TLS certificate is in use by an instance attached to the
-- load balancer.
deleteLoadBalancerTlsCertificate_force :: Lens.Lens' DeleteLoadBalancerTlsCertificate (Prelude.Maybe Prelude.Bool)
deleteLoadBalancerTlsCertificate_force = Lens.lens (\DeleteLoadBalancerTlsCertificate' {force} -> force) (\s@DeleteLoadBalancerTlsCertificate' {} a -> s {force = a} :: DeleteLoadBalancerTlsCertificate)

-- | The load balancer name.
deleteLoadBalancerTlsCertificate_loadBalancerName :: Lens.Lens' DeleteLoadBalancerTlsCertificate Prelude.Text
deleteLoadBalancerTlsCertificate_loadBalancerName = Lens.lens (\DeleteLoadBalancerTlsCertificate' {loadBalancerName} -> loadBalancerName) (\s@DeleteLoadBalancerTlsCertificate' {} a -> s {loadBalancerName = a} :: DeleteLoadBalancerTlsCertificate)

-- | The SSL\/TLS certificate name.
deleteLoadBalancerTlsCertificate_certificateName :: Lens.Lens' DeleteLoadBalancerTlsCertificate Prelude.Text
deleteLoadBalancerTlsCertificate_certificateName = Lens.lens (\DeleteLoadBalancerTlsCertificate' {certificateName} -> certificateName) (\s@DeleteLoadBalancerTlsCertificate' {} a -> s {certificateName = a} :: DeleteLoadBalancerTlsCertificate)

instance
  Prelude.AWSRequest
    DeleteLoadBalancerTlsCertificate
  where
  type
    Rs DeleteLoadBalancerTlsCertificate =
      DeleteLoadBalancerTlsCertificateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteLoadBalancerTlsCertificateResponse'
            Prelude.<$> ( x Prelude..?> "operations"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteLoadBalancerTlsCertificate

instance
  Prelude.NFData
    DeleteLoadBalancerTlsCertificate

instance
  Prelude.ToHeaders
    DeleteLoadBalancerTlsCertificate
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Lightsail_20161128.DeleteLoadBalancerTlsCertificate" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    DeleteLoadBalancerTlsCertificate
  where
  toJSON DeleteLoadBalancerTlsCertificate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("force" Prelude..=) Prelude.<$> force,
            Prelude.Just
              ("loadBalancerName" Prelude..= loadBalancerName),
            Prelude.Just
              ("certificateName" Prelude..= certificateName)
          ]
      )

instance
  Prelude.ToPath
    DeleteLoadBalancerTlsCertificate
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DeleteLoadBalancerTlsCertificate
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteLoadBalancerTlsCertificateResponse' smart constructor.
data DeleteLoadBalancerTlsCertificateResponse = DeleteLoadBalancerTlsCertificateResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteLoadBalancerTlsCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'deleteLoadBalancerTlsCertificateResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'deleteLoadBalancerTlsCertificateResponse_httpStatus' - The response's http status code.
newDeleteLoadBalancerTlsCertificateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteLoadBalancerTlsCertificateResponse
newDeleteLoadBalancerTlsCertificateResponse
  pHttpStatus_ =
    DeleteLoadBalancerTlsCertificateResponse'
      { operations =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
deleteLoadBalancerTlsCertificateResponse_operations :: Lens.Lens' DeleteLoadBalancerTlsCertificateResponse (Prelude.Maybe [Operation])
deleteLoadBalancerTlsCertificateResponse_operations = Lens.lens (\DeleteLoadBalancerTlsCertificateResponse' {operations} -> operations) (\s@DeleteLoadBalancerTlsCertificateResponse' {} a -> s {operations = a} :: DeleteLoadBalancerTlsCertificateResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
deleteLoadBalancerTlsCertificateResponse_httpStatus :: Lens.Lens' DeleteLoadBalancerTlsCertificateResponse Prelude.Int
deleteLoadBalancerTlsCertificateResponse_httpStatus = Lens.lens (\DeleteLoadBalancerTlsCertificateResponse' {httpStatus} -> httpStatus) (\s@DeleteLoadBalancerTlsCertificateResponse' {} a -> s {httpStatus = a} :: DeleteLoadBalancerTlsCertificateResponse)

instance
  Prelude.NFData
    DeleteLoadBalancerTlsCertificateResponse
