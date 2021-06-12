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
-- Module      : Network.AWS.Lightsail.DeleteDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified domain recordset and all of its domain records.
--
-- The @delete domain@ operation supports tag-based access control via
-- resource tags applied to the resource identified by @domain name@. For
-- more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.DeleteDomain
  ( -- * Creating a Request
    DeleteDomain (..),
    newDeleteDomain,

    -- * Request Lenses
    deleteDomain_domainName,

    -- * Destructuring the Response
    DeleteDomainResponse (..),
    newDeleteDomainResponse,

    -- * Response Lenses
    deleteDomainResponse_operation,
    deleteDomainResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteDomain' smart constructor.
data DeleteDomain = DeleteDomain'
  { -- | The specific domain name to delete.
    domainName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'deleteDomain_domainName' - The specific domain name to delete.
newDeleteDomain ::
  -- | 'domainName'
  Core.Text ->
  DeleteDomain
newDeleteDomain pDomainName_ =
  DeleteDomain' {domainName = pDomainName_}

-- | The specific domain name to delete.
deleteDomain_domainName :: Lens.Lens' DeleteDomain Core.Text
deleteDomain_domainName = Lens.lens (\DeleteDomain' {domainName} -> domainName) (\s@DeleteDomain' {} a -> s {domainName = a} :: DeleteDomain)

instance Core.AWSRequest DeleteDomain where
  type AWSResponse DeleteDomain = DeleteDomainResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDomainResponse'
            Core.<$> (x Core..?> "operation")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteDomain

instance Core.NFData DeleteDomain

instance Core.ToHeaders DeleteDomain where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.DeleteDomain" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteDomain where
  toJSON DeleteDomain' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("domainName" Core..= domainName)]
      )

instance Core.ToPath DeleteDomain where
  toPath = Core.const "/"

instance Core.ToQuery DeleteDomain where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteDomainResponse' smart constructor.
data DeleteDomainResponse = DeleteDomainResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operation :: Core.Maybe Operation,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operation', 'deleteDomainResponse_operation' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'deleteDomainResponse_httpStatus' - The response's http status code.
newDeleteDomainResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteDomainResponse
newDeleteDomainResponse pHttpStatus_ =
  DeleteDomainResponse'
    { operation = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
deleteDomainResponse_operation :: Lens.Lens' DeleteDomainResponse (Core.Maybe Operation)
deleteDomainResponse_operation = Lens.lens (\DeleteDomainResponse' {operation} -> operation) (\s@DeleteDomainResponse' {} a -> s {operation = a} :: DeleteDomainResponse)

-- | The response's http status code.
deleteDomainResponse_httpStatus :: Lens.Lens' DeleteDomainResponse Core.Int
deleteDomainResponse_httpStatus = Lens.lens (\DeleteDomainResponse' {httpStatus} -> httpStatus) (\s@DeleteDomainResponse' {} a -> s {httpStatus = a} :: DeleteDomainResponse)

instance Core.NFData DeleteDomainResponse
