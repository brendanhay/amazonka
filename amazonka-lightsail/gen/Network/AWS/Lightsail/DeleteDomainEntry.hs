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
-- Module      : Network.AWS.Lightsail.DeleteDomainEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific domain entry.
--
-- The @delete domain entry@ operation supports tag-based access control
-- via resource tags applied to the resource identified by @domain name@.
-- For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.DeleteDomainEntry
  ( -- * Creating a Request
    DeleteDomainEntry (..),
    newDeleteDomainEntry,

    -- * Request Lenses
    deleteDomainEntry_domainName,
    deleteDomainEntry_domainEntry,

    -- * Destructuring the Response
    DeleteDomainEntryResponse (..),
    newDeleteDomainEntryResponse,

    -- * Response Lenses
    deleteDomainEntryResponse_operation,
    deleteDomainEntryResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteDomainEntry' smart constructor.
data DeleteDomainEntry = DeleteDomainEntry'
  { -- | The name of the domain entry to delete.
    domainName :: Core.Text,
    -- | An array of key-value pairs containing information about your domain
    -- entries.
    domainEntry :: DomainEntry
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDomainEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'deleteDomainEntry_domainName' - The name of the domain entry to delete.
--
-- 'domainEntry', 'deleteDomainEntry_domainEntry' - An array of key-value pairs containing information about your domain
-- entries.
newDeleteDomainEntry ::
  -- | 'domainName'
  Core.Text ->
  -- | 'domainEntry'
  DomainEntry ->
  DeleteDomainEntry
newDeleteDomainEntry pDomainName_ pDomainEntry_ =
  DeleteDomainEntry'
    { domainName = pDomainName_,
      domainEntry = pDomainEntry_
    }

-- | The name of the domain entry to delete.
deleteDomainEntry_domainName :: Lens.Lens' DeleteDomainEntry Core.Text
deleteDomainEntry_domainName = Lens.lens (\DeleteDomainEntry' {domainName} -> domainName) (\s@DeleteDomainEntry' {} a -> s {domainName = a} :: DeleteDomainEntry)

-- | An array of key-value pairs containing information about your domain
-- entries.
deleteDomainEntry_domainEntry :: Lens.Lens' DeleteDomainEntry DomainEntry
deleteDomainEntry_domainEntry = Lens.lens (\DeleteDomainEntry' {domainEntry} -> domainEntry) (\s@DeleteDomainEntry' {} a -> s {domainEntry = a} :: DeleteDomainEntry)

instance Core.AWSRequest DeleteDomainEntry where
  type
    AWSResponse DeleteDomainEntry =
      DeleteDomainEntryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDomainEntryResponse'
            Core.<$> (x Core..?> "operation")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteDomainEntry

instance Core.NFData DeleteDomainEntry

instance Core.ToHeaders DeleteDomainEntry where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.DeleteDomainEntry" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteDomainEntry where
  toJSON DeleteDomainEntry' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("domainName" Core..= domainName),
            Core.Just ("domainEntry" Core..= domainEntry)
          ]
      )

instance Core.ToPath DeleteDomainEntry where
  toPath = Core.const "/"

instance Core.ToQuery DeleteDomainEntry where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteDomainEntryResponse' smart constructor.
data DeleteDomainEntryResponse = DeleteDomainEntryResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operation :: Core.Maybe Operation,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDomainEntryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operation', 'deleteDomainEntryResponse_operation' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'deleteDomainEntryResponse_httpStatus' - The response's http status code.
newDeleteDomainEntryResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteDomainEntryResponse
newDeleteDomainEntryResponse pHttpStatus_ =
  DeleteDomainEntryResponse'
    { operation =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
deleteDomainEntryResponse_operation :: Lens.Lens' DeleteDomainEntryResponse (Core.Maybe Operation)
deleteDomainEntryResponse_operation = Lens.lens (\DeleteDomainEntryResponse' {operation} -> operation) (\s@DeleteDomainEntryResponse' {} a -> s {operation = a} :: DeleteDomainEntryResponse)

-- | The response's http status code.
deleteDomainEntryResponse_httpStatus :: Lens.Lens' DeleteDomainEntryResponse Core.Int
deleteDomainEntryResponse_httpStatus = Lens.lens (\DeleteDomainEntryResponse' {httpStatus} -> httpStatus) (\s@DeleteDomainEntryResponse' {} a -> s {httpStatus = a} :: DeleteDomainEntryResponse)

instance Core.NFData DeleteDomainEntryResponse
