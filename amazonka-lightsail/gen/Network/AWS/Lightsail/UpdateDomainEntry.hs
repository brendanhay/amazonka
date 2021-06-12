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
-- Module      : Network.AWS.Lightsail.UpdateDomainEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a domain recordset after it is created.
--
-- The @update domain entry@ operation supports tag-based access control
-- via resource tags applied to the resource identified by @domain name@.
-- For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.UpdateDomainEntry
  ( -- * Creating a Request
    UpdateDomainEntry (..),
    newUpdateDomainEntry,

    -- * Request Lenses
    updateDomainEntry_domainName,
    updateDomainEntry_domainEntry,

    -- * Destructuring the Response
    UpdateDomainEntryResponse (..),
    newUpdateDomainEntryResponse,

    -- * Response Lenses
    updateDomainEntryResponse_operations,
    updateDomainEntryResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateDomainEntry' smart constructor.
data UpdateDomainEntry = UpdateDomainEntry'
  { -- | The name of the domain recordset to update.
    domainName :: Core.Text,
    -- | An array of key-value pairs containing information about the domain
    -- entry.
    domainEntry :: DomainEntry
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDomainEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'updateDomainEntry_domainName' - The name of the domain recordset to update.
--
-- 'domainEntry', 'updateDomainEntry_domainEntry' - An array of key-value pairs containing information about the domain
-- entry.
newUpdateDomainEntry ::
  -- | 'domainName'
  Core.Text ->
  -- | 'domainEntry'
  DomainEntry ->
  UpdateDomainEntry
newUpdateDomainEntry pDomainName_ pDomainEntry_ =
  UpdateDomainEntry'
    { domainName = pDomainName_,
      domainEntry = pDomainEntry_
    }

-- | The name of the domain recordset to update.
updateDomainEntry_domainName :: Lens.Lens' UpdateDomainEntry Core.Text
updateDomainEntry_domainName = Lens.lens (\UpdateDomainEntry' {domainName} -> domainName) (\s@UpdateDomainEntry' {} a -> s {domainName = a} :: UpdateDomainEntry)

-- | An array of key-value pairs containing information about the domain
-- entry.
updateDomainEntry_domainEntry :: Lens.Lens' UpdateDomainEntry DomainEntry
updateDomainEntry_domainEntry = Lens.lens (\UpdateDomainEntry' {domainEntry} -> domainEntry) (\s@UpdateDomainEntry' {} a -> s {domainEntry = a} :: UpdateDomainEntry)

instance Core.AWSRequest UpdateDomainEntry where
  type
    AWSResponse UpdateDomainEntry =
      UpdateDomainEntryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDomainEntryResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateDomainEntry

instance Core.NFData UpdateDomainEntry

instance Core.ToHeaders UpdateDomainEntry where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.UpdateDomainEntry" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateDomainEntry where
  toJSON UpdateDomainEntry' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("domainName" Core..= domainName),
            Core.Just ("domainEntry" Core..= domainEntry)
          ]
      )

instance Core.ToPath UpdateDomainEntry where
  toPath = Core.const "/"

instance Core.ToQuery UpdateDomainEntry where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateDomainEntryResponse' smart constructor.
data UpdateDomainEntryResponse = UpdateDomainEntryResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDomainEntryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'updateDomainEntryResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'updateDomainEntryResponse_httpStatus' - The response's http status code.
newUpdateDomainEntryResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateDomainEntryResponse
newUpdateDomainEntryResponse pHttpStatus_ =
  UpdateDomainEntryResponse'
    { operations =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
updateDomainEntryResponse_operations :: Lens.Lens' UpdateDomainEntryResponse (Core.Maybe [Operation])
updateDomainEntryResponse_operations = Lens.lens (\UpdateDomainEntryResponse' {operations} -> operations) (\s@UpdateDomainEntryResponse' {} a -> s {operations = a} :: UpdateDomainEntryResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
updateDomainEntryResponse_httpStatus :: Lens.Lens' UpdateDomainEntryResponse Core.Int
updateDomainEntryResponse_httpStatus = Lens.lens (\UpdateDomainEntryResponse' {httpStatus} -> httpStatus) (\s@UpdateDomainEntryResponse' {} a -> s {httpStatus = a} :: UpdateDomainEntryResponse)

instance Core.NFData UpdateDomainEntryResponse
