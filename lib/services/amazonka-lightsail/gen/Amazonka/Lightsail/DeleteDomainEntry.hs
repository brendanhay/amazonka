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
-- Module      : Amazonka.Lightsail.DeleteDomainEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-controlling-access-using-tags Amazon Lightsail Developer Guide>.
module Amazonka.Lightsail.DeleteDomainEntry
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDomainEntry' smart constructor.
data DeleteDomainEntry = DeleteDomainEntry'
  { -- | The name of the domain entry to delete.
    domainName :: Prelude.Text,
    -- | An array of key-value pairs containing information about your domain
    -- entries.
    domainEntry :: DomainEntry
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'domainEntry'
  DomainEntry ->
  DeleteDomainEntry
newDeleteDomainEntry pDomainName_ pDomainEntry_ =
  DeleteDomainEntry'
    { domainName = pDomainName_,
      domainEntry = pDomainEntry_
    }

-- | The name of the domain entry to delete.
deleteDomainEntry_domainName :: Lens.Lens' DeleteDomainEntry Prelude.Text
deleteDomainEntry_domainName = Lens.lens (\DeleteDomainEntry' {domainName} -> domainName) (\s@DeleteDomainEntry' {} a -> s {domainName = a} :: DeleteDomainEntry)

-- | An array of key-value pairs containing information about your domain
-- entries.
deleteDomainEntry_domainEntry :: Lens.Lens' DeleteDomainEntry DomainEntry
deleteDomainEntry_domainEntry = Lens.lens (\DeleteDomainEntry' {domainEntry} -> domainEntry) (\s@DeleteDomainEntry' {} a -> s {domainEntry = a} :: DeleteDomainEntry)

instance Core.AWSRequest DeleteDomainEntry where
  type
    AWSResponse DeleteDomainEntry =
      DeleteDomainEntryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDomainEntryResponse'
            Prelude.<$> (x Data..?> "operation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDomainEntry where
  hashWithSalt _salt DeleteDomainEntry' {..} =
    _salt `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` domainEntry

instance Prelude.NFData DeleteDomainEntry where
  rnf DeleteDomainEntry' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf domainEntry

instance Data.ToHeaders DeleteDomainEntry where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.DeleteDomainEntry" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteDomainEntry where
  toJSON DeleteDomainEntry' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("domainName" Data..= domainName),
            Prelude.Just ("domainEntry" Data..= domainEntry)
          ]
      )

instance Data.ToPath DeleteDomainEntry where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDomainEntry where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDomainEntryResponse' smart constructor.
data DeleteDomainEntryResponse = DeleteDomainEntryResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operation :: Prelude.Maybe Operation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteDomainEntryResponse
newDeleteDomainEntryResponse pHttpStatus_ =
  DeleteDomainEntryResponse'
    { operation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
deleteDomainEntryResponse_operation :: Lens.Lens' DeleteDomainEntryResponse (Prelude.Maybe Operation)
deleteDomainEntryResponse_operation = Lens.lens (\DeleteDomainEntryResponse' {operation} -> operation) (\s@DeleteDomainEntryResponse' {} a -> s {operation = a} :: DeleteDomainEntryResponse)

-- | The response's http status code.
deleteDomainEntryResponse_httpStatus :: Lens.Lens' DeleteDomainEntryResponse Prelude.Int
deleteDomainEntryResponse_httpStatus = Lens.lens (\DeleteDomainEntryResponse' {httpStatus} -> httpStatus) (\s@DeleteDomainEntryResponse' {} a -> s {httpStatus = a} :: DeleteDomainEntryResponse)

instance Prelude.NFData DeleteDomainEntryResponse where
  rnf DeleteDomainEntryResponse' {..} =
    Prelude.rnf operation
      `Prelude.seq` Prelude.rnf httpStatus
