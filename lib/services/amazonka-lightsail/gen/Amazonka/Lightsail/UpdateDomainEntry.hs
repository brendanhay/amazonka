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
-- Module      : Amazonka.Lightsail.UpdateDomainEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-controlling-access-using-tags Amazon Lightsail Developer Guide>.
module Amazonka.Lightsail.UpdateDomainEntry
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDomainEntry' smart constructor.
data UpdateDomainEntry = UpdateDomainEntry'
  { -- | The name of the domain recordset to update.
    domainName :: Prelude.Text,
    -- | An array of key-value pairs containing information about the domain
    -- entry.
    domainEntry :: DomainEntry
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'domainEntry'
  DomainEntry ->
  UpdateDomainEntry
newUpdateDomainEntry pDomainName_ pDomainEntry_ =
  UpdateDomainEntry'
    { domainName = pDomainName_,
      domainEntry = pDomainEntry_
    }

-- | The name of the domain recordset to update.
updateDomainEntry_domainName :: Lens.Lens' UpdateDomainEntry Prelude.Text
updateDomainEntry_domainName = Lens.lens (\UpdateDomainEntry' {domainName} -> domainName) (\s@UpdateDomainEntry' {} a -> s {domainName = a} :: UpdateDomainEntry)

-- | An array of key-value pairs containing information about the domain
-- entry.
updateDomainEntry_domainEntry :: Lens.Lens' UpdateDomainEntry DomainEntry
updateDomainEntry_domainEntry = Lens.lens (\UpdateDomainEntry' {domainEntry} -> domainEntry) (\s@UpdateDomainEntry' {} a -> s {domainEntry = a} :: UpdateDomainEntry)

instance Core.AWSRequest UpdateDomainEntry where
  type
    AWSResponse UpdateDomainEntry =
      UpdateDomainEntryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDomainEntryResponse'
            Prelude.<$> (x Core..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDomainEntry where
  hashWithSalt _salt UpdateDomainEntry' {..} =
    _salt `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` domainEntry

instance Prelude.NFData UpdateDomainEntry where
  rnf UpdateDomainEntry' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf domainEntry

instance Core.ToHeaders UpdateDomainEntry where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.UpdateDomainEntry" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateDomainEntry where
  toJSON UpdateDomainEntry' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("domainName" Core..= domainName),
            Prelude.Just ("domainEntry" Core..= domainEntry)
          ]
      )

instance Core.ToPath UpdateDomainEntry where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateDomainEntry where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDomainEntryResponse' smart constructor.
data UpdateDomainEntryResponse = UpdateDomainEntryResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateDomainEntryResponse
newUpdateDomainEntryResponse pHttpStatus_ =
  UpdateDomainEntryResponse'
    { operations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
updateDomainEntryResponse_operations :: Lens.Lens' UpdateDomainEntryResponse (Prelude.Maybe [Operation])
updateDomainEntryResponse_operations = Lens.lens (\UpdateDomainEntryResponse' {operations} -> operations) (\s@UpdateDomainEntryResponse' {} a -> s {operations = a} :: UpdateDomainEntryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateDomainEntryResponse_httpStatus :: Lens.Lens' UpdateDomainEntryResponse Prelude.Int
updateDomainEntryResponse_httpStatus = Lens.lens (\UpdateDomainEntryResponse' {httpStatus} -> httpStatus) (\s@UpdateDomainEntryResponse' {} a -> s {httpStatus = a} :: UpdateDomainEntryResponse)

instance Prelude.NFData UpdateDomainEntryResponse where
  rnf UpdateDomainEntryResponse' {..} =
    Prelude.rnf operations
      `Prelude.seq` Prelude.rnf httpStatus
