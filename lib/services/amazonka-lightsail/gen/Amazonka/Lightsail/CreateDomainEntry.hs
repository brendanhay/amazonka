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
-- Module      : Amazonka.Lightsail.CreateDomainEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one of the following domain name system (DNS) records in a
-- domain DNS zone: Address (A), canonical name (CNAME), mail exchanger
-- (MX), name server (NS), start of authority (SOA), service locator (SRV),
-- or text (TXT).
--
-- The @create domain entry@ operation supports tag-based access control
-- via resource tags applied to the resource identified by @domain name@.
-- For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-controlling-access-using-tags Amazon Lightsail Developer Guide>.
module Amazonka.Lightsail.CreateDomainEntry
  ( -- * Creating a Request
    CreateDomainEntry (..),
    newCreateDomainEntry,

    -- * Request Lenses
    createDomainEntry_domainName,
    createDomainEntry_domainEntry,

    -- * Destructuring the Response
    CreateDomainEntryResponse (..),
    newCreateDomainEntryResponse,

    -- * Response Lenses
    createDomainEntryResponse_operation,
    createDomainEntryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDomainEntry' smart constructor.
data CreateDomainEntry = CreateDomainEntry'
  { -- | The domain name (e.g., @example.com@) for which you want to create the
    -- domain entry.
    domainName :: Prelude.Text,
    -- | An array of key-value pairs containing information about the domain
    -- entry request.
    domainEntry :: DomainEntry
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDomainEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'createDomainEntry_domainName' - The domain name (e.g., @example.com@) for which you want to create the
-- domain entry.
--
-- 'domainEntry', 'createDomainEntry_domainEntry' - An array of key-value pairs containing information about the domain
-- entry request.
newCreateDomainEntry ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'domainEntry'
  DomainEntry ->
  CreateDomainEntry
newCreateDomainEntry pDomainName_ pDomainEntry_ =
  CreateDomainEntry'
    { domainName = pDomainName_,
      domainEntry = pDomainEntry_
    }

-- | The domain name (e.g., @example.com@) for which you want to create the
-- domain entry.
createDomainEntry_domainName :: Lens.Lens' CreateDomainEntry Prelude.Text
createDomainEntry_domainName = Lens.lens (\CreateDomainEntry' {domainName} -> domainName) (\s@CreateDomainEntry' {} a -> s {domainName = a} :: CreateDomainEntry)

-- | An array of key-value pairs containing information about the domain
-- entry request.
createDomainEntry_domainEntry :: Lens.Lens' CreateDomainEntry DomainEntry
createDomainEntry_domainEntry = Lens.lens (\CreateDomainEntry' {domainEntry} -> domainEntry) (\s@CreateDomainEntry' {} a -> s {domainEntry = a} :: CreateDomainEntry)

instance Core.AWSRequest CreateDomainEntry where
  type
    AWSResponse CreateDomainEntry =
      CreateDomainEntryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDomainEntryResponse'
            Prelude.<$> (x Data..?> "operation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDomainEntry where
  hashWithSalt _salt CreateDomainEntry' {..} =
    _salt `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` domainEntry

instance Prelude.NFData CreateDomainEntry where
  rnf CreateDomainEntry' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf domainEntry

instance Data.ToHeaders CreateDomainEntry where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.CreateDomainEntry" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDomainEntry where
  toJSON CreateDomainEntry' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("domainName" Data..= domainName),
            Prelude.Just ("domainEntry" Data..= domainEntry)
          ]
      )

instance Data.ToPath CreateDomainEntry where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDomainEntry where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDomainEntryResponse' smart constructor.
data CreateDomainEntryResponse = CreateDomainEntryResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operation :: Prelude.Maybe Operation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDomainEntryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operation', 'createDomainEntryResponse_operation' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'createDomainEntryResponse_httpStatus' - The response's http status code.
newCreateDomainEntryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDomainEntryResponse
newCreateDomainEntryResponse pHttpStatus_ =
  CreateDomainEntryResponse'
    { operation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
createDomainEntryResponse_operation :: Lens.Lens' CreateDomainEntryResponse (Prelude.Maybe Operation)
createDomainEntryResponse_operation = Lens.lens (\CreateDomainEntryResponse' {operation} -> operation) (\s@CreateDomainEntryResponse' {} a -> s {operation = a} :: CreateDomainEntryResponse)

-- | The response's http status code.
createDomainEntryResponse_httpStatus :: Lens.Lens' CreateDomainEntryResponse Prelude.Int
createDomainEntryResponse_httpStatus = Lens.lens (\CreateDomainEntryResponse' {httpStatus} -> httpStatus) (\s@CreateDomainEntryResponse' {} a -> s {httpStatus = a} :: CreateDomainEntryResponse)

instance Prelude.NFData CreateDomainEntryResponse where
  rnf CreateDomainEntryResponse' {..} =
    Prelude.rnf operation
      `Prelude.seq` Prelude.rnf httpStatus
