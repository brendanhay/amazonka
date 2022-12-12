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
-- Module      : Amazonka.SDB.GetAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all of the attributes associated with the specified item.
-- Optionally, the attributes returned can be limited to one or more
-- attributes by specifying an attribute name parameter.
--
-- If the item does not exist on the replica that was accessed for this
-- operation, an empty set is returned. The system does not return an error
-- as it cannot guarantee the item does not exist on other replicas.
module Amazonka.SDB.GetAttributes
  ( -- * Creating a Request
    GetAttributes (..),
    newGetAttributes,

    -- * Request Lenses
    getAttributes_attributeNames,
    getAttributes_consistentRead,
    getAttributes_domainName,
    getAttributes_itemName,

    -- * Destructuring the Response
    GetAttributesResponse (..),
    newGetAttributesResponse,

    -- * Response Lenses
    getAttributesResponse_attributes,
    getAttributesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SDB.Types

-- | /See:/ 'newGetAttributes' smart constructor.
data GetAttributes = GetAttributes'
  { -- | The names of the attributes.
    attributeNames :: Prelude.Maybe [Prelude.Text],
    -- | Determines whether or not strong consistency should be enforced when
    -- data is read from SimpleDB. If @true@, any data previously written to
    -- SimpleDB will be returned. Otherwise, results will be consistent
    -- eventually, and the client may not see data that was written immediately
    -- before your read.
    consistentRead :: Prelude.Maybe Prelude.Bool,
    -- | The name of the domain in which to perform the operation.
    domainName :: Prelude.Text,
    -- | The name of the item.
    itemName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeNames', 'getAttributes_attributeNames' - The names of the attributes.
--
-- 'consistentRead', 'getAttributes_consistentRead' - Determines whether or not strong consistency should be enforced when
-- data is read from SimpleDB. If @true@, any data previously written to
-- SimpleDB will be returned. Otherwise, results will be consistent
-- eventually, and the client may not see data that was written immediately
-- before your read.
--
-- 'domainName', 'getAttributes_domainName' - The name of the domain in which to perform the operation.
--
-- 'itemName', 'getAttributes_itemName' - The name of the item.
newGetAttributes ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'itemName'
  Prelude.Text ->
  GetAttributes
newGetAttributes pDomainName_ pItemName_ =
  GetAttributes'
    { attributeNames = Prelude.Nothing,
      consistentRead = Prelude.Nothing,
      domainName = pDomainName_,
      itemName = pItemName_
    }

-- | The names of the attributes.
getAttributes_attributeNames :: Lens.Lens' GetAttributes (Prelude.Maybe [Prelude.Text])
getAttributes_attributeNames = Lens.lens (\GetAttributes' {attributeNames} -> attributeNames) (\s@GetAttributes' {} a -> s {attributeNames = a} :: GetAttributes) Prelude.. Lens.mapping Lens.coerced

-- | Determines whether or not strong consistency should be enforced when
-- data is read from SimpleDB. If @true@, any data previously written to
-- SimpleDB will be returned. Otherwise, results will be consistent
-- eventually, and the client may not see data that was written immediately
-- before your read.
getAttributes_consistentRead :: Lens.Lens' GetAttributes (Prelude.Maybe Prelude.Bool)
getAttributes_consistentRead = Lens.lens (\GetAttributes' {consistentRead} -> consistentRead) (\s@GetAttributes' {} a -> s {consistentRead = a} :: GetAttributes)

-- | The name of the domain in which to perform the operation.
getAttributes_domainName :: Lens.Lens' GetAttributes Prelude.Text
getAttributes_domainName = Lens.lens (\GetAttributes' {domainName} -> domainName) (\s@GetAttributes' {} a -> s {domainName = a} :: GetAttributes)

-- | The name of the item.
getAttributes_itemName :: Lens.Lens' GetAttributes Prelude.Text
getAttributes_itemName = Lens.lens (\GetAttributes' {itemName} -> itemName) (\s@GetAttributes' {} a -> s {itemName = a} :: GetAttributes)

instance Core.AWSRequest GetAttributes where
  type
    AWSResponse GetAttributes =
      GetAttributesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetAttributesResult"
      ( \s h x ->
          GetAttributesResponse'
            Prelude.<$> (Core.may (Data.parseXMLList "Attribute") x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAttributes where
  hashWithSalt _salt GetAttributes' {..} =
    _salt `Prelude.hashWithSalt` attributeNames
      `Prelude.hashWithSalt` consistentRead
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` itemName

instance Prelude.NFData GetAttributes where
  rnf GetAttributes' {..} =
    Prelude.rnf attributeNames
      `Prelude.seq` Prelude.rnf consistentRead
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf itemName

instance Data.ToHeaders GetAttributes where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetAttributes where
  toPath = Prelude.const "/"

instance Data.ToQuery GetAttributes where
  toQuery GetAttributes' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("GetAttributes" :: Prelude.ByteString),
        "Version"
          Data.=: ("2009-04-15" :: Prelude.ByteString),
        Data.toQuery
          ( Data.toQueryList "AttributeName"
              Prelude.<$> attributeNames
          ),
        "ConsistentRead" Data.=: consistentRead,
        "DomainName" Data.=: domainName,
        "ItemName" Data.=: itemName
      ]

-- | /See:/ 'newGetAttributesResponse' smart constructor.
data GetAttributesResponse = GetAttributesResponse'
  { -- | The list of attributes returned by the operation.
    attributes :: Prelude.Maybe [Attribute],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'getAttributesResponse_attributes' - The list of attributes returned by the operation.
--
-- 'httpStatus', 'getAttributesResponse_httpStatus' - The response's http status code.
newGetAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAttributesResponse
newGetAttributesResponse pHttpStatus_ =
  GetAttributesResponse'
    { attributes =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of attributes returned by the operation.
getAttributesResponse_attributes :: Lens.Lens' GetAttributesResponse (Prelude.Maybe [Attribute])
getAttributesResponse_attributes = Lens.lens (\GetAttributesResponse' {attributes} -> attributes) (\s@GetAttributesResponse' {} a -> s {attributes = a} :: GetAttributesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getAttributesResponse_httpStatus :: Lens.Lens' GetAttributesResponse Prelude.Int
getAttributesResponse_httpStatus = Lens.lens (\GetAttributesResponse' {httpStatus} -> httpStatus) (\s@GetAttributesResponse' {} a -> s {httpStatus = a} :: GetAttributesResponse)

instance Prelude.NFData GetAttributesResponse where
  rnf GetAttributesResponse' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf httpStatus
