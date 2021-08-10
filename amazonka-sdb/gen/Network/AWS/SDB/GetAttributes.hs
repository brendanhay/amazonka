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
-- Module      : Network.AWS.SDB.GetAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.SDB.GetAttributes
  ( -- * Creating a Request
    GetAttributes (..),
    newGetAttributes,

    -- * Request Lenses
    getAttributes_consistentRead,
    getAttributes_attributeNames,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SDB.Types

-- | /See:/ 'newGetAttributes' smart constructor.
data GetAttributes = GetAttributes'
  { -- | Determines whether or not strong consistency should be enforced when
    -- data is read from SimpleDB. If @true@, any data previously written to
    -- SimpleDB will be returned. Otherwise, results will be consistent
    -- eventually, and the client may not see data that was written immediately
    -- before your read.
    consistentRead :: Prelude.Maybe Prelude.Bool,
    -- | The names of the attributes.
    attributeNames :: Prelude.Maybe [Prelude.Text],
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
-- 'consistentRead', 'getAttributes_consistentRead' - Determines whether or not strong consistency should be enforced when
-- data is read from SimpleDB. If @true@, any data previously written to
-- SimpleDB will be returned. Otherwise, results will be consistent
-- eventually, and the client may not see data that was written immediately
-- before your read.
--
-- 'attributeNames', 'getAttributes_attributeNames' - The names of the attributes.
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
    { consistentRead = Prelude.Nothing,
      attributeNames = Prelude.Nothing,
      domainName = pDomainName_,
      itemName = pItemName_
    }

-- | Determines whether or not strong consistency should be enforced when
-- data is read from SimpleDB. If @true@, any data previously written to
-- SimpleDB will be returned. Otherwise, results will be consistent
-- eventually, and the client may not see data that was written immediately
-- before your read.
getAttributes_consistentRead :: Lens.Lens' GetAttributes (Prelude.Maybe Prelude.Bool)
getAttributes_consistentRead = Lens.lens (\GetAttributes' {consistentRead} -> consistentRead) (\s@GetAttributes' {} a -> s {consistentRead = a} :: GetAttributes)

-- | The names of the attributes.
getAttributes_attributeNames :: Lens.Lens' GetAttributes (Prelude.Maybe [Prelude.Text])
getAttributes_attributeNames = Lens.lens (\GetAttributes' {attributeNames} -> attributeNames) (\s@GetAttributes' {} a -> s {attributeNames = a} :: GetAttributes) Prelude.. Lens.mapping Lens._Coerce

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
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetAttributesResult"
      ( \s h x ->
          GetAttributesResponse'
            Prelude.<$> (Core.may (Core.parseXMLList "Attribute") x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAttributes

instance Prelude.NFData GetAttributes

instance Core.ToHeaders GetAttributes where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetAttributes where
  toPath = Prelude.const "/"

instance Core.ToQuery GetAttributes where
  toQuery GetAttributes' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("GetAttributes" :: Prelude.ByteString),
        "Version"
          Core.=: ("2009-04-15" :: Prelude.ByteString),
        "ConsistentRead" Core.=: consistentRead,
        Core.toQuery
          ( Core.toQueryList "AttributeName"
              Prelude.<$> attributeNames
          ),
        "DomainName" Core.=: domainName,
        "ItemName" Core.=: itemName
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
getAttributesResponse_attributes = Lens.lens (\GetAttributesResponse' {attributes} -> attributes) (\s@GetAttributesResponse' {} a -> s {attributes = a} :: GetAttributesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getAttributesResponse_httpStatus :: Lens.Lens' GetAttributesResponse Prelude.Int
getAttributesResponse_httpStatus = Lens.lens (\GetAttributesResponse' {httpStatus} -> httpStatus) (\s@GetAttributesResponse' {} a -> s {httpStatus = a} :: GetAttributesResponse)

instance Prelude.NFData GetAttributesResponse
