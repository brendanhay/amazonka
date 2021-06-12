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
    consistentRead :: Core.Maybe Core.Bool,
    -- | The names of the attributes.
    attributeNames :: Core.Maybe [Core.Text],
    -- | The name of the domain in which to perform the operation.
    domainName :: Core.Text,
    -- | The name of the item.
    itemName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'itemName'
  Core.Text ->
  GetAttributes
newGetAttributes pDomainName_ pItemName_ =
  GetAttributes'
    { consistentRead = Core.Nothing,
      attributeNames = Core.Nothing,
      domainName = pDomainName_,
      itemName = pItemName_
    }

-- | Determines whether or not strong consistency should be enforced when
-- data is read from SimpleDB. If @true@, any data previously written to
-- SimpleDB will be returned. Otherwise, results will be consistent
-- eventually, and the client may not see data that was written immediately
-- before your read.
getAttributes_consistentRead :: Lens.Lens' GetAttributes (Core.Maybe Core.Bool)
getAttributes_consistentRead = Lens.lens (\GetAttributes' {consistentRead} -> consistentRead) (\s@GetAttributes' {} a -> s {consistentRead = a} :: GetAttributes)

-- | The names of the attributes.
getAttributes_attributeNames :: Lens.Lens' GetAttributes (Core.Maybe [Core.Text])
getAttributes_attributeNames = Lens.lens (\GetAttributes' {attributeNames} -> attributeNames) (\s@GetAttributes' {} a -> s {attributeNames = a} :: GetAttributes) Core.. Lens.mapping Lens._Coerce

-- | The name of the domain in which to perform the operation.
getAttributes_domainName :: Lens.Lens' GetAttributes Core.Text
getAttributes_domainName = Lens.lens (\GetAttributes' {domainName} -> domainName) (\s@GetAttributes' {} a -> s {domainName = a} :: GetAttributes)

-- | The name of the item.
getAttributes_itemName :: Lens.Lens' GetAttributes Core.Text
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
            Core.<$> (Core.may (Core.parseXMLList "Attribute") x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetAttributes

instance Core.NFData GetAttributes

instance Core.ToHeaders GetAttributes where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetAttributes where
  toPath = Core.const "/"

instance Core.ToQuery GetAttributes where
  toQuery GetAttributes' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("GetAttributes" :: Core.ByteString),
        "Version" Core.=: ("2009-04-15" :: Core.ByteString),
        "ConsistentRead" Core.=: consistentRead,
        Core.toQuery
          ( Core.toQueryList "AttributeName"
              Core.<$> attributeNames
          ),
        "DomainName" Core.=: domainName,
        "ItemName" Core.=: itemName
      ]

-- | /See:/ 'newGetAttributesResponse' smart constructor.
data GetAttributesResponse = GetAttributesResponse'
  { -- | The list of attributes returned by the operation.
    attributes :: Core.Maybe [Attribute],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetAttributesResponse
newGetAttributesResponse pHttpStatus_ =
  GetAttributesResponse'
    { attributes = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of attributes returned by the operation.
getAttributesResponse_attributes :: Lens.Lens' GetAttributesResponse (Core.Maybe [Attribute])
getAttributesResponse_attributes = Lens.lens (\GetAttributesResponse' {attributes} -> attributes) (\s@GetAttributesResponse' {} a -> s {attributes = a} :: GetAttributesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getAttributesResponse_httpStatus :: Lens.Lens' GetAttributesResponse Core.Int
getAttributesResponse_httpStatus = Lens.lens (\GetAttributesResponse' {httpStatus} -> httpStatus) (\s@GetAttributesResponse' {} a -> s {httpStatus = a} :: GetAttributesResponse)

instance Core.NFData GetAttributesResponse
