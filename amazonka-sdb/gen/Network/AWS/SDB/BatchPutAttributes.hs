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
-- Module      : Network.AWS.SDB.BatchPutAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @BatchPutAttributes@ operation creates or replaces attributes within
-- one or more items. By using this operation, the client can perform
-- multiple PutAttribute operation with a single call. This helps yield
-- savings in round trips and latencies, enabling Amazon SimpleDB to
-- optimize requests and generally produce better throughput.
--
-- The client may specify the item name with the @Item.X.ItemName@
-- parameter. The client may specify new attributes using a combination of
-- the @Item.X.Attribute.Y.Name@ and @Item.X.Attribute.Y.Value@ parameters.
-- The client may specify the first attribute for the first item using the
-- parameters @Item.0.Attribute.0.Name@ and @Item.0.Attribute.0.Value@, and
-- for the second attribute for the first item by the parameters
-- @Item.0.Attribute.1.Name@ and @Item.0.Attribute.1.Value@, and so on.
--
-- Attributes are uniquely identified within an item by their name\/value
-- combination. For example, a single item can have the attributes
-- @{ \"first_name\", \"first_value\" }@ and
-- @{ \"first_name\", \"second_value\" }@. However, it cannot have two
-- attribute instances where both the @Item.X.Attribute.Y.Name@ and
-- @Item.X.Attribute.Y.Value@ are the same.
--
-- Optionally, the requester can supply the @Replace@ parameter for each
-- individual value. Setting this value to @true@ will cause the new
-- attribute values to replace the existing attribute values. For example,
-- if an item @I@ has the attributes @{ \'a\', \'1\' }, { \'b\', \'2\'}@
-- and @{ \'b\', \'3\' }@ and the requester does a BatchPutAttributes of
-- @{\'I\', \'b\', \'4\' }@ with the Replace parameter set to true, the
-- final attributes of the item will be @{ \'a\', \'1\' }@ and
-- @{ \'b\', \'4\' }@, replacing the previous values of the \'b\' attribute
-- with the new value.
--
-- This operation is vulnerable to exceeding the maximum URL size when
-- making a REST request using the HTTP GET method. This operation does not
-- support conditions using @Expected.X.Name@, @Expected.X.Value@, or
-- @Expected.X.Exists@.
--
-- You can execute multiple @BatchPutAttributes@ operations and other
-- operations in parallel. However, large numbers of concurrent
-- @BatchPutAttributes@ calls can result in Service Unavailable (503)
-- responses.
--
-- The following limitations are enforced for this operation:
--
-- -   256 attribute name-value pairs per item
-- -   1 MB request size
-- -   1 billion attributes per domain
-- -   10 GB of total user data storage per domain
-- -   25 item limit per @BatchPutAttributes@ operation
module Network.AWS.SDB.BatchPutAttributes
  ( -- * Creating a Request
    BatchPutAttributes (..),
    newBatchPutAttributes,

    -- * Request Lenses
    batchPutAttributes_domainName,
    batchPutAttributes_items,

    -- * Destructuring the Response
    BatchPutAttributesResponse (..),
    newBatchPutAttributesResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SDB.Types

-- | /See:/ 'newBatchPutAttributes' smart constructor.
data BatchPutAttributes = BatchPutAttributes'
  { -- | The name of the domain in which the attributes are being stored.
    domainName :: Prelude.Text,
    -- | A list of items on which to perform the operation.
    items :: [ReplaceableItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchPutAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'batchPutAttributes_domainName' - The name of the domain in which the attributes are being stored.
--
-- 'items', 'batchPutAttributes_items' - A list of items on which to perform the operation.
newBatchPutAttributes ::
  -- | 'domainName'
  Prelude.Text ->
  BatchPutAttributes
newBatchPutAttributes pDomainName_ =
  BatchPutAttributes'
    { domainName = pDomainName_,
      items = Prelude.mempty
    }

-- | The name of the domain in which the attributes are being stored.
batchPutAttributes_domainName :: Lens.Lens' BatchPutAttributes Prelude.Text
batchPutAttributes_domainName = Lens.lens (\BatchPutAttributes' {domainName} -> domainName) (\s@BatchPutAttributes' {} a -> s {domainName = a} :: BatchPutAttributes)

-- | A list of items on which to perform the operation.
batchPutAttributes_items :: Lens.Lens' BatchPutAttributes [ReplaceableItem]
batchPutAttributes_items = Lens.lens (\BatchPutAttributes' {items} -> items) (\s@BatchPutAttributes' {} a -> s {items = a} :: BatchPutAttributes) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest BatchPutAttributes where
  type
    Rs BatchPutAttributes =
      BatchPutAttributesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull BatchPutAttributesResponse'

instance Prelude.Hashable BatchPutAttributes

instance Prelude.NFData BatchPutAttributes

instance Prelude.ToHeaders BatchPutAttributes where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath BatchPutAttributes where
  toPath = Prelude.const "/"

instance Prelude.ToQuery BatchPutAttributes where
  toQuery BatchPutAttributes' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("BatchPutAttributes" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2009-04-15" :: Prelude.ByteString),
        "DomainName" Prelude.=: domainName,
        Prelude.toQueryList "Item" items
      ]

-- | /See:/ 'newBatchPutAttributesResponse' smart constructor.
data BatchPutAttributesResponse = BatchPutAttributesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchPutAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newBatchPutAttributesResponse ::
  BatchPutAttributesResponse
newBatchPutAttributesResponse =
  BatchPutAttributesResponse'

instance Prelude.NFData BatchPutAttributesResponse
