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
-- Module      : Network.AWS.SDB.BatchDeleteAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Performs multiple DeleteAttributes operations in a single call, which
-- reduces round trips and latencies. This enables Amazon SimpleDB to
-- optimize requests, which generally yields better throughput.
--
-- The following limitations are enforced for this operation:
--
-- -   1 MB request size
-- -   25 item limit per BatchDeleteAttributes operation
module Network.AWS.SDB.BatchDeleteAttributes
  ( -- * Creating a Request
    BatchDeleteAttributes (..),
    newBatchDeleteAttributes,

    -- * Request Lenses
    batchDeleteAttributes_domainName,
    batchDeleteAttributes_items,

    -- * Destructuring the Response
    BatchDeleteAttributesResponse (..),
    newBatchDeleteAttributesResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SDB.Types

-- | /See:/ 'newBatchDeleteAttributes' smart constructor.
data BatchDeleteAttributes = BatchDeleteAttributes'
  { -- | The name of the domain in which the attributes are being deleted.
    domainName :: Prelude.Text,
    -- | A list of items on which to perform the operation.
    items :: [DeletableItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'batchDeleteAttributes_domainName' - The name of the domain in which the attributes are being deleted.
--
-- 'items', 'batchDeleteAttributes_items' - A list of items on which to perform the operation.
newBatchDeleteAttributes ::
  -- | 'domainName'
  Prelude.Text ->
  BatchDeleteAttributes
newBatchDeleteAttributes pDomainName_ =
  BatchDeleteAttributes'
    { domainName = pDomainName_,
      items = Prelude.mempty
    }

-- | The name of the domain in which the attributes are being deleted.
batchDeleteAttributes_domainName :: Lens.Lens' BatchDeleteAttributes Prelude.Text
batchDeleteAttributes_domainName = Lens.lens (\BatchDeleteAttributes' {domainName} -> domainName) (\s@BatchDeleteAttributes' {} a -> s {domainName = a} :: BatchDeleteAttributes)

-- | A list of items on which to perform the operation.
batchDeleteAttributes_items :: Lens.Lens' BatchDeleteAttributes [DeletableItem]
batchDeleteAttributes_items = Lens.lens (\BatchDeleteAttributes' {items} -> items) (\s@BatchDeleteAttributes' {} a -> s {items = a} :: BatchDeleteAttributes) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest BatchDeleteAttributes where
  type
    Rs BatchDeleteAttributes =
      BatchDeleteAttributesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull BatchDeleteAttributesResponse'

instance Prelude.Hashable BatchDeleteAttributes

instance Prelude.NFData BatchDeleteAttributes

instance Prelude.ToHeaders BatchDeleteAttributes where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath BatchDeleteAttributes where
  toPath = Prelude.const "/"

instance Prelude.ToQuery BatchDeleteAttributes where
  toQuery BatchDeleteAttributes' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("BatchDeleteAttributes" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2009-04-15" :: Prelude.ByteString),
        "DomainName" Prelude.=: domainName,
        Prelude.toQueryList "Item" items
      ]

-- | /See:/ 'newBatchDeleteAttributesResponse' smart constructor.
data BatchDeleteAttributesResponse = BatchDeleteAttributesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newBatchDeleteAttributesResponse ::
  BatchDeleteAttributesResponse
newBatchDeleteAttributesResponse =
  BatchDeleteAttributesResponse'

instance Prelude.NFData BatchDeleteAttributesResponse
